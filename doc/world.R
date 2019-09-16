# run example run to get data
library(tidyverse)
library(maps)
df <- readRDS("/home/greggu/git/dhsdata/inst/default_data/df_plot.rds")
temp <- dhsdata:::get_recode()
df <- df %>% inner_join(temp, by ="country.code")
df <- df %>% filter(fuel3 != 2)
df$fuel1 <- as.numeric(df$fuel3 == 1)
#names <- df$region %>% unique
WorldData <- map_data('world') %>% filter(region != "Antartica") %>% fortify




temp <- df
temp$one <- 1
temp <- temp  %>% group_by(country.name, fuel1) %>%
  dplyr::summarise(count = sum(one))
temp <- temp %>%
  group_by(country.name) %>%
  mutate(count_base = sum(count))
temp <- temp %>%
  mutate(perc = round(count/count_base*100,1))
temp <- temp %>% filter(fuel1 == 1)
temp$perc <- temp$perc / 100
temp$perc <- temp$perc %>%
  #round(3) %>%
  as.factor() %>%
  dhsdata::binfactor(seq(0.001, .1, 0.001),
                     seq(.101, .2, 0.001),
                     seq(.201, .3, 0.001),
                     seq(.301, .4, 0.001),
                     seq(.401, .5, 0.001),
                     seq(.501, .6, 0.001),
                     seq(.601, .99, 0.001)
  ) %>%
  factor(labels = c("<10%","11-20%", "21-30%", "31-40%", "41-50%", "51-60%", "60-100%"))
# df <- df %>%
#   group_by(region) %>%
#   summarise(value = mean(birth_weight))
df <- temp
df <- df %>%
  rename(region = country.name)
df <- inner_join(df, WorldData)
df <- df[!is.na(df$perc),]

pdf("world.pdf", 13,4)
ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id=region),
           fill = "white", colour = "grey", size=0.25) +
  geom_map(data = df, map=WorldData,
           aes(fill=perc, map_id=region),
           colour="black", size=0.1) +
  coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) +
  #scale_fill_continuous(low="#f5f5f5", high="#d8b365", guide="colorbar") +
  #scale_fill_discrete() +
  scale_fill_brewer(palette="Blues") +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  labs(fill="percentage solid fuel", title="", x="", y="") +
  theme(panel.border = element_blank()) +
  theme_minimal(base_size = 14) +
  theme(legend.position="bottom") +
  theme(legend.position="left")
dev.off()

pdf("world2.pdf", 13,4)
ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id=region),
           fill = "white", colour = "grey", size=0.25) +
  geom_map(data = df, map=WorldData,
           aes(fill=perc, map_id=region),
           colour="black", size=0.1) +
  coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) +
  #scale_fill_continuous(low="#f5f5f5", high="#d8b365", guide="colorbar") +
  #scale_fill_discrete() +
  scale_fill_brewer(palette="Blues") +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  labs(fill="percentage solid fuel", title="", x="", y="") +
  theme(panel.border = element_blank()) +
  theme_minimal(base_size = 10) +
  theme(guide_colorbar())
  theme(legend.position="bottom")
dev.off()

