# run example run to get data
library(tidyverse)
library(maps)
df <- dhsdata:::get_df3()
temp <- dhsdata:::get_recode()
df <- df %>% left_join(temp, by ="country.code") # fix duplicate rows in our area files earlier

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
df <- temp

df <- df %>%
  rename(region = country.name)
df <- df %>%
  group_by(region) %>%
  summarise(value = mean(birth_weight))
df <- inner_join(df, WorldData)
pdf("world.pdf", 13,4)
ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id=region),
           fill = "white", colour = "grey", size=0.25) +
  geom_map(data = df, map=WorldData,
           aes(fill=perc, map_id=region),
           colour="black", size=0.25) +
  coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) +
  scale_fill_continuous(low="thistle2", high="darkred", guide="colorbar") +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  labs(fill="percentage solid fuel", title="", x="", y="") +
  theme(panel.border = element_blank()) +
  theme_minimal()
dev.off()

pdf("world2.pdf", 13,4)
ggplot() +
  geom_map(data = df, map = WorldData,
           aes(x = long, y = lat, group = group, map_id=region),
           fill = "white", colour = "grey", size=0.25) +
  geom_map(data = df, map=WorldData,
           aes(fill=perc, map_id=region),
           colour="black", size=0.25) +
  coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) +
  scale_fill_continuous(low="thistle2", high="darkred", guide="colorbar") +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  labs(fill="percentage solid fuel", title="", x="", y="") +
  theme(panel.border = element_blank()) +
  theme_minimal()
dev.off()


df$one <- 1
pdf("world_null.pdf", 13,4)
ggplot() +
  geom_map(data = df, map = WorldData,
           aes(x = long, y = lat, group = group, map_id=region),
           fill = "white", colour = "grey", size=0.25) +
  geom_map(data = df, map=WorldData, fill = "#4e73ad",
           aes(map_id=region),
           colour="black", size=0.25) +
  coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  labs(fill="percentage solid fuel", title="", x="", y="") +
  theme(panel.border = element_blank(), legend.position = "none") +
  theme_minimal()
dev.off()


