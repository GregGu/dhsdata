# run example run to get data
library(tidyverse)
library(maps)
df <- dhsdata:::get_df3()
temp <- dhsdata:::get_recode()
df <- df %>% left_join(temp, by ="country.code") # fix duplicate rows in our area files earlier


WorldData <- map_data('world') %>% filter(region != "Antarctica") %>% fortify


df <- df %>%
  rename(region = country.name)
df <- df %>%
  group_by(region) %>%
  summarise(value = mean(birth_weight))
pdf("world.pdf")
ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id=region),
           fill = "white", colour = "#7f7f7f", size=0.5) +
  geom_map(data = df, map=WorldData,
           aes(fill=value, map_id=region),
           colour="#7f7f7f", size=0.5) +
  coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) +
  scale_fill_continuous(low="thistle2", high="darkred", guide="colorbar") +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  labs(fill="legend", title="Title", x="", y="") +
  theme_bw()
dev.off()
