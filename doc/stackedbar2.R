# run example run to get data
library(tidyverse) # some sort of plyr / dplyr conflict later in script
library(RColorBrewer)
#df <- dhsdata:::get_df0()
df <- readRDS("inst/default_data/df.rds")
temp <- dhsdata:::get_recode()
df <- df %>% inner_join(temp, by ="country.code")


df$one <- 1
temp <- df  %>% group_by(country.name, fuel3) %>%
  dplyr::summarise(count = sum(one))
temp <- temp %>%
  group_by(country.name) %>%
  mutate(count_base = sum(count))
temp <- temp %>%
  mutate(perc = round(count/count_base*100,1))
library(plyr)
#temp$fuel3 <- temp$fuel3 %>% as.factor()
temp <- plyr::ddply(temp, .(country.name),
                    transform, pos = cumsum(perc) - (0.5 * perc))
p1 <- ggplot(data = temp, aes(x = country.name, y = perc, fill = fuel3)) +
  geom_bar(stat="identity",position = "stack")
p1 + geom_text(data=temp, aes(x = country.name, y = pos,
                              label = paste0(perc,"%")), size=4) +
  ylab("percentage %") +
  xlab("low birth weight") +
  scale_x_continuous(breaks=seq(0,1,1)) +
  theme(legend.position = "bottom")


temp2 <- temp
temp2$fuel3 <- temp2$fuel3 %>% as.factor()
levels(temp2$fuel3) <- c("clean", "solid", "other", "kerosene")
temp2$fuel3 <- temp2$fuel3 %>% forcats::fct_rev()
temp2 <- plyr::ddply(temp2, .(country.name),
                     transform, pos = cumsum(perc) - (0.5 * perc))
detach("package:plyr", unload = TRUE)
library(tidyverse)
# temp3 <- temp2 %>% select(country.name, fuel3, perc) %>% spread(fuel3, perc)
# ord <- temp3$clean %>% order(decreasing = TRUE)
# temp3 <- temp3[ord,]
# temp3 <- temp3 %>% gather(fuel, perc, -country.name)
## This method does not work, order is not retained upon gathering

temp3 <- temp2 %>% select(country.name, fuel3, perc) %>% spread(fuel3, perc)
ord <- temp3$clean %>% order(decreasing = TRUE)
name_order <- temp3$country.name[ord]
#use this ordered factor method and apply it prior to summarise function to get results as desired
sizes <- ordered(c("small", "large", "large", "small", "medium"))
sizes <- ordered(sizes, levels = c("small", "medium", "large"))
sizes
#> [1] small  large  large  small  medium
#> Levels: small < medium < large


p1 <- ggplot(data = temp2, aes(x = country.name, y = perc, fill = fuel3)) +
  geom_bar(stat="identity",position = "stack") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  +
  geom_text(data=temp2, aes(x = country.name, y = pos, label = paste0(perc,"%")), size=4) +
  ylab("percentage %") +
  xlab("low birth weight") +
  theme(legend.position = "bottom") + scale_fill_brewer(palette="BrBG")


#pdf("stack.pdf")
p1
#dev.off()
