# run example run to get data
library(tidyverse) # some sort of plyr / dplyr conflict later in script
library(RColorBrewer)
#df <- dhsdata:::get_df0()
df <- readRDS("/home/greggu/git/dhsdata/inst/default_data/df_plot.rds")
temp <- dhsdata:::get_recode()
df <- df %>% inner_join(temp, by ="country.code")
df <- df %>% filter(fuel3 != 2)

df$one <- 1

temp <- df
temp$fuel3 <- temp$fuel3 %>% as.factor()
levels(temp$fuel3) <- factor(c("clean", "solid", "kerosene"))
temp$one <- 1
temp <- temp  %>% group_by(country.name, fuel3) %>%
  dplyr::summarise(count = sum(one))
temp <- temp %>%
  group_by(country.name) %>%
  mutate(count_base = sum(count))
temp <- temp %>%
  mutate(perc = round(count/count_base*100,1))


temp3 <- temp %>% select(country.name, fuel3, perc) %>% spread(fuel3, perc)
ord <- temp3$clean %>% order(decreasing = TRUE)
name_order <- temp3$country.name[ord]
#use this ordered factor method and apply it prior to summarise function to get results as desired
# sizes <- ordered(c("small", "large", "large", "small", "medium"))
# sizes <- ordered(sizes, levels = c("small", "medium", "large"))
# sizes
#> [1] small  large  large  small  medium
#> Levels: small < medium < large
temp <- df
temp$fuel3 <- temp$fuel3 %>% as.factor()
levels(temp$fuel3) <- factor(c("clean", "solid", "kerosene"))
temp$one <- 1
temp$country.name <- temp$country.name %>% ordered()
temp$country.name <- temp$country.name %>% ordered(name_order)
temp <- temp  %>% group_by(country.name, fuel3) %>%
  dplyr::summarise(count = sum(one))
temp <- temp %>%
  group_by(country.name) %>%
  mutate(count_base = sum(count))
temp <- temp %>%
  mutate(perc = round(count/count_base*100,1))

#
# library(plyr)
# #temp$fuel3 <- temp$fuel3 %>% as.factor()
# temp <- plyr::ddply(temp, .(country.name),
#                     transform, pos = cumsum(perc) - (0.5* perc))
# p1 <- ggplot(data = temp, aes(x = country.name, y = perc, fill = fuel3)) +
#   geom_bar(stat="identity",position = "stack")
# p1 + geom_text(data=temp, aes(x = country.name, y = pos,
#                               label = paste0(perc,"%")), size=4) +
#   ylab("percentage %") +
#   xlab("low birth weight") +
#   #scale_x_continuous(breaks=seq(0,1,1)) +
#   theme(legend.position = "bottom")
#

library(plyr)
temp2 <- temp
temp2$fuel3 <- temp2$fuel3 %>% as.factor()
levels(temp2$fuel3) <- c("clean", "solid", "kerosene")
temp2$fuel3 <- temp2$fuel3 %>% forcats::fct_rev()
temp2 <- plyr::ddply(temp2, .(country.name),
                     transform, pos = cumsum(perc) + 1)#- (.5 * perc))
detach("package:plyr", unload = TRUE)
library(tidyverse)
temp2 <- temp2 %>% rename(fuel = fuel3)
temp2$pos[temp2$fuel == "kerosene"] <- NA
# temp3 <- temp2 %>% select(country.name, fuel3, perc) %>% spread(fuel3, perc)
# ord <- temp3$clean %>% order(decreasing = TRUE)
# temp3 <- temp3[ord,]
# temp3 <- temp3 %>% gather(fuel, perc, -country.name)
## This method does not work, order is not retained upon gathering


cols <- c("solid" = "#d8b365", "clean" = "#f5f5f5", "kerosene" = "#5ab4ac")
p1 <- ggplot(data = temp2, aes(x = country.name, y = perc, fill = fuel)) +
  geom_bar(stat="identity",position = "stack") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  +
  geom_text(data=temp2, aes(x = country.name, y = pos, label = paste0(perc,"%")), size=2.15) +
  ylab("percentage %") +
  xlab("low birth weight") +
  theme(legend.position = "bottom") + #scale_fill_brewer(palette="BrBG") +
  scale_fill_manual(values = cols) +
  xlab(NULL) + scale_y_continuous(breaks=seq(0,100,10)) +
  geom_hline(yintercept = seq(0,100,20),linetype = "dashed", size=.1)+
  theme(panel.background = element_blank())
p1

pdf("stack.pdf", 13, 9)
p1
dev.off()
