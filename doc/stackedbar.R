# run example run to get data
library(tidyverse)
library(RColorBrewer)
df <- dhsdata:::get_df0()
temp <- dhsdata:::get_recode()
df <- df %>% inner_join(temp, by ="country.code")


df$birth_weight_f2 <- as.numeric(df$birth_weight_f == 0)
df$one <- 1
temp <- df  %>% group_by(birth_weight_f2, fuel3) %>%
  dplyr::summarise(count = sum(one))
temp <- temp %>%
  group_by(birth_weight_f2) %>%
  mutate(count_base = sum(count))
temp <- temp %>%
  mutate(perc = round(count/count_base*100,1))
library(plyr)
#temp$fuel3 <- temp$fuel3 %>% as.factor()
temp <- plyr::ddply(temp, .(birth_weight_f2),
      transform, pos = cumsum(perc) - (0.5 * perc))
p1 <- ggplot(data = temp, aes(x = birth_weight_f2, y = perc, fill = fuel3)) +
  geom_bar(stat="identity",position = "stack")
p1 + geom_text(data=temp, aes(x = birth_weight_f2, y = pos,
                                     label = paste0(perc,"%")), size=4) +
  ylab("percentage %") +
  xlab("low birth weight") +
  scale_x_continuous(breaks=seq(0,1,1)) +
  theme(legend.position = "bottom")


temp2 <- temp
temp2$fuel3 <- temp2$fuel3 %>% as.factor()
levels(temp2$fuel3) <- c("clean", "solid", "other", "kerosene")
temp2$fuel3 <- temp2$fuel3 %>% forcats::fct_rev()
temp2 <- plyr::ddply(temp2, .(birth_weight_f2),
                    transform, pos = cumsum(perc) - (0.5 * perc))
p1 <- ggplot(data = temp2, aes(x = birth_weight_f2, y = perc, fill = fuel3)) +
  geom_bar(stat="identity",position = "stack")
p1 <- p1 + geom_text(data=temp2, aes(x = birth_weight_f2, y = pos,
                              label = paste0(perc,"%")), size=4) +
  ylab("percentage %") +
  xlab("low birth weight") +
  scale_x_continuous(breaks=seq(0,1,1)) +
  theme(legend.position = "bottom") + scale_fill_brewer(palette="BrBG")

pdf("stack.pdf")
p1
dev.off()
