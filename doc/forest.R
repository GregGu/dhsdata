# run example run to get data
library(tidyverse)
df <- dhsdata:::get_df3()
temp <- dhsdata:::get_recode()
df <- df %>% inner_join(temp, by ="country.code")
# #Order(by median birth weight by country)/sort/boxplot
bymedian <- with(df, reorder(country.code, -birth_weight, median))
line <- median(df$birth_weight)
boxplot(df$birth_weight~bymedian, horizontal = TRUE)
abline(v = line, lwd = 2)

library(ggplot2)
line <- 2500
pdf("forest.pdf", 11,11)
ggplot(df, aes(x =  reorder(country.name, birth_weight, median), y = birth_weight)) +
  geom_boxplot(fill = '#e5e5e5', outlier.shape = NA) +
  geom_hline(yintercept=line, show.legend = TRUE, linetype = 2) +
  coord_flip() +
  theme_bw(base_size = 15) +
  theme(panel.border = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none", axis.title.y = element_blank()) +
  scale_y_continuous(breaks = seq(0,6000,500), limits = c(1000,7000)) +
  ylab("birth weight")
dev.off()
#sample size per country
temp <- df %>%
  group_by(country.code) %>%
  summarise(samples = length(country.code))
print(temp, n=80)
