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
ggplot(df, aes(x =  reorder(country.name, birth_weight, median), y = birth_weight)) +
  geom_boxplot(fill = '#e5e5e5') +
  geom_hline(yintercept=line, show.legend = TRUE) +
  coord_flip() +
  theme_minimal(base_size = 20) +
  theme(legend.position = "none", axis.title.y = element_blank()) #+
  #scale_x_continuous(limits=c(1000, 5000))

#sample size per country
temp <- df %>%
  group_by(country.code) %>%
  summarise(samples = length(country.code))
print(temp, n=80)
