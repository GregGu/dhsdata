library(tidyverse)
#df <- readRDS("/home/greggu/git/dhsdata/data2/data0519.rds")
#df <- readRDS("/home/greggu/git/dhsdata/data_external/birth_weight_full0.rda")
# df <- readRDS("/home/greggu/git/dhsdata/data2/data0824.rds")
df <- readRDS("output/plucked_dhs.rds")
colnames(df) <- colnames(df) %>% toupper()
df <- df %>% rename(weight = V437,
                    height = V438,
                    scode = V000,
                    house = V002,
                    #year = V007,
                    #year = V008,
                    #year_birth = V010,
                    child_age = `HW1$1`,
                    #year_birth = `B3$01`,
                    age = V012,
                    education = V106,
                    water = V113,
                    fuel = V161,
                    wealth = V190,
                    insurance = V481,
                    first_born= `BORD$01`,
                    twin = `B0$01`,
                    live = `B5$01`,
                    sex = `B4$01`,
                    document = `M19A$1`,
                    birth_weight = `M19$1`
)
scode <- df$scode
#df <- mutate_all(df, as.numeric)
df$scode <- scode
# investigation shows no single survey has different units of measure
# instead there are simply errors within surveys, outliers with unrealistic weights and heights
# weight > 200kg | height > 2.15m will be assumed erroneous, they fall past the 99th percentile of respective distributions
# V437 weight in kg /1000
# V438 height in cm divide by 100
# kg/m^2 = BMI
df$weight <-df$weight / 10 # now in kilograms
df$height <-df$height / 1000 # now in meters
df$weight[df$weight > 200] <- NA
df$height[df$height > 2.15] <- NA

# is.na(df$weight) <- which(df$weight %in% c(200:9999))
# is.na(df$height) <- which(df$height %in% c(2.15:9999))
df$bmi <- df$weight/((df$height)^2)
df$bmi[df$bmi > 80] <- NA
# CMC format
# df$year <- 1900 + as.integer((df$year-1)/12)
# df$year[df$year == 2062] <-2006
# df$year[df$year == 2063] <-2006
# df$year[df$year == 2067] <-2011
# df$year[df$year == 2068] <-2011
# df$year[df$year == 2073] <-2016
# is.na(df$year) <- which(df$year < 1970)
# prior investigation in order to hardcode year below
# unique(df$survey_code_v000[df$year > 2016])
# unique(df$survey_code_v000[df$year == 2062])
# unique(df$survey_code_v000[df$year == 2063])
# unique(df$survey_code_v000[df$year == 2067])
# unique(df$survey_code_v000[df$year == 2068])
# unique(df$survey_code_v000[df$year == 2073])
# "NP5" "NP6" "NP7" have incorrect dates (correct dates 2006, 2011,, 2016)
# I manually checked the dhs website my searching Nepal under
# What We Do //// Survey search
# Once I found Nepal I checked which year the phase was conducted in
# CMC format
# too many eroneous births ...
# df$year_birth <- 1900 + as.integer((df$year_birth-1)/12)
# df$child_age <- df$year - df$year_birth
# df <- df[df$child_age > 0,]
df$child_age[df$child_age == 99] <- NA
tempchildage <- df$child_age[!is.na(df$child_age)]
df$child_age[is.na(df$child_age)] <- median(tempchildage)
df$maternal_age <- df$age - round(df$child_age/12)
#hist(df$maternal_age, breaks= 100)

# df$education <- df$education %>%  dhsdata:::binfactor(c(0),
#                             c(1:3),
#                             c(4:9))
is.na(df$education) <- which(df$education > 3)
# Drinking Water, UNICEF
# Improved
# 10 PIPED WATER
# 11 Piped into dwelling
# 12 Piped to yard/plot
# Other/improved
# 13 Public tap/standpipe
# 20 TUBE WELL WATER
# 21 Tube well or borehole
# 30 DUG WELL (OPEN/PROTECTED)
# 31 Protected well
# 51 Rainwater
# Unimproved
# 32 Unprotected well
# 40 SURFACE WATER
# 41 Protected spring
# 42 Unprotected spring
# 43 River/dam/lake/ponds/stream/canal/irrigation channel
# 61 Tanker truck
# 62 Cart with small tank
# 71 Bottled water
# 96 Other
# 97 Not a dejure resident
# (m) 99 Missing
# previous code
df$water <- df$water %>% dhsdata:::binfactor(c(c(10:31),51), #improved/other impr
                                   c(32:96), #unimproved
                                   c(c(97:99),0:9)) #missing
is.na(df$water) <- which(df$water == 2)
# Cooking fuel
# exclude people who cook outside of the house
# 95 No food cooked in house
# 97 Not a dejure resident
# Aggregate 6-10 are solid fuels
# 1 Electricity
# 2 LPG
# 3 Natural gas
# 4 Biogas
# 5 kerosene
# 6 Coal, lignite
# 7 Charcoal
# 8 Wood
# 9 Straw/shrubs/grass
# 10 Agricultural crop
# 11 Animal dung
# possibly remove code 5 kerosene for hard fuel analysis
# consider adding kerosene as a secondary analysis
df$fuel1 <- df$fuel %>% dhsdata:::binfactor(c(1:4),
                                  c(6:11),
                                  c(5,91:99,12:90))
is.na(df$fuel1) <- which(df$fuel1 == 2)
#solid + other (-kerosene)
df$fuel2 <- df$fuel %>% dhsdata:::binfactor(c(1:4),
                                  c(6:11,12:90),
                                  c(5,91:99))
is.na(df$fuel1) <- which(df$fuel1 == 2)


df$fuel3 <- df$fuel %>% dhsdata:::binfactor(c(1:4), #clean
                                            c(6:11), #solid
                                            c(12:90), #other
                                            c(5), #kerosene
                                            c(91:99)) #na
is.na(df$fuel3) <- which(df$fuel3 == 4)
df$fuel4 <- df$fuel3
df$fuel4[df$fuel4 == 2] <- NA
df$fuel4[df$fuel4 == 1] <- NA
df$fuelk <- as.numeric(df$fuel4 == 3)

# V190: Wealth index
# 1  Poorest
# 2  Poorer
# 3  Middle
# 4  Richer
# 5  Richest
# df$wealth <- df$wealth %>% dhsdata:::binfactor(
#   c(1:2),
#   c(3),
#   c(4:5))
#insurance
df$insurance <- df$insurance %>% dhsdata:::binfactor(0,
                                           1,
                                           2:9)
#firstborn
df$first_born <- df$first_born %>% dhsdata:::binfactor(1, 2:20)
is.na(df$first_born) <- which(df$first_born == 2)
df$document <- df$document %>% dhsdata:::binfactor(c(0,2),
                                         c(1),
                                         c(3:9))
is.na(df$document) <- which(df$document == 2)
df$birth_weight_f <- df$birth_weight %>% dhsdata:::binfactor(0:2500,
                                                   2501:8000,
                                                   8001:9999)
is.na(df$birth_weight_f) <- which(df$birth_weight_f == 2)
is.na(df$birth_weight) <- which(df$birth_weight > 8001)



#1 is male
df$sex <- as.numeric(df$sex == 1)

df$scode <- dhsdata:::remove_last_digit(df$scode)
recode <- dhsdata:::get_recode()
df <- inner_join(df, recode, by=c("scode" = "dhsalpha2"))


# subset/filter data down to our study population
# df <- df[df$live == 1,]
# df <- df[df$twin == 0,]
# df <- df[df$document == 1,]
df_undoc <- df %>% filter(live == 1 & twin == 0)# & document == 1)
df <- df_doc <- df %>% filter(live == 1 & twin == 0 & document == 1)







df_plot <- df_undoc %>% select(country.code,
                     fuel3,
                     birth_weight_f,
                     birth_weight) %>% drop_na

df1 <- df %>% select(country.code,
       bmi,
       maternal_age,
       education,
       fuel1,
       wealth,
       insurance,
       sex,
       birth_weight_f,
       birth_weight) %>% drop_na()


df3 <- df %>% select(country.code,
       maternal_age,
       education,
       fuel1,
       wealth,
       birth_weight_f,
       birth_weight) %>% drop_na

saveRDS(df_plot, "inst/default_data/df_plot.rds")
saveRDS(df1, "inst/default_data/df1.rds")
saveRDS(df3, "inst/default_data/df3.rds")


impute <- function(maindata, refdata){
  maindata$wealth[is.na(maindata$wealth)] <- refdata$wealth %>% median
  maindata$education[is.na(maindata$education)] <- refdata$education %>% median
  maindata$maternal_age[is.na(maindata$maternal_age)] <- refdata$maternal_age %>% median
  maindata$bmi[is.na(maindata$bmi)] <- refdata$bmi %>% median
  maindata$insurance[is.na(maindata$insurance)] <- refdata$insurance %>% median
  return(maindata)
}

df_doc_impute <- impute(df_doc, df1)
df_undoc_impute <- impute(df_undoc, df1)



df_undoc3k <- df_undoc %>% select(country.code,
                                   bmi,
                                   maternal_age,
                                   education,
                                   fuelk,
                                   wealth,
                                   birth_weight_f,
                                   birth_weight) %>% drop_na()

df_undoc_impute3k <- df_undoc_impute %>% select(country.code,
                                   bmi,
                                   maternal_age,
                                   education,
                                   fuelk,
                                   wealth,
                                   birth_weight_f,
                                   birth_weight) %>% drop_na()

df_undoc1 <- df_undoc %>% select(country.code,
                                 bmi,
                                 maternal_age,
                                 education,
                                 fuel1,
                                 wealth,
                                 insurance,
                                 sex,
                                 birth_weight_f,
                                 birth_weight) %>% drop_na()

df_undoc3 <- df_undoc %>% select(country.code,
                     maternal_age,
                     education,
                     fuel1,
                     wealth,
                     birth_weight_f,
                     birth_weight) %>% drop_na()


df_undoc_impute1 <- df_undoc_impute %>% select(country.code,
                                               bmi,
                                               maternal_age,
                                               education,
                                               fuel1,
                                               wealth,
                                               insurance,
                                               sex,
                                               birth_weight_f,
                                               birth_weight) %>% drop_na()
df_undoc_impute3 <- df_undoc_impute %>% select(country.code,
                                maternal_age,
                                education,
                                fuel1,
                                wealth,
                                birth_weight_f,
                                birth_weight) %>% drop_na()


#saveRDS(df_undoc1k, "inst/default_data/df_undoc1k.rds")
saveRDS(df_undoc3k, "inst/default_data/df_undoc3k.rds")
saveRDS(df_undoc1, "inst/default_data/df_undoc1.rds")
saveRDS(df_undoc3, "inst/default_data/df_undoc3.rds")
saveRDS(df_undoc_impute1, "inst/default_data/df_undoc_impute1.rds")
saveRDS(df_undoc_impute3, "inst/default_data/df_undoc_impute3.rds")
saveRDS(df_undoc_impute3k, "inst/default_data/df_undoc_impute3k.rds")


