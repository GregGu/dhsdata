library(tidyverse)

# area data
dhs_alpha2 <- read.csv("/home/greggu/git/dhsdata/data_external/dhs_alpha2.csv")
iso_alpha2 <- read.csv("/home/greggu/git/dhsdata/data_external/iso_alpha2.csv")
iso_numeric <- read.csv("/home/greggu/git/dhsdata/data_external/iso_numeric.csv")
recode_reg <- read.csv("/home/greggu/git/dhsdata/data_external/region.csv")

recode_reg <- recode_reg[,c("ISO.Code","Major.area", "Region")]
names(recode_reg)<-c("country.code", "major_area",'sub_region')
#check dhs_alpha2 country names match iso country names ...
#we will be merging all area files by country.names (counter intuitively these match more often than codes)
dhs_alpha2$country.name[!dhs_alpha2$country.name %in% iso_alpha2$country.name]
if (sum(!dhs_alpha2$country.name %in% iso_alpha2$country.name) > 0){
  stop("country.name do not match between data files")
}

recode_country <- merge(dhs_alpha2, iso_alpha2, by="country.name")
recode_country <- merge(recode_country, iso_numeric, by="country.name")
recode_country <- rename(recode_country, country.code = iso.numeric)
recode <- merge(recode_country, recode_reg, by="country.code")
recode <- recode[recode$notes!=" (Ondo State)",] #duplicate nigeria
saveRDS(recode, "/home/greggu/git/dhsdata/inst/default_data/recode.rds")
