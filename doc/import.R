library(tidyverse)
B <- '01'
M <- '1'
# HV241                  Food cooked in the house/ separate building/ ou   89    1    N    I    1    0   No   No
# 1  In the house
# 2  In a separate building
# 3  Outdoors
# 6  Other
# We need raphael to request household variables
cnames0<-c("V000", "V002", #"V007", #"V008", "V010",
           "V012", "V106", "V113",
           "V161", "V190", "V437", "V438", "V481", "HW1$1")
# THESE VARIABLES HOLD 20 MOST RECENT BIRTHS
cnames1 <- paste0(c("BORD$","B0$","B4$", "B5$"), B) #"B3$"
# THESE VARIABLES HOLD 6 MOST RECENT BIRTHS
cnames2 <- paste0(c("M19$", "M19A$") ,M)
cnames <- c(cnames0,cnames1,cnames2)
cnames_l <- cnames %>% tolower()
cnames <- c(cnames, cnames_l)
filepaths <- dhsdata::get_file_paths(workdir=getwd(), data_folder="data", filetype=".csv") #gets path too microdata survey files

# for (path in filepaths) {
#   df <- read.csv(path)
#   df <- df[,names(df) %in% cnames]
#   saveRDS(df, paste0("data1/",df$V000[1], ".rds"))
#   saveRDS(df, paste0("data1/",df$v000[1], ".rds"))
# }
temp <- data.table::fread(file=filepaths[3], select = cnames)
comb_data(filepaths, cnames)
dhsdata::get_file_paths(workdir=getwd(), data_folder="data1", filetype=".rds") %>% length()

filepaths <- dhsdata::get_file_paths(workdir=getwd(), data_folder="data1", filetype=".rds") #gets path too
# df <- bind_files(filepaths)
# saveRDS(df, "data_0519.rds")
#dhsdata::CreateSingleFile(filepaths, "data2", "data0717", "rds")

rbind_data(filepaths, "data2", "data0821", "rds")
