library(tidyverse)
B <- '01'
M <- '1'
cnames0<-c("V000", "V002", "V008",
           "V012", "V106", "V113",
           "V161", "V190", "V437", "V438", "V481")
# THESE VARIABLES HOLD 20 MOST RECENT BIRTHS
cnames1 <- paste0(c("BORD$","B0$", "B3$","B4$", "B5$"), B)
# THESE VARIABLES HOLD 6 MOST RECENT BIRTHS
cnames2 <- paste0(c("M19$", "M19A$") ,M)
cnames <- c(cnames0,cnames1,cnames2)
cnames_l <- cnames %>% tolower()
cnames <- c(cnames, cnames_l)
filepaths <- get_file_paths(workdir=getwd(), data_folder="data", filetype=".csv") #gets path too
for (path in filepaths) {
  df <- read_csv(path)
  df <- df[,names(df) %in% cnames]
  saveRDS(df, paste0("data1/",df$V000[1], ".rds"))
  saveRDS(df, paste0("data1/",df$v000[1], ".rds"))
}

filepaths <- get_file_paths(workdir=getwd(), data_folder="data1", filetype=".rds") #gets path too
# df <- bind_files(filepaths)
# saveRDS(df, "data_0519.rds")
CreateSingleFile(filepaths, "data2", "data0519", "rds")
