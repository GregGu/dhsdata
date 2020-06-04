library(dplyr)
devtools::load_all() # this is how you load functions from a package in R without installing it

# This
B <- '01'
M <- '1'
cnames0<-c("V000", "V001","V002","V004", #"V007", #"V008", "V010",
           "V012", "V106", "V113",
           "V161", "V190", "V437", "V438", "V481", "HW1$1")
# THESE VARIABLES HOLD 20 MOST RECENT BIRTHS
cnames1 <- paste0(c("BORD$","B0$","B4$", "B5$"), B) #"B3$"
# THESE VARIABLES HOLD 6 MOST RECENT BIRTHS
cnames2 <- paste0(c("M19$", "M19A$") ,M)
cnames <- c(cnames0,cnames1,cnames2)
cnames_l <- cnames %>% tolower()
cnames <- c(cnames, cnames_l)



path <- "D:/storage/fail_safe/tempdhs"
filepaths <- get_file_paths(path, filetype=".csv") #gets path too microdata survey files
pluck_and_combine_dhs_surveys (filepaths, cnames)

