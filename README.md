dhsdata
================

## Introduction

dhsdata is a simple data package to create a single data file with
specific variables from numerous DHS surveys. It only takes 3 steps
outlined below.

1.  [Select variables](#1)
2.  [Get the filenames of all surverys](#2) `get_file_paths`
3.  [Make a single data file with select variabels](#3)
    `pluck_and_combine_dhs_surveys`

## 1\. Select variables

## <a name="1"></a>

Create a vector with all variable names you wish to import (NOTE: in
uppercase). You can find these variable codes in DHS recode manuals.

``` r
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
```

Make a lowercase version and concatenate with the original uppercase
vector. DHS is not consistent with cases so this ensures we get the
variable regardless of how it is in the survey. They will all be set to
lower after they are read in with my function.

``` r
cnames_l <- cnames %>% tolower()
cnames <- c(cnames, cnames_l)
```

## 2\. Get the filenames of all surverys

## <a name="2"></a>

Write the full path to the folder that holds are your DHS surveys.
You’ll need to change my path here. You can find these files on box in
this private folder <https://umass.app.box.com/folder/56680977451>

``` r
path <- "D:/storage/fail_safe/tempdhs" 
filepaths <- get_file_paths(path, filetype=".csv") #gets path too microdata survey files
```

## 3\. Make a single data file with select variabels

## <a name="3"></a>

The resulting file is automatically saved to the folder named “output”
which can be found inside your dhsdata folder.

``` r
pluck_and_combine_dhs_surveys(filepaths, cnames)
```
