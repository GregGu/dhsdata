# This function assumes NA's have not been recoded and the last range includes values which will be set to NA

# test for change in order!!!
# make robust so error is thrown when not all levels are specified!
library(tidyverse)
df <- readRDS("/home/greggu/git/dhsdata/inst/core_data.rda")
x <- df$wealth_index_3_v190 %>% as.factor

assign_null <- function(...) {
  assign(..., NULL, env = parent.env(environment()))
}
fc <- function(x, ...) {
  paste0("group", 1:6) %>% map(assign_null)
  nargs <- nargs() - 1
  if(nargs > 6) stop("sorry I got tired and limited this function to 6 groups")
  args <- list(...)
  for(i in 1:nargs){
    assign(paste0("group", i), args[[i]])
  }
  levels(x) <- list(one = group1, 
                    two = group2,
                    three = group3,
                    four = group4,
                    five = group5,
                    six = group6)
  return(as.numeric(x)-1)
}
fc(x, c("0","1"))

test <-c("cat", "dog", "mouse", "cat", "cat", "mouse") %>% as.factor

test %>% fc(c("cat", "dog"), "mouse")

