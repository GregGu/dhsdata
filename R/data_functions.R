# This function assumes NA's have not been recoded and the last range includes values which will be set to NA

assign_null <- function(...) {
  assign(..., NULL, env = parent.env(environment()))
}
binfactor <- function(x, ...) {
  x <- as.factor(x)
  nargs <- nargs() - 1
  if(nargs > 6) stop("sorry I got tired and limited this function to 6 groups")
  args <- list(...)
  for(i in 1:nargs){
    assign(paste0("group", i), args[[i]])
  }
  if( nargs<6){
  for(i in (nargs+1):6){
    assign(paste0("group", i), NULL)
  }
  }
  levels(x) <- list(one = group1,
                    two = group2,
                    three = group3,
                    four = group4,
                    five = group5,
                    six = group6)
  return(as.numeric(x)-1)
}

# merge with codes
remove_last_digit <- function(x){
  #x <- gsub('[[:digit:]]+', '', x)
  x <- x %>% substr(start = 1, stop = 2)
  return(x)
}
# test <-c("cat", "dog", "mouse", "cat", "cat", "mouse") %>% as.factor
# test %>% binfactor(c("cat", "dog"), "mouse")


