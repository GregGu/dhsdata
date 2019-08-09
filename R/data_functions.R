# This function assumes NA's have not been recoded and the last range includes values which will be set to NA

assign_null <- function(...) {
  assign(..., NULL, env = parent.env(environment()))
}
binfactor <- function(x, ...) {
  x <- as.factor(x)
  nargs <- nargs() - 1
  if(nargs > 8) stop("sorry I got tired and limited this function to 8 groups")
  args <- list(...)
  for(i in 1:nargs){
    assign(paste0("group", i), args[[i]])
  }
  if( nargs<8){
  for(i in (nargs+1):8){
    assign(paste0("group", i), NULL)
  }
  }
  levels(x) <- list(one = group1,
                    two = group2,
                    three = group3,
                    four = group4,
                    five = group5,
                    six = group6,
                    seven = group7,
                    eight = group8)
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


get_file_paths <- function(workdir, data_folder, filetype) {
  # two options to get file names
  filenames <- list.files(paste0(workdir, "/", data_folder), pattern=paste0("*", filetype))
  filenames <- paste0(data_folder,"/",filenames) #paste directory onto these names for importation
  return(filenames)
}


bind_files <- function(filepaths) {
  for (i in 1:length(filepaths)) {
    path <- filepaths[i]
    dat <- tryCatch(readRDS(file=path), error=function(e) e) # data.table::fread to select specific variables
    if(inherits(dat, "error")){ # check to see if object 'name' has class error
      print(paste0("something went wrong at index ",i))
      next
    }
    colnames(dat) <- dat %>% colnames() %>% toupper()
    assign(paste0("data", i), dat)
    data_names <- ls(pattern = 'data[0-300]+')
  }
  df <- do.call(dplyr::bind_rows, mget(data_names))
  return(df)
}


CreateSingleFile <- function(filenames, data_folder, output_name, file_type) {
  for (i in 1:length(filenames)) {
    name <- paste0("data", i)
    dat <- tryCatch(readRDS(file=filenames[i]), error=function(e) e) # data.table::fread to select specific variables
    if(inherits(dat, "error")){ # check to see if object 'name' has class error
      print(paste0("something went wrong at index ",i))
      next
    }
    colnames(dat) <- dat %>% colnames() %>% toupper()
    assign(name, dat)#, envir = .GlobalEnv)
  }
  data_names <- ls(pattern = 'data[0-600]+') # list all files in environment (since this is called inside a function it is looking in the functions environment)
  df <- do.call(bind_rows, mget(data_names)) # mget gets the named object/s
  # df <- df[complete.cases(df),]
  # rm(list=paste(data_names, sep="")) # was used to clean GlobalEnv before updating to use function env which is temporary
  if (file_type=="csv") {
    write.csv(object = df, file = paste0(data_folder,"/", output_name, ".csv"))
  }
  if (file_type=="rds") {
    saveRDS(object = df, file = paste0(data_folder,"/", output_name, ".rds"))
  }
} #end function



#-----------------------
Gett.i <- function(years.i, year.t ){
  gett.i <- rep(NA, length(years.i))
  for (i in 1:length(years.i)){
    gett.i[i] <- which(year.t == years.i[i])
  }
  return(gett.i)
}


#-----------------------
Getc.i <- function(iso.i, iso.c){
  getc.i <- rep(NA, length(iso.i))
  for (i in 1:length(iso.i)){
    getc.i[i] <- which(iso.c == iso.i[i])
  }
  return(getc.i)
}
