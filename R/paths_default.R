#' Get the default data set path
#'
#' @return installed package default data paths
default_data_set_path = function() {
  pp = system.file(package = 'dhsdata', mustWork=TRUE)
  dp = file.path(pp, 'data')
  dp
}
