#' # * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
#' # * Created:   09:19 PM Wednesday, 10 April 2013
#' # * Copyright: AS IS
#' # *
#'
#'
#' # file related function
#'
#' #' Sort files with index
#' #'
#' #' @param files A vector of files
#' #' @export
#' mySortFiles <- function(files)
#' {
#'     num <- as.numeric(gsub('^(\\d+)\\..*$', '\\1', basename(files)))
#'     files[order(num)]
#' }
#'
