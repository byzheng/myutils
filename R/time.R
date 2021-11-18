#' # * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
#' # * Created:   11:37 AM Wednesday, 10 April 2013
#' # * Copyright: AS IS
#' # *
#'
#'
#' # Function related with data time
#'
#' #' Convert doy to date
#' #'
#' #' @param doy a vertor of day of year
#' #' @param format the date format
#' #' @export
#' myDoy2Date <- function(doy, format = '%d-%b')
#' {
#'     format(as.Date(doy, origin = '2010-12-31'), format = format)
#' }
#'
#' #' Return the first day of month
#' #'
#' #' @export
#' myFirstDayOfMonth <- function()
#' {
#'     c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
#' }
#'
#' #' Convert date to DOY
#' #' @param date The date like 1-May
#' #' @export
#' myDate2Doy <- function(date)
#' {
#'     as.numeric(as.Date(paste(date, '-2011', sep = ''), format = '%d-%b-%Y')) -
#'         as.numeric(as.Date('2010-12-31'))
#' }
