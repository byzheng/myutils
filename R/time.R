# Function related with data time

#' Convert day of year (doy) to date in month and day format
#'
#' @details
#' No leap year is considered
#'
#' @param doy a vertor of day of year
#' @param format the date format
#' @export
#' @return
#' Date in month and day format in character format
#' @examples
#' doy2date(seq(1, 10))
doy2date <- function(doy, format = '%d-%b')
{
    format(as.Date(doy, origin = '2010-12-31'), format = format)
}

#' First day (day of year) of month
#'
#' @return a numeric vector of first day of month (day of year)
#' @export
month_first_day <- function()
{
    c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
}


#' First day of month
#'
#' @return a character vector of first day of month (day of year)
#' @export
month_first_day_label <- function()
{
    doy2date(month_first_day())
}

#' Convert date to DOY
#' @param date The date like 1-May
#' @export
date2doy <- function(date)
{
    as.numeric(as.Date(paste(date, '-2011', sep = ''), format = '%d-%b-%Y')) -
        as.numeric(as.Date('2010-12-31'))
}
