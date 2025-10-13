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
doy2date <- function(doy, format = '%d-%b') {
    format(as.Date(doy, origin = '2010-12-31'), format = format)
}

#' First day (day of year) of month
#'
#' @return a numeric vector of first day of month (day of year)
#' @export
month_first_day <- function() {
    c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
}


#' First day of month
#'
#' @return a character vector of first day of month (day of year)
#' @export
month_first_day_label <- function() {
    doy2date(month_first_day())
}

#' Convert date to DOY
#' @param date The date like 1-May
#' @export
date2doy <- function(date) {
    as.numeric(as.Date(paste(date, '-2011', sep = ''), format = '%d-%b-%Y')) -
        as.numeric(as.Date('2010-12-31'))
}
#' Custom X-axis Scale for first day of months
#'
#' This function creates a ggplot2 continuous x-axis scale with breaks at the first day of each month,
#' and labels corresponding to those dates. It is useful for time series plots where the x-axis represents months.
#'
#' @param ... Additional arguments passed to `ggplot2::scale_x_continuous`.
#' @return A ggplot2 scale object for the x-axis.
#' @examples
#' data <- data.frame(
#'   date = seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day"),
#'   value = rnorm(365)
#' )
#' library(ggplot2)
#' ggplot(data, aes(x = date, y = value)) +
#'   geom_line() +
#'   scale_x_month()
#' @export
scale_x_month <- function(...) {
    ggplot2::scale_x_continuous(breaks = month_first_day(),
                    labels = month_first_day_label(), ...) 
}

#' Custom Y-axis Scale for first day of months
#'
#' This function creates a ggplot2 continuous y-axis scale with breaks at the first day of each month,
#' and labels corresponding to those dates. It is useful for plots where the y-axis represents months.
#'
#' @param ... Additional arguments passed to `ggplot2::scale_y_continuous`.
#' @return A ggplot2 scale object for the y-axis.
#' @examples
#' data <- data.frame(
#'   date = seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day"),
#'   value = rnorm(365)
#' )
#' library(ggplot2)
#' ggplot(data, aes(x = value, y = date)) +
#'   geom_point() +
#'   scale_y_month()
#' @export
scale_y_month <- function(...) {
    ggplot2::scale_y_continuous(breaks = month_first_day(),
                    labels = month_first_day_label(), ...) 
}