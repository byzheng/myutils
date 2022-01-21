
.check_numeric_vector <- function(v) {
    if (!is.numeric(v)) {
        e <- v[seq_len(min(5, length(v)))]
        e <- paste(e, collapse = ", ")
        stop("Should be a numeric vector: ", e)
    }
}


#'  Slope of linear regression
#'
#' @param x a numeric vector for x
#' @param y a numeric vector for y
#'
#' @return Slope of linear regression
#' @export
#'
#' @examples
#' slope(runif(10), runif(10))
slope <- function(x, y) {
    stats::cor(x, y) *(stats::sd(y)/stats::sd(x))
}
#' Squared coefficient of correlation (R2)
#'
#' @param x variable x
#' @param y variable y
#'
#' @return Squared coefficient of correlation (R2) or coefficient of determination.
#' @export
#'
#' @examples
#' rsq(runif(10), runif(10))
rsq <- function (x, y) {
    .check_numeric_vector(x)
    .check_numeric_vector(y)
    stats::cor(x, y) ^ 2
}


#' Normalized root mean square error (nrmse)
#'
#' @param x variable x
#' @param y variable y
#' @param method method to calculate nrmse. Valid values are:
#' \itemize{
#'     \item{sd: standard deviation of observations (x)}
#'     \item{maxmin: difference between maximum and minimum observations (x)}
#'     \item{mea: average of observations (x)}
#'     \item{iq: the difference between 25th and 75th percentile (x)}
#' }
#'
#' @return Normalized root mean square error.
#' @export
#'
#' @examples
#' nrmse(runif(10), runif(10))
nrmse <- function(x, y,
                  method = c("maxmin", "sd", "mean", "iq")) {
    method <- match.arg(method)
    .check_numeric_vector(x)
    .check_numeric_vector(y)
    value <- NULL
    if (method == "sd") {
        value <- Metrics::rmse(x, y) / stats::sd(x)
    } else if (method == "maxmin") {
        value <- Metrics::rmse(x, y) / diff(range(x))
    } else if (method == "mean") {
        value <- Metrics::rmse(x, y) / mean(x)
    } else if (method == "iq") {
        value <- Metrics::rmse(x, y) / as.numeric(diff(stats::quantile(x, c(0.25, 0.75))))
    } else {
        stop("Method ", method, " is not implemented.")
    }
    return (value)
}

#' Summarise a model with statistics indicators
#'
#' @param data A data frame to summarise
#' @param x x variable name
#' @param y y variable name
#' @param digits integer indicating the number of decimal places (round) or significant digits (signif) to be used.
#' @param direction the wide (default) or long format for the output
#' @param ... other arguments passing to functions. Supporting arguments
#' \itemize{
#'     \item{nrmse_method: Method for nrmse}
#' }
#'
#' @return A data frame with statistics indicators in columns including:
#' \itemize{
#'     \item{n: Number of rows}
#'     \item{r: Coefficient of correlation}
#'     \item{r2: Squared coefficient of correlation}
#'     \item{bias: Average amount by which actual is greater than predicted}
#'     \item{mse: Average squared difference}
#'     \item{rmse: Root mean squared error}
#'     \item{nrmse: Normalized root mean squared error}
#' }
#' @export
#'
#' @examples
#' library(dplyr)
#' data <- data.frame(x = 1:10, y = 1:10 + runif(10))
#' data %>% model_summarise()
#' data %>% model_summarise(digits = 2)
#' # Export as long format
#' data %>% model_summarise(digits = 2, direction = "long")
model_summarise <- function(data, x = "x", y = "y",
                            digits = NULL,
                            direction = c("wide", "long"),
                            ...) {
    direction <- match.arg(direction)
    if (!(purrr::is_character(x) && length(x) == 1)) {
        stop("x variable should be character with length 1: ", x)
    }
    if (!(purrr::is_character(y) && length(y) == 1)) {
        stop("y variable should be character with length 1: ", y)
    }

    # Check other arguments
    other_args <- list(...)
    other_args_names <- names(other_args)
    if ("nrmse_method" %in% other_args_names) {
        nrmse_method <- other_args[["nrmse_method"]]
    } else {
        nrmse_method <- "mean"
    }
    res <- dplyr::summarise(data,
                     n = dplyr::n(),
                     r = stats::cor(.data[[x]], .data[[y]]),
                     r2 = rsq(.data[[x]], .data[[y]]),
                     bias = Metrics::bias(.data[[x]], .data[[y]]),
                     mse = Metrics::mse(.data[[x]], .data[[y]]),
                     rmse = Metrics::rmse(.data[[x]], .data[[y]]),
                     nrmse = nrmse(.data[[x]], .data[[y]], method = nrmse_method),
              .groups = "drop")
    if (!is.null(digits) && length(digits) == 1 && is.numeric(digits)) {
        res <- res %>%
            dplyr::mutate(r2 = round(.data$r2, digits),
                   r = round(.data$r, digits),
                   mse = round(.data$mse, digits),
                   bias = round(.data$bias, digits),
                   rmse = round(.data$rmse, digits),
                   nrmse = round(.data$nrmse, digits))
    }
    if (direction == "long") {
        res <- res %>%
            tidyr::pivot_longer(cols = c("n", "r", "r2", "bias", "mse", "rmse", "nrmse"),
                                names_to = "indicator")
    }
    res
}
