
.check_numeric_vector <- function(v) {
    if (!is.numeric(v)) {
        e <- v[seq_len(min(5, length(v)))]
        e <- paste(e, collapse = ", ")
        stop("Should be a numeric vector: ", e)
    }
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

#' Summarise a model with statistics indicators
#'
#' @param data A data frame to summarise
#' @param x x variable name
#' @param y y variable name
#'
#' @return A data frame with statistics indicators in columns
#' @export
#'
#' @examples
#' library(dplyr)
#' data <- data.frame(x = 1:10, y = 1:10)
#' data %>% model_summarise()
model_summarise <- function(data, x = "x", y = "y") {
    if (!(purrr::is_character(x) && length(x) == 1)) {
        stop("x variable should be character with length 1: ", x)
    }
    if (!(purrr::is_character(y) && length(y) == 1)) {
        stop("y variable should be character with length 1: ", y)
    }
    dplyr::summarise(data, r2 = rsq(.data[[x]], .data[[y]]),
              bias = Metrics::bias(.data[[x]], .data[[y]]),
              rmse = Metrics::rmse(.data[[x]], .data[[y]]),
              .groups = "drop")
}
