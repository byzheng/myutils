
#' Squared R
#'
#' @param x variable x
#' @param y variable y
#'
#' @return Squared R
#' @export
#'
#' @examples
#' rsq(runif(10), runif(10))
rsq <- function (x, y) {
    stats::cor(x, y) ^ 2
}
