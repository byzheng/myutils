

#' Generate an vector with size and length
#'
#' @param size size of each chunk
#' @param length length of total vector
#'
#' @return A vector
#' @export
#'
#' @examples
#' chunk(10, 50)
chunk <- function(size, length) {
    stopifnot(is.numeric(size))
    stopifnot(is.numeric(length))
    stopifnot(length(size) == 1)
    stopifnot(length(length) == 1)
    group <- seq(1, ceiling(length/size))
    group <- rep(group, each = size ,
                 length.out = length)
    group
}
