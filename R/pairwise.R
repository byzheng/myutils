

#' Apply a function to pairwise of all combinations in row or column of a matrix.
#'
#' @param X a matrix
#' @param margin row (1) or column (2) to apply the function
#' @param FUN the function to be applied, which has two arguments
#' and only returns a single value.
#' @param ... optional arguments to FUN
#'
#' @return a matrix with the same number of row (margin = 1) or column (margin = 2)
#' with the original matrix.
#' @export
#'
#' @examples
#' X <- matrix(c(1, 2, 3, 4,
#'               1, 2, 5, 6,
#'               1, 2, 5, 7), nrow = 4)
#' FUN <- function(x, y, na.rm = FALSE) {
#'    sum(x != y, na.rm = na.rm)
#' }
#' pairwise(X, 2, FUN, na.rm = TRUE)
pairwise <- function(X,
                     margin = c(1, 2),
                     FUN,
                     ...) {
    dim_x <- dim(X)
    # Check argument
    if (!("matrix" %in% class(X)) ||
        length(dim_x) != 2) {
        stop("Require X is a matrix")
    }
    if (!(margin %in% c(1, 2))) {
        stop("margin should be either 1 or 2")
    }
    n_x <- dim_x[margin]
    if (margin == 2) {
        X <- t(X)
    }
    # generate all combinations
    p_matrix <- t(utils::combn(seq_len(n_x),2))
    p_matrix <- rbind(p_matrix,  matrix(rep(seq_len(n_x), times = 2), ncol = 2))

    # check function
    res1 <- FUN(X[1,], X[2,], ...)
    if (length(res1) != 1) {
        stop("FUN requires to output with single value.")
    }
    p_res <- apply(p_matrix, 1, function(idx) {
        FUN(X[idx[1],], X[idx[2],], ...)
    })

    res <- matrix(NA, nrow = n_x, ncol = n_x)
    res[p_matrix] <- p_res
    res[p_matrix[,c(2,1)]] <- p_res
    res[matrix(rep(seq_len(n_x), times = 2), ncol = 2)] <- 0

    dimnames(res) <- list(dimnames(X)[[1]], dimnames(X)[[1]])
    res
}

#' Calculate the pairwise difference of all combinations in row or column of a matrix.
#'
#' @param X a matrix
#' @param margin row (1) or column (2) to apply the function
#' @param na.rm logical. Should missing values (including NaN) be removed?
#'
#' @return a matrix with the same number of row (margin = 1) or column (margin = 2)
#' with the original matrix.
#' @export
#'
#' @examples
#' X <- matrix(c(1, 2, 3, 4,
#'               1, 2, 5, 6,
#'               1, 2, 5, 7), nrow = 4)
#' pairwise_diff(X, 2)
pairwise_diff <- function(X, margin = c(1, 2), na.rm = FALSE) {
    FUN <- function(x, y, na.rm = FALSE) {
        sum(x != y, na.rm = na.rm)
    }
    res <- pairwise(X, margin = margin, FUN = FUN, na.rm = na.rm)
    res
}
