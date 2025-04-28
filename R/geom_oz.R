# Plot oz with ggplot2


#' Lines for Australia
#'
#' @return A data.frame for Australia coast and state lines
#' @export
#'
#' @examples
#' oz_lines()
oz_lines <- function() {
    oz_df <- oz::ozRegion()
    oz_df$rangey
    df <- list()
    for (i in seq(oz_df$lines)) {
        df[[i]] <- data.frame(x = oz_df$lines[[i]]$x,
                              y = oz_df$lines[[i]]$y,
                              g = i)
    }
    df <- do.call(rbind, df)
    df
}


#' Geometry of Australia coast and state lines
#'
#' @param ... Other arguments to geom_path
#' @return a geom object
#' @export
geom_oz <- function(...) {
    ggplot2::geom_path(ggplot2::aes_string(x = 'x', y = 'y', group = 'g'), data = oz_lines(), ...)
}
