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
#' @param color color of lines
#' @param ... Other arguments to geom_path
#' @return a geom object
#' @export
geom_oz <- function(color = "gray", ...) {
    ggplot2::geom_path(ggplot2::aes_string(x = 'x', y = 'y', group = 'g'),
                       data = oz_lines(), color = color, ...)
}



#' Theme for map
#'
#' @returns  theme object for ggplot2
#' @export
theme_map <- function() {
    ggplot2::`%+replace%`(
        ggplot2::theme_bw(),
        ggplot2::theme(
            axis.title   = ggplot2::element_blank(),
            axis.ticks   = ggplot2::element_blank(),
            axis.text    = ggplot2::element_blank(),
            panel.grid   = ggplot2::element_blank(),
            panel.border = ggplot2::element_blank(),
            complete     = TRUE
        )
    )
}
