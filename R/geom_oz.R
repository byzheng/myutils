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
    ggplot2::geom_path(
        ggplot2::aes(
            x = !!rlang::sym("x"),
            y = !!rlang::sym("y"), 
            group = !!rlang::sym("g")
        ),
        data = oz_lines(),
        color = color,
        ...
    )
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

#' Geom label for Australian States
#'
#' @param color color for text
#' @param ACT Whether to include ACT (FALSE in default)
#'
#' @returns geom_text
#' @export
geom_oz_states <- function(color = "gray", ACT = FALSE) {
    state_labels <- data.frame(
        state = c("Queensland", "New South Wales", "Victoria", "Western Australia",
                  "South Australia", "Tasmania", "Northern Territory", "Australian Capital Territory"),
        code = c("QLD", "NSW", "VIC", "WA", "SA", "TAS", "NT", "ACT"),
        lon = c(145, 147, 144.5, 123, 135, 147, 133, 149),
        lat = c(-21, -32, -37, -26, -30, -42, -20, -35)
    )
    if (!ACT) {
        state_labels <- state_labels[state_labels$code != "ACT",]
    }
    ggplot2::geom_text(data = state_labels,
        ggplot2::aes(x = !!rlang::sym("lon"),
                    y = !!rlang::sym("lat"),
                    label = !!rlang::sym("code")),
        size = 4, color = color)
}
