#' Create tabsets for R Markdown and Quarto documents
#'
#' This function generates tabsets that can be used in R Markdown or Quarto 
#' documents to organize content into tabs. It provides a convenient way to 
#' create multiple tabs with headers and content. 
#'
#' @details This function should be used within R Markdown chunks with 
#' `results="asis"` to properly render the tabset markup.
#'
#' @param data A data frame grouped by one or more variables using `dplyr::group_by()`.
#' @param FUN A function that generates the content for each tab.
#' @param ... Arguments passed to `FUN`.
#' @param use_quarto Logical; if `TRUE`, generates Quarto-compatible tabsets
#'
#' @return A character vector or HTML structure representing the tabsets
#'
#' @examples
#' iris |> 
#'   dplyr::group_by(Species) |>
#'   tabsets(FUN = knitr::kable)
#' iris |> 
#'   dplyr::group_by(Species) |>
#'   tabsets(FUN = knitr::kable, use_quarto = FALSE)
#' @export
tabsets <- function(
    data,
    FUN,
    ...,
    use_quarto = TRUE
) {
    stopifnot(is.data.frame(data))

    groups <- dplyr::group_vars(data)
    if (length(groups) == 0) {
        stop("`tabsets()` requires grouped data. Use dplyr::group_by().")
    }

    # Recursive renderer
    render_level <- function(df, vars, level) {
        if (length(vars) == 0) {
            out <- FUN(df, ...)
            print(out)
            return(invisible())
        }

        var <- vars[1]
        vals <- if (is.factor(df[[var]])) levels(df[[var]]) else unique(df[[var]])

        if (use_quarto) {
            # Quarto-native tabset
            cat("::: {.panel-tabset}\n\n")

            for (v in vals) {
                sub_df <- df[df[[var]] == v, , drop = FALSE]
                if (nrow(sub_df) == 0) next

                cat("## ", v, "\n\n", sep = "")
                render_level(sub_df, vars[-1], level)
                cat("\n\n")
            }

            cat(":::\n\n")
        } else {
            # Pandoc / rmarkdown tabset
            cat(strrep("#", level), " ", var, " {.tabset}\n\n", sep = "")

            for (v in vals) {
                sub_df <- df[df[[var]] == v, , drop = FALSE]
                if (nrow(sub_df) == 0) next

                cat(strrep("#", level + 1), " ", v, "\n\n", sep = "")
                render_level(sub_df, vars[-1], level + 2)
                cat("\n\n")
            }
        }
    }

    base_level <- if (use_quarto) 2 else 2

    render_level(data, groups, base_level)

    invisible(dplyr::ungroup(data))
}
