
#' Install packages for myself
#'
#' @return no return
#' @export
install_packages <- function() {

    # Update existing packages
    if (!base::require("pak", quietly = TRUE, warn.conflicts = FALSE)) {
        utils::install.packages("pak")
    }
    # CRAN packages
    pkgs <- c("oz",
              "tidyverse",
              "RMySQL",
              "remotes",
              "roxygen2",
              "arrow",
              "RColorBrewer",
              "ggpcp",
              "byzheng/weaana",
              "byzheng/rproject",
              "byzheng/rtiddlywiki",
              "byzheng/rapsimng",
              "byzheng/BWGS",
              "byzheng/taskqueue")

    for (i in seq(along = pkgs)) {
        message("Install package: ", pkgs[i])
        if (.Platform$OS.type == "unix") {
            dps <- pak::pkg_deps(pkgs[i])
            for (j in seq(along = dps[[1]])) {
                pak::pkg_install(dps$package[j], upgrade = TRUE, ask = FALSE)
            }
        }
        pak::pkg_install(pkgs[i], upgrade = TRUE, ask = FALSE)
        #
        # if (!require(pkgs[i], character.only = TRUE, quietly = TRUE)) {
        #     utils::install.packages(pkgs[i])
        # }
    }
#
#     # github packages
#     g_packages <- c("byzheng/weaana", "byzheng/rproject",
#       "byzheng/rtiddlywiki", "byzheng/rapsimng",
#       "byzheng/BWGS", "byzheng/taskqueue")
#
#     for (i in seq(along = g_packages)) {
#         if (!require(pkgs[i], character.only = TRUE, quietly = TRUE)) {
#             remotes::install_github(g_packages[i], force = "TRUE")
#         }
#     }

}
