
#' Install packages for myself
#'
#' @return no return
#' @export
install_packages <- function() {
    pkgs <- c("oz", "tidyverse", "RMySQL", "remotes")
    i <- 3
    for (i in seq(along = pkgs)) {
        utils::install.packages(pkgs[i])
    }

    # github packages
    g_packages <- c("byzheng/weaana", "byzheng/rproject",
      "byzheng/rtiddlywiki", "byzheng/rapsimng",
      "byzheng/BWGS", "byzheng/taskqueue")

    for (i in seq(along = g_packages)) {
        remotes::install_github(g_packages[i], force = "TRUE")
    }

}
