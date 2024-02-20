
#' Install packages for myself
#'
#' @return no return
#' @export
install_packages <- function() {
    if (!require("pak")) {
        install.packages("pak")
    }
    pkgs <- c("oz", "tidyverse", "RMySQL")
    i <- 3
    for (i in seq(along = pkgs)) {
        install.packages(pkgs[i])
    }
}
