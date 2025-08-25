modules <- c("proj", "geos", "gdal", "sqlite",
            "postgresql", "libxml2",
            "cmake", "udunits")
pkgs <- c("oz",
            "tidyverse",
            "roxygen2",
            "arrow",
            "RColorBrewer",
            "assertr",
            "ggpcp",
            "sf",
            "byzheng/weaana",
            "byzheng/rproject",
            "byzheng/rtiddlywiki",
            "byzheng/rapsimng",
            "byzheng/BWGS",
            "byzheng/taskqueue", 
            "byzheng/ragridata",
            "byzheng/twproject")


#' Install packages for myself
#'
#' @return no return
#' @export
install_packages <- function() {
    
    # Update existing packages
    if (!base::require("pak", quietly = TRUE, warn.conflicts = FALSE)) {
        utils::install.packages("pak")
    }
    #.load_modules()
    # CRAN packages
    i <- 1
    for (i in seq(along = pkgs)) {
        message("Install package: ", pkgs[i])
        pak::pkg_install(pkgs[i], upgrade = TRUE, ask = FALSE, dependencies = TRUE)
    }
}
