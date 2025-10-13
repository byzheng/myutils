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

.load_modules <- function() {
    if (.Platform$OS.type != "unix") {
        return(invisible(NULL))
    }
    module_check <- suppressWarnings(system("command -v module", 
            intern = TRUE, ignore.stderr = TRUE))
    if (!nzchar(module_check)) {
        return(invisible(NULL))
    }
    
    for (i in seq(along = modules)) {
        # Find all available versions for the module
        avail_cmd <- paste("module avail", modules[i])
        avail_out <- suppressWarnings(system(avail_cmd, intern = TRUE))
        avail_out <- avail_out[grepl(modules[i], avail_out)]
        if (length(avail_out) == 0) {
            next
        }
        # Extract versions and pick the highest one
        versions <- regmatches(avail_out, gregexpr(paste0(modules[i], "/[0-9.]+"), avail_out))
        versions <- unlist(versions)
        if (length(versions) == 0) {
            next
        }
        # Sort versions and pick the highest
        ver_nums <- sub(paste0(modules[i], "/"), "", versions)
        highest <- versions[order(package_version(ver_nums), decreasing = TRUE)][1]
        message("Loading module: ", highest)
        cmd <- sprintf("bash -l -c 'module load %s; env'", highest)
        env_out <- tryCatch(system(cmd, intern = TRUE), error = function(e) character())
        if (length(env_out) == 0) next
        env_vars <- strsplit(env_out, "=", fixed = TRUE)
        for (kv in env_vars) {
            if (length(kv) != 2) next
            do.call(Sys.setenv, stats::setNames(list(kv[2]), kv[1]))
        }
    }
    tryCatch(system("bash -l -c 'module list'"), error = function(e) NULL)
}

#' Install packages for myself
#'
#' @return no return
#' @export
install_packages <- function() {
    
    # Update existing packages
    if (!base::require("pak", quietly = TRUE, warn.conflicts = FALSE)) {
        utils::install.packages("pak")
    }
    # .load_modules()
    # CRAN packages
    i <- 1
    for (i in seq(along = pkgs)) {
        message("Install package: ", pkgs[i])
        pak::pkg_install(pkgs[i], upgrade = TRUE, ask = FALSE, dependencies = TRUE)
    }
}
