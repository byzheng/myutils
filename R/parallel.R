

#' Title
#'
#' @param x 
#' @param fun 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
myRunParallel <- function(x, fun, cpus = parallel::detectCores() - 1, ...) {
    # Run parallel
    cl <- snow::getMPIcluster() 
    args = commandArgs(trailingOnly = TRUE)
    if (!is.null(cl)) {
        # In case of MPI cluster, e.g. pearcey
        r <- snow::parLapply(cl, x, fun, ...)
    } else if (length(args) > 0) {
        # In case of win hpc pack for parameter sweep job
        # Assume the first argument is the idx
        # We should not expect any results return from this method
        idx <- as.numeric(args[1])
        # in case of more sweep job are created
        if (idx <= length(x)) {
            fun(idx, ...)
        }
        r <- NULL
    } else {
        # In other case for local computer
        library(snowfall)
        sfInit(cpus = cpus, 
               parallel = TRUE, slaveOutfile = 'tmp.txt')
        r <- sfLapply(x, fun, ...)
        sfStop()
    }
    r
}
