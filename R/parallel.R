

#' A wrap function to run parallel in different platforms
#'
#' @details
#' The supported platforms include
#' * local computer through snowfall package (default)
#' * MPI cluster through snow package
#' * Windows HPC pack for parameter sweep job
#'
#' @param x A numeric vector for parallel
#' @param fun A function
#' @param cpus Number of CPUs for snowfall package
#' @param ... Other arguments passed to fun
#'
#' @return Result of parallel function.
#' @export
#' @examples
#' \dontrun{
#' fun <- function(i, a) {
#'   return(i * a)
#' }
#' run_parallel(seq(1, 10), fun, a = 2)
#' }
run_parallel <- function(x, fun, cpus = parallel::detectCores() - 1, ...) {
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
    } else if (taskqueue::is_db_connect()) {
        # in case of task queue server can be connected
        project_name <- stringi::stri_rand_strings(1, length = 10)
        taskqueue::project_add(project_name)
        taskqueue::project_resource_add()
    } else {
        # In other case for local computer
        snowfall::sfInit(cpus = cpus,
               parallel = TRUE)
        r <- snowfall::sfLapply(x, fun, ...)
        snowfall::sfStop()
    }
    r
}
