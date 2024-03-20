

#' Use taskqueue on petrichor
#'
#' @return No return
#' @export
use_petrichor <- function() {
    taskqueue::db_clean()
    taskqueue::db_init()
    taskqueue::resource_add(name = "petrichor",
                 host = "petrichor-login.hpc.csiro.au",
                 type = "slurm",
                 workers = 2500,
                 log_folder = "/datasets/work/af-caps/work/zhe00a/Working/taskqueue-log")
}
