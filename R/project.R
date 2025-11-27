# About project managements


#' Source R Functions from Directory
#'
#' This function sources all R files from a specified directory and its subdirectories into global environment.
#'
#' @param folder Character string. Path to the directory containing R files relative to project root. 
#'   Default is "Rcode/function".
#' @return Invisible NULL. The function sources the R files for their side effects.
#' @export
#' @examples
#' \dontrun{
#' # Source all R files from default directory
#' project_fun()
#' 
#' # Source R files from custom directory
#' project_fun("my_functions")
#' }
project_fun <- function(folder = "Rcode/function") {
    folder_path <- here::here(folder)
    a <- list.files(folder_path, pattern = "\\.(R|r)$", 
        full.names = TRUE, 
        recursive = TRUE) |>
        sapply(source)
    return(invisible())
}
