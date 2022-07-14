
#' @importFrom magrittr %>%
#' @importFrom snow setDefaultClusterOptions
#' @importFrom rlang .data
#' @importFrom expDB expdbConnect
#' @importFrom htcondor ncOpen
#' @importFrom parallel makeCluster
#' @importFrom rapsimng get_metfile
#' @importFrom rnaturalearth ne_coastline
#' @importFrom rproject project_fun
#' @importFrom tidyverse tidyverse_conflicts
#' @importFrom weaana readWeatherRecords
#' @importFrom rtiddlywiki tiddler_document
#' @importFrom here here
#' @importFrom fst read_fst
#' @importFrom RMySQL dbConnect
#' @importFrom RSQLite dbConnect
#' @importFrom BWGS bwgs.predict
NULL


.import_extra <- function() {
    a <- rnaturalearthdata::coastline110
}
