
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
#' @importFrom here here
NULL


.import_extra <- function() {
    a <- rnaturalearthdata::coastline110
}
