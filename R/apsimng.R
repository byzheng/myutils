# Function related with APSIM NG

#' Get releae cultivars from apsimx
#'
#' @param crop The crop name
#'
#' @return A data frame for all cultivar name and parameter values
#' @export
apsimng_cultivars <- function(crop) {
    stopifnot(length(crop) == 1)
    url <- sprintf("https://raw.githubusercontent.com/APSIMInitiative/ApsimX/master/Models/Resources/%s.json",
                   crop)
    message("Get cultivar name and parameter values from ",
            url)
    model <- rapsimng::read_apsimx(url)
    cultivars <- rapsimng::get_cultivar(model, alias = TRUE)
    cultivars
}
