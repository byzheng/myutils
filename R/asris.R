
#' Download soil file from asris and convert to json which can be used in APSIM NG
#'
#' @param lon longitude
#' @param lat latitude
#' @param outfile output file
#' @param models path to models
#'
#' @return
#' @export
#'
#' @examples
asris_soil <- function(lon, lat, outfile, models) {
    servicePath = "https://www.asris.csiro.au/"
    soilURL = paste0(servicePath, 'ASRISApi/api/APSIM/getApsoil?longitude=', lon, '&latitude=', lat)
    message("Get Soil profile with ", soilURL)
    soildata = httr::GET(soilURL)
    response <- httr::content(soildata, "text")

    writeLines(response, outfile)
    models <- "Result/win-x64/Models.exe"
    cmd <- paste0(models, " ", outfile, " --upgrade")
    system(cmd)
}
