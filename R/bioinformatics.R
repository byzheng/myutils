crops <- c("Wheat", "barley", "Canola")
#' Lists of chromosome for crops
#'
#' @param crops a vector of crops (i.e. wheat, barley, canola)
#' @param type return as list or data.frame
#'
#' @return a list or data.frame for predefined chromosome
#' @export
#'
#' @examples
#' crop_chromosome(c("wheat", "barley", "canola"), type = "data.frame")
#' crop_chromosome(c("wheat", "barley", "canola"))
crop_chromosome <- function(crops, type = c("list", "data.frame")) {
    crops <- tolower(crops)
    type <- match.arg(type)
    chroms <- list()
    if ("wheat" %in% crops) {
        wheat_chrom <- list(C2 = seq(1, 7),
                            C1 = c("A", "B", "D"))
        wheat_chrom <- expand.grid(wheat_chrom)
        wheat_chrom <- paste0(wheat_chrom$C2, wheat_chrom$C1)
        chroms[["wheat"]] <- wheat_chrom
    }
    if ("barley" %in% crops) {
        barley_chrom <- paste0(seq(1, 7), "H")
        chroms[["barley"]] <- barley_chrom
    }

    if ("canola" %in% crops) {
        canola_chrom <- c(paste0("chrA", seq(1, 10)),
                          paste0("chrC", seq(1, 9)))
        chroms[["canola"]] <- canola_chrom
    }
    if (type == "list") {
        return (chroms)
    } else if (type == "data.frame") {
        all_crops <- names(chroms)
        res <- list()
        for (i in seq(along = all_crops)) {
            res[[i]] <- data.frame(crop = all_crops[i], chromosome = chroms[[i]],
                                   stringsAsFactors = FALSE)
        }
        res <- do.call(rbind, res)
        return(res)
    }

}
