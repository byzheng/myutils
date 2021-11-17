# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   03:01 PM Tuesday, 14 May 2013
# * Copyright: AS IS
# *


#' Generate a new array a vector to a array for a fixed dimension
#'
#' @param arr arr
#' @param vector vector
#' @param vector idx
#' @export
myArrayGen <- function(arr, vector, idx)
{
    if (length(vector) != dim(arr)[idx])
    {
        stop(sprintf('length of vector should be %s for dimension %s', 
            length(vector), idx))
    }
    if (idx == 1)
    {
        res <- vector
    } else
    {
        res <- rep(vector, each = prod(dim(arr)[seq(length = idx - 1)]))
    }
    return (res)
}

#' Subset array by value of dimensions
#' 
#' @param x object to be subsetted.
#' @param ... values of dimension
#' @export
mySubsetArr <- function(x, ...)
{
    if (is.null(dimnames(x)))
    {
        stop('Dimension names must be specified')
    } 
    dim_names <- dimnames(x)
    dim_names_name <- names(dim_names)
    if (sum(nchar(dim_names_name) == 0) > 0)
    {
        stop('Dimension names must be specified')
    }
    
    args_dim <- list(...)
    
    args_dim_names <- names(args_dim)
    args_subset <- list(x)
    for (i in seq(along = dim_names_name))
    {
        if (dim_names_name[i] %in% args_dim_names)
        {
            args_subset[[i+1]] <- dim_names[[i]] %in% as.character(args_dim[[dim_names_name[i]]])
        } else
        {
            args_subset[[i+1]] <- rep(TRUE, length(dim_names[[i]]))
        }
    }
    args_subset$drop <- FALSE
    do.call(`[`, args_subset)
}

#' Apply Functions Over Array Margins
#'
#' @param x an array, including a matrix.
#' @param MARGIN a vector giving the subscripts which the function will be applied over.
#' Or a character vector for dimension names will be applied over
#' @param FUN the function to be applied.
#' @param ... optional arguments to FUN.
#' @export
myApply <- function(x, MARGIN, FUN, ...)
{
    if (class(MARGIN) %in% 'character')
    {
        execlude <- FALSE
        if (sum(substring(MARGIN, 1, 1) == '-') == length(MARGIN))
        {
            MARGIN <- substring(MARGIN, 2, nchar(MARGIN))
            execlude <- TRUE
        }
        MARGIN_t <- match(MARGIN, names(dimnames(x)))
        if (sum(is.na(MARGIN_t)) > 0)
        {
            stop(sprintf('Dimensions don\'t exist: %s', 
                paste(MARGIN[is.na(MARGIN_t)], collapse = ', ')))
        }
        if (execlude)
        {
            MARGIN <- seq_along(dim(x))[-MARGIN_t]
        } else
        {    
            MARGIN <- sort(MARGIN_t)
        }
    }
    apply(x, MARGIN, FUN, ...)
}

#' Generate an empty array with a list
#'
#' @param lst A list to generate an array
#' @export
myList2Array <- function(lst)
{
    arr <- array(NA, dim = as.numeric(unlist(lapply(lst, length))))
    dimnames(arr) <- lst
    arr
}


#' The the slope of linear regression
#' 
#' @param x a vector of model variables. If null, fill along with y.
#' @param y a vector of response.
#' @export
myLmSlope <- function(y, x = NULL)
{
    y <- as.numeric(y)
    if (is.null(x))
    {
        x <- seq(along = y)
    }
    slope <- cor(x, y) * sd(y)/sd(x)
    return(slope)
}

#' Export to Trac WIKI table
#'
#' @param x an R object (data.frame)
#' @export
myTracTable <- function(x)
{
    library(magrittr)
    c(paste0('=', names(x), '=') %>% paste(collapse = '||'),
        apply(x, 1, paste, collapse = '||')) %>%
        paste0('||', ., '||') %>%
        paste(collapse = '\n') %>%
        cat
}


#' Convert plantuml into figure
#'
#' @param uml PlantUML text
#' @param text Text to show
#' @export

myPlantUML <- function(uml, text)
{
    filename <- tempfile()
    uml_filename <- paste0(filename, ".uml")
    
    write(uml, uml_filename)
    system2("java", paste0(" -jar plantuml.jar ", uml_filename))
    
    # cat(paste0('![', text, '](', filename, '.png)'))
    library(png)
    library(grid)
    img <- readPNG(paste0(filename, '.png'))
    grid.raster(img)
}
