# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   09:54 PM Wednesday, 10 April 2013
# * Copyright: AS IS
# *


# Weather records

#' Get PPD weather records by station
#' 
#' @param number The number of weather station 
#' @param latitude latitude
#' @param longtitude longitude
#' @param yrange Year range
#' @param file Path of netcdf file
#' @export
myGetPpd <- function(number = NULL, latitude = NULL, longitude = NULL,
                     yrange = NULL,
                     file = '../000-CommonFiles/Results/weather/ncWbPpd.nc')
{
    library(ncdf4)
    nc <- nc_open(file)
    nc_number <- NULL
    pos <- NULL
    if (!is.null(number))
    {
        nc_number <- ncvar_get(nc, 'number')
        pos <- match(number, nc_number)
        nc_number <- nc_number[pos]
    } else if (!is.null(latitude) & !is.null(longitude))
    {
        nc_latitude <- ncvar_get(nc, 'latitude')
        nc_longitude <- ncvar_get(nc, 'longitude')
        library(weaana)
        if (length(latitude) != length(longitude))
        {
            warning('latitude and longitude should have the same length')
            pos <- seq(1, min(length(latitude), length(longitude)))
            latitude <- latitude[pos]
            longitude <- longitude[pos]
        }
        nc_number <- NULL
        for (i in seq(along = latitude))
        {
            dis <- sphericalDistance(latitude[i], longitude[i], nc_latitude, nc_longitude)
            warning(sprintf('The closest PPD station is used with distance %s km',
                            format(min(dis), digits=3)))
            pos[i] <- which.min(dis)
            nc_number[i] <- ncvar_get(nc, 'number', pos[i], 1)
        }
    } else
    {
        stop('No station number specified')
    }
    met <- as.list(NULL)
    for (i in seq(along = nc_number))
    {
        nc_name <- ncvar_get(nc, 'name_lbl', c(1, pos[i]), c(-1, 1))
        nc_latitude <- ncvar_get(nc, 'latitude', pos[i], 1)
        nc_longitude <- ncvar_get(nc, 'longitude', pos[i], 1)
        climates <- ncvar_get(nc, 'climates', c(1, 1, pos[i]), c(-1, -1, 1))
        var <- ncvar_get(nc, 'var_lbl')
    
        climates <- as.data.frame(climates)
        names(climates) <- var
        nc$dim$time
        start_date <- as.Date(gsub('days since ', '', ncatt_get(nc, 'time', 'units')$value))
        end_date <- start_date + nrow(climates) - 1
        date_s <- seq(start_date, end_date, by = 1)
        climates$year <- as.numeric(format(date_s, '%Y'))
        library(weaana)
        climates$day <- DOY(date_s)
        if (!is.null(yrange))
        {
            climates <- climates[climates$year %in% yrange,]
        }
        met[[i]] <- list(Name = as.character(nc_name),
            Number = as.character(nc_number[i]),
            Latitude = as.numeric(nc_latitude),
            Longitude = as.numeric(nc_longitude),
            Records = climates)
    }
    
    nc_close(nc)
    met <- createWeaAna(met)
    met
}


# Weather records

#' Get information of closest PPD weather station
#' 
#' @param latitude latitude
#' @param longtitude longitude
#' @param file Path of netcdf file
#' @export
myFindPpdStation <- function(latitude, longitude,
                     file = project_filepath('Results/weather/ncWbPpd.nc', project = 'com'))
{
    library(ncdf4)
    nc <- nc_open(file)
    nc_number <- NULL
    pos <- NULL
    nc_latitude <- as.numeric(ncvar_get(nc, 'latitude'))
    nc_longitude <- as.numeric(ncvar_get(nc, 'longitude'))
    nc_name <- as.numeric(ncvar_get(nc, 'name_lbl'))
    nc_number <- as.numeric(ncvar_get(nc, 'number'))
    library(weaana)
    distance <- rep(NA, length = length(latitude))
    for (i in seq(along = latitude))
    {
        dis <- sphericalDistance(latitude[i], longitude[i], nc_latitude, nc_longitude)
        distance[i] <- min(dis)
        pos[i] <- which.min(dis)
    }
    nc_close(nc)
    data.frame(latitude = latitude, 
               longitude = longitude,
               ppd_name = nc_name[pos],
               ppd_number = nc_number[pos],
               ppd_latitude = nc_latitude[pos],
               ppd_longitude = nc_longitude[pos],
               distance = distance)
}



#' Download SILO weather records
#'
#' @param stations The number of weather station
#' @param output The output folder 
#' @param start The start time of weather records
#' @param end The end time of weather records
#' @export
myDownloadSILO <- function(stations, output, start = '1889/1/1', 
    end = format(Sys.time(), '%Y/%m/%d'))
{
    for (i in seq_along(stations))
    {
        file_name <- file.path(output, sprintf('%s.met', stations[i]))
        base_url <- 'http://apsrunet.apsim.info/cgi-bin/getData.met?format=apsim&station='
        start_date <- as.numeric(strsplit(start, '/')[[1]])
        end_date <- as.numeric(strsplit(end,'/')[[1]])
        
        site_url <- sprintf('%s%s&ddStart=%s&mmStart=%s&yyyyStart=%s&ddFinish=%s&mmFinish=%s&yyyyFinish=%s', 
            base_url, stations[i], 
            start_date[3], start_date[2], start_date[1],
            end_date[3] - 1, end_date[2], end_date[1])
        new_data <- readLines(site_url)
        writeLines(new_data, file_name)
    }
}



myWangBeta <- function(tav, t_min, t_opt, t_max, t_ref = t_opt) {
    res <- ifelse ((tav > t_min) & (tav < t_max),
                   {
                       a <- log(2.0) / log((t_max - t_min) / (t_opt - t_min))
                       refeff <- t_opt * (2 * ((tav - t_min)^a) * ((t_opt - t_min)^a) -
                                              ((tav - t_min) ^ (2 * a))) / ((t_opt - t_min) ^ (2 * a))
                       a <- log(2.0) / log((t_max - t_min) / (t_opt - t_min))
                       refefft <- t_opt * (2 * ((t_ref - t_min)^a) * ((t_opt - t_min)^a) -
                                               ((t_ref - t_min) ^ (2 * a))) / ((t_opt - t_min) ^ (2 * a))
                       refeff / refefft
                   }, 0)
    
    return (res)
}

