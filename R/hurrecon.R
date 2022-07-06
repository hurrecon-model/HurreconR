# Copyright (C) President and Fellows of Harvard College 2020

# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public
#   License along with this program.  If not, see
#   <http://www.gnu.org/licenses/>.

###############################################################################

# The HURRECON model estimates wind speed, wind direction, enhanced Fujita 
# scale wind damage, and duration of EF0 to EF5 winds as a function of hurricane 
# location and maximum sustained wind speed. Results may be generated for a 
# single site or an entire region. Hurricane track and intensity data may be 
# imported directly from the US National Hurricane Center's HURDAT2 database.

### INTERNAL FUNCTIONS ####################################

# create environment to store path

hur_env <- new.env(parent=emptyenv())

#' get_path returns the path for the current set of model runs.
#' If not set, an error message is displayed.
#' @return current path
#' @noRd

get_path <- function() {
    # display error message if not set
    if (!exists("hur_path", envir=hur_env)) {
        stop("Path not set. Please use hurrecon_set_path.", call. = FALSE)

    # otherwise return current path
    } else {
        hur_path <- hur_env[["hur_path"]]
        invisible(hur_path)
    }
}

#' get_fujita_wind_speeds returns a vector containing the minimum 3-second 
#' wind gust speed (meters/second) for each enhanced Fujita class.
#' @return a vector of 3-second gust speeds
#' @noRd

get_fujita_wind_speeds <- function() {
    ef0 <- 29.1 # meters/second
    ef1 <- 38.0 # meters/second
    ef2 <- 49.2 # meters/second
    ef3 <- 60.4 # meters/second
    ef4 <- 73.8 # meters/second
    ef5 <- 89.4 # meters/second

    return(c(ef0, ef1, ef2, ef3, ef4, ef5))
}

#' get_fujita_colors returns a vector containing the color for each 
#' enhanced Fujita class.
#' @return a vector of colors
#' @noRd

get_fujita_colors <- function() {
    ef0_col <- "purple"
    ef1_col <- "blue"
    ef2_col <- "green"
    ef3_col <- "yellow"
    ef4_col <- "orange"
    ef5_col <- "red"
    efx_col <- "grey"

    return(c(ef0_col, ef1_col, ef2_col, ef3_col, ef4_col, ef5_col, efx_col))
}

#' get_hur_id reformats a hurricane ID from HURDAT2 in a form that facilitates
#' sorting by year. For example: AL061938 is reformatted as AL1938-06.
#' @param hd2_id hurricane ID from HURDAT2
#' @return a reformated hurricane ID
#' @noRd

get_hur_id <- function(hd2_id) {
    basin <- substr(hd2_id, 1, 2)
    num   <- substr(hd2_id, 3, 4)
    year  <- substr(hd2_id, 5, 8)

    hur_id <- paste(basin, year, '-', num, sep="")
    
    return(hur_id)
}

#' format_time_difference_hms returns a time difference formatted as
#' hours:minutes:seconds.
#' @param start_time start time
#' @param end_time end time
#' @return a time difference formatted as hh:mm:ss.
#' @noRd

format_time_difference_hms <- function(start_time, end_time) {
    dsec <- as.numeric(difftime(end_time, start_time, units="secs"))
  
    hours <- floor(dsec/3600)
    minutes <- floor((dsec - 3600*hours)/60)
    seconds <- dsec - 3600*hours - 60*minutes

    hh <- formatC(hours, width=2, format="d", flag="0")
    mm <- formatC(minutes, width=2, format="d", flag="0")
    ss <- formatC(seconds, width=4, format="f", digits=1, flag="0")

    t_diff <- paste(hh, mm, ss, sep=":")

    return(t_diff)
}

#' format_time_difference_ms returns a time difference in milliseconds.
#' @param start_time start time
#' @param end_time end time
#' @return a time difference in milliseconds
#' @noRd

format_time_difference_ms <- function(start_time, end_time) {
    t_diff <- formatC(1000*difftime(end_time, start_time, units="secs"), width=3, format="f", digits=0, flag="0")

    return(t_diff)
}

#' check_file_exists displays an error message and stops execution if
#' the specified file does not exist.
#' @param file_name name of file
#' @return no return value
#' @noRd

check_file_exists <- function(file_name) {
    if (file.exists(file_name) == FALSE) {  
        stop("File not found: ", file_name, call. = FALSE)
    }
}

#' check_graphics_legend_location checks if the specified legend location is valid.
#' @param loc legend location
#' @return TRUE or FALSE
#' @noRd

check_legend_location <- function(loc) {
    locations <- c("topleft", "topright", "bottomleft", "bottomright")

    return (is.element(loc, locations))
}

#' read_site_file reads a site file and returns a vector containing the
#' latitude (degrees), longitude (degrees), and cover type (water=1, land=2)
#' for the specified site.
#' @param site_name name of site
#' @return vector of latitude, longitude, and cover type
#' @noRd

read_site_file <- function(site_name) {
    # get path
    hur_path <- get_path()

    # read site file
    site_file <- paste(hur_path, "/input/sites.csv", sep="")
    check_file_exists(site_file)
    ss <- utils::read.csv(site_file)
    names(ss)[1] <- "site_name"

    # get site location & cover type
    index <- which(ss$site_name == site_name)

    if (length(index) == 0) {   
        stop("Site not found", call. = FALSE)
    }

    i <- min(index)
    site_latitude <- ss$latitude[i]
    site_longitude <- ss$longitude[i]
    cover_type <- ss$cover_type[i]
 
    return(c(site_latitude, site_longitude, cover_type))
}

#' read_parameter_file reads a parameter file and returns a vector containing
#' the radius of maximum wind (rmw) (kilometers) and a scaling parameter (profile 
#' exponent) (s_par). If width is TRUE, parameters are returned for the specified 
#' hurricane; otherwise parameters for ALL are returned.
#' @param hur_id hurricane id
#' @param width whether to use width parameters for the specified hurricane
#' @return vector of rmw and s_par
#' @noRd

read_parameter_file <- function(hur_id, width) {
    # get path
    hur_path <- get_path()

    # read parameter file
    par_file <- paste(hur_path, "/input/parameters.csv", sep="")
    check_file_exists(par_file)
    pp <- utils::read.csv(par_file)
    names(pp)[1] <- "hur_id"

    # get rmw & s_par parameters
    if (width == TRUE) {
        index <- which(pp$hur_id == hur_id)
        if (length(index) == 0) {
        stop("Parameters not found for: ", hur_id, call. = FALSE)  
    } 

    } else {
        index <- which(pp$hur_id == "ALL")
        if (length(index) == 0) {
            stop("Parameter file must contain an entry for ALL", call. = FALSE)    
        }
    }

    i <- min(index)
    rmw <- pp$rmw[i]
    s_par <- pp$s_par[i]

    return(c(rmw, s_par))
}

#' get_fixed_model_parameters returns a vector of fixed model parameters,
#' including asymmetry factor, inflow angle, friction factor, and gust factor.
#' @param cover_type cover type (1=water, 2=land)
#' @return a vector of fixed model parameters
#' @noRd

get_fixed_model_parameters <- function(cover_type) {
    asymmetry_factor <- 1.0

    # water
    if (cover_type == 1) {
        inflow_angle <- 20 # degrees
        friction_factor <- 1.0
        gust_factor <- 1.2

    # land
    } else if (cover_type == 2) {
        inflow_angle <- 40 # degrees
        friction_factor <- 0.8
        gust_factor <- 1.5

    } else {
        stop("Cover type must be 1 (water) or 2 (land)", call. = FALSE)
    }

    return(c(asymmetry_factor, inflow_angle, friction_factor, gust_factor))
}

#' get_time_step calculates the time step (minutes) for regional modeling, 
#' assuming a maximum hurricane forward speed of 20 meters per second (1200 
#' meters per minute). Values are rounded to the nearest 1, 2, 3, 5, 10, 15, 
#' 30, or 60 minutes.
#' @return time step in minutes
#' @noRd

get_time_step <- function() {
    # get path
    hur_path <- get_path()

    # read land-water file
    land_water_file <- paste(hur_path, "/input/land_water.tif", sep="")
    check_file_exists(land_water_file)
    land_water <- raster::raster(land_water_file)

    # get cell height in meters (at latitude  45 degrees)
    nrows <- dim(land_water)[1]
    lat_min <- raster::extent(land_water)[3]
    lat_max <- raster::extent(land_water)[4]
    cell_y <- 111132 * (lat_max - lat_min)/nrows

    # calculate time step
    ts <- round(cell_y/1200)

    if (ts <= 1) {
        time_step <- 1
    } else if (ts <= 2) {
        time_step <- 2
    } else if (ts <= 4) {
        time_step <- 3
    } else if (ts <= 7) {
        time_step <- 5
    } else if (ts <= 12) {
        time_step <- 10
    } else if (ts <= 22) {
        time_step <- 15
    } else if (ts <= 45) {
        time_step <- 30
    } else {
        time_step <- 60
    }  
  
    return(time_step)
}

#' read_hurricane_track_file reads a hurricane track file and returns
#' a data frame of track data for the specfied hurricane.
#' @param hur_id hurricane id
#' @return a data frame of track data
#' @noRd

read_hurricane_track_file <- function(hur_id) {
    # get path
    hur_path <- get_path()

    # read hurricane track file
    read_hurricane_track_file
    track_file <- paste(hur_path, "/input/tracks.csv", sep="")
    check_file_exists(track_file)
    zz <-utils::read.csv(track_file, header=TRUE)
    names(zz)[1] <- "hur_id"

    # subset by hurricane name
    tt <- zz[(zz$hur_id == hur_id), ]

    if (nrow(tt) == 0) {
        stop("Hurricane not in track file", call. = FALSE)
    }

    return(tt)
}

#' interpolate_hurricane_location_max_wind performs a linear interpolation
#' of hurricane latitude & longitude (degrees) and maximum sustained wind
#' speed (meters/second) using data from a hurricane track file and a 
#' specified time step.
#' @param tt data frame of track data
#' @param time_step time step (minutes)
#' @return a list containing vectors of year, Julian day, latitude, longitude, 
#'   and max wind speed
#' @noRd

interpolate_hurricane_location_max_wind <- function(tt, time_step) {
    tt_rows <- nrow(tt)

    # initialize vectors
    jd_vec <- vector()
    lat_vec <- vector()
    lon_vec <- vector()
    wmax_vec <- vector()

    # interpolate values for each segment of track.file
    for (i in 1:(tt_rows-1)) {
        new_rows <- round(1440*(tt$jd[i+1] - tt$jd[i])/time_step) + 1
    
        jd <- seq(from=tt$jd[i], to=tt$jd[i+1], length.out=new_rows)
        lat <- seq(from=tt$latitude[i], to=tt$latitude[i+1], length.out=new_rows)
        lon <- seq(from=tt$longitude[i], to=tt$longitude[i+1], length.out=new_rows)
        wmax <- seq(from=tt$wind_max[i], to=tt$wind_max[i+1], length.out=new_rows)

        # remove last element to avoid duplication
        jd <- jd[-length(jd)]
        lat <- lat[-length(lat)]
        lon <- lon[-length(lon)]
        wmax <- wmax[-length(wmax)]

        jd_vec <- append(jd_vec, jd)
        lat_vec <- append(lat_vec, lat)
        lon_vec <- append(lon_vec, lon)
        wmax_vec <- append(wmax_vec, wmax)
    }
  
    # add final row
    jd_vec <- append(jd_vec, tt$jd[tt_rows])
    lat_vec <- append(lat_vec, tt$lat[tt_rows])
    lon_vec <- append(lon_vec, tt$lon[tt_rows])
    wmax_vec <- append(wmax_vec, tt$wind_max[tt_rows])

    year <- as.numeric(substr(tt$date_time[1], 1, 4))
    yr_vec <- rep(year, times=length(jd_vec))

    return(list(yr_vec, jd_vec, lat_vec, lon_vec, wmax_vec))
}

#' estimate_range uses the Pythagorean equation to estimate the range 
#' (kilometers) from one point to another based on the latitude & longitude 
#' of each point. Note: overestimates range unless both points are on the 
#' same meridian.
#' @param lat1 latitude of first point (degrees)
#' @param lon1 longitude of first point (degrees)
#' @param lat2 latitude of second point (degrees)
#' @param lon2 longitude of second point (degrees)
#' @return range in kilometers
#' @noRd

estimate_range <- function(lat1, lon1, lat2, lon2) {
    R <- 6367 # radius of earth in kilometers (at latitude 45 degrees)
    d2r <- 0.017453292519943295  # pi / 180

    lat_avg <- d2r*(lat1 + lat2)/2
    x <- d2r*(lon1 - lon2)*cos(d2r*lat_avg)
    y <- d2r*(lat1 - lat2)
    range_est <- R * sqrt(x^2 + y^2)

    return(range_est)
}

#' calculate_range uses the Haversine formula to calculate the range 
#' (kilometers) from one point to another based on the latitude & longitude
#' of each point.
#' @param lat1 latitude of first point (degrees)
#' @param lon1 longitude of first point (degrees)
#' @param lat2 latitude of second point (degrees)
#' @param lon2 longitude of second point (degrees)
#' @return range in kilometers
#' @noRd

calculate_range <- function(lat1, lon1, lat2, lon2) {
    R <- 6367 # radius of earth in kilometers (at latitude 45 degrees)
    d2r <- 0.017453292519943295  # pi / 180

    # nearly same point
    if (abs(lat2 - lat1) < 0.000001 && abs(lon2 - lon1) < 0.000001) {
        rang <- 0
    
    } else {
        # date line
        if (lon1 > 90 && lon2 < -90) {
            lon2 <- lon2 + 360
        } else if (lon1 < -90 && lon2 > 90) {
            lon1 <- lon1 + 360
        }
        
        # convert degrees to radians
        rlat1 <- d2r*lat1
        rlat2 <- d2r*lat2
        rlon1 <- d2r*lon1
        rlon2 <- d2r*lon2

        A <- (sin((rlat2-rlat1)/2))^2 + cos(rlat1)*cos(rlat2)*(sin((rlon2-rlon1)/2))^2
        C <- 2 * atan2(sqrt(A), sqrt(1-A))
        rang <- R * C
    }

    return(rang)
}

#' calculate_bearing uses the Haversine formula to calculate the bearing 
#' (degrees) from one point to another based on the latitude & longitude 
#' of each point.
#' @param lat1 latitude of first point (degrees)
#' @param lon1 longitude of first point (degrees)
#' @param lat2 latitude of second point (degrees)
#' @param lon2 longitude of second point (degrees)
#' @return bearing in degrees
#' @noRd

calculate_bearing <- function(lat1, lon1, lat2, lon2) {
    R <- 6367 # radius of earth in kilometers (at latitude 45 degrees)
    d2r <- 0.017453292519943295  # pi / 180
    r2d <- 57.29577951308232  # 180 / pi

    # nearly same point
    if (abs(lat2 - lat1) < 0.000001 && abs(lon2 - lon1) < 0.000001) {
        bear <- 0
    
    } else {
        # date line
        if (lon1 > 90 && lon2 < -90) {
            lon2 <- lon2 + 360
        } else if (lon1 < -90 && lon2 > 90) {
            lon1 <- lon1 + 360
        }

        # same longitude
        if (lon1 == lon2) {
            if (lat1 > lat2) {
                bear <- 180
            } else {
                bear <- 0
            }
    
        # different longitude
        } else {
            # convert degrees to radians
            rlat1 <- d2r*lat1
            rlat2 <- d2r*lat2
            rlon1 <- d2r*lon1
            rlon2 <- d2r*lon2

            B2 <- atan2(sin(rlon2-rlon1)*cos(rlat2), cos(rlat1)*sin(rlat2) - sin(rlat1)*cos(rlat2)*cos(rlon2-rlon1))
      
            # convert radians to degrees
            B <- r2d*B2

            if (lon1 < lon2) {
                # quadrants I, IV
                bear <- B
            } else {  
                # quadrants II, III
                bear <- 360 + B
            }
        }
    }

    return(bear)
}

#' get_maximum_wind_speed returns the maximum sustained wind speed for
#' the specified hurricane.
#' @param hur_id hurricane id
#' @return maximum sustained wind speed (meters/second)
#' @noRd

get_maximum_wind_speed <- function(hur_id) {
    # get path
    hur_path <- get_path()

    # read hurricane track file
    track_file <- paste(hur_path, "/input/tracks.csv", sep="")
    check_file_exists(track_file)
    zz <-utils::read.csv(track_file, header=TRUE)
    names(zz)[1] <- "hur_id"

    # subset by hurricane name
    tt <- zz[(zz$hur_id == hur_id), ]

    # get maximum wind speed
    wmax <- max(tt$wind_max)

    return(wmax)
}

#' get_maximum_range estimates the range (kilometers) at which sustained wind
#' speeds are less than gale (17.5 meters/second).
#' @param wmax maximum sustained wind speed (meters/second)
#' @param rmw radius of maximum winds (kilometers)
#' @param s_par scaling parameter
#' @return range in kilometers
#' @noRd

get_maximum_range <- function(wmax, rmw, s_par) {
    rang <- rmw
    wspd <- 100

    while (wspd >= 17.5) {
        rang <- rang + 10
        x <- (rmw/rang)^s_par
        wspd <- wmax * sqrt(x * exp(1-x))
    }

    return(rang)
}

#' interpolate_hurricane_speed_bearing performs a linear interpolation of hurricane
#' forward speed (meters/second) and bearing (degrees) along a hurricane track based
#' on mid-segment values.
#' @param tt data frame of track values
#' @param jd_vec vector of Julian day values
#' @return a list containing vectors of hurricane speed & bearing
#' @noRd

interpolate_hurricane_speed_bearing <- function(tt, jd_vec) {
    tt_rows <- nrow(tt)
    vv_rows <- tt_rows - 1
    num <- length(jd_vec)

    # intialize vectors
    vv_jd <- rep(0, vv_rows)
    vv_spd <- rep(0, vv_rows)
    vv_bear <- rep(0, vv_rows)

    # calculate mid-segment hurricane speed & bearing
    for (i in (1:(tt_rows-1))) {
        hur_range <- calculate_range(tt$latitude[i], tt$longitude[i],
            tt$latitude[i+1], tt$longitude[i+1])
  
        hur_bear <- calculate_bearing(tt$latitude[i], tt$longitude[i],
            tt$latitude[i+1], tt$longitude[i+1])
    
        interval_sec <- (tt$jd[i+1] - tt$jd[i]) * 1440 * 60
    
        vv_jd[i] <- tt$jd[i] + (tt$jd[i+1] - tt$jd[i])/2
        vv_spd[i] <- (1000*hur_range)/interval_sec
        vv_bear[i] <- hur_bear
    }
  
    # initialize vectors
    bear_vec <- rep(0, num)
    spd_vec <- rep(0, num)

    # interpolate hurricane speed & bearing for each segment
    for (i in 1:(vv_rows+1)) {
        # before mid-point of 1st segment
        if (i == 1) {
            index <- which(jd_vec <= vv_jd[1])
     
            bear_vec[index] <- vv_bear[1]
            spd_vec[index] <- vv_spd[1]
      
        # interpolate between mid-points
        } else if (i <= vv_rows) {
            index <- which((jd_vec > vv_jd[i-1]) & (jd_vec <= vv_jd[i]))
            new_rows <- length(index)

            # bearing
            b1 <- vv_bear[i-1]
            b2 <- vv_bear[i]

            if (b2 - b1 > 180) {
                b1 <- b1 + 360
            } else if (b1 - b2 > 180) {
                b2 <- b2 + 360
            }
        
            bear <- seq(from=b1, to=b2, length.out=new_rows)        

            # speed
            spd <- seq(from=vv_spd[i-1], to=vv_spd[i], length.out=new_rows)
    
            for (j in 1:new_rows) {
                bear_vec[index[j]] <- bear[j]
                spd_vec[index[j]]  <- spd[j]
            }

        # after mid-point of last segment
        } else {
            index <- which(jd_vec > vv_jd[vv_rows])

            bear_vec[index] <- vv_bear[vv_rows]
            spd_vec[index] <- vv_spd[vv_rows]
        }
    }  
    
    # adjust bearing as needed
    for (i in 1:length(bear_vec)) {
        if (bear_vec[i] < 0) {
            bear_vec[i] <- bear_vec[i] + 360
        } else if (bear_vec[i] > 360) {
            bear_vec[i] <- bear_vec[i] - 360
        }
    }

    return(list(spd_vec, bear_vec))
}

#' calculate_site_range_bearing calculates the range (kilometers) and bearing
#' (degrees) from a site to the hurricane center.
#' @param lat_vec vector of hurricane latitudes (degrees)
#' @param lon_vec vector of hurricane longitudes (degrees)
#' @param site_latitude latitude of site (degrees)
#' @param site_longitude longitude of site (degrees)
#' @return a list containing vectors of range & bearing
#' @noRd

calculate_site_range_bearing <- function(lat_vec, lon_vec, site_latitude, site_longitude) {
    num <- length(lat_vec)

    # initialize vectors
    srange <- rep(0, num)
    sbear  <- rep(0, num)
  
    for (i in 1:num) {
        srange[i] <- calculate_range(site_latitude, site_longitude, 
            lat_vec[i], lon_vec[i])
    
        sbear[i] <- calculate_bearing(site_latitude, site_longitude, 
            lat_vec[i], lon_vec[i])
    }
  
    return(list(srange, sbear))
}

#' calculate_wind_direction calculates the wind direction (degrees) at the
#' specified site.
#' @param hurr_lat latitude of hurricane (degrees)
#' @param site_bear bearing from site to hurricane center (degrees)
#' @param inflow_angle cross-isobar inflow angle (degrees)
#' @return calculated wind direction in degrees
#' @noRd

calculate_wind_direction <- function (hur_lat, site_bear, inflow_angle) {
    # northern hemisphere: tangent minus inflow angle
    if (hur_lat > 0) {
        wind_dir <- site_bear - 90 - inflow_angle
        if (wind_dir < 0) {
            wind_dir <- wind_dir + 360
        }
  
    # southern hemisphere: tangent plus inflow angle
    } else {
        wind_dir <- site_bear + 90 + inflow_angle
        if (wind_dir > 360) {
            wind_dir <- wind_dir - 360
        }
    }

    return(wind_dir)
}

#' calculate_wind_speed calculates the sustained wind speed (meters/second) at
#' the specified site.
#' @param site_bear bearing from site to hurricane center (degrees)
#' @param site_range range from site to hurricance center (kilometers)
#' @param hur_lat latitude of hurricane (degrees)
#' @param hur_bear hurricane bearing (degrees)
#' @param hur_spd hurricane speed (meters/second)
#' @param wind_max maximum sustained wind speed (meters/second)
#' @param rmw radius of maximum winds (kilometers)
#' @param s_par scaling parameter
#' @param asymmetry_factor asymmetry factor
#' @param friction_factor friction factor
#' @return calculated sustained wind speed (meters/second)
#' @noRd

calculate_wind_speed <- function (site_bear, site_range, hur_lat, hur_bear, hur_spd, 
    wind_max, rmw, s_par, asymmetry_factor, friction_factor) {
  
    # hurricane eye (avoid division by zero)
    if (site_range == 0) {
        wind_spd <- 0 

    } else {
        # northern hemisphere: clockwise angle from path
        if (hur_lat > 0) {
            T <- site_bear - hur_bear + 180

        # southern hemisphere: counterclockwise angle from path
        } else {
            T <- site_bear - hur_bear
        }

        X <- (rmw / site_range)^s_par

        # sustained wind speed at radius of maximum wind (rmw)
        Z <- wind_max - hur_spd * asymmetry_factor * (1 - sin(T * pi/180))/2
    
        if (Z < 0) {
            Z <- 0
        }
      
        # sustained wind speed at site
        wind_spd <- Z * sqrt(X * exp(1 - X))

        # adjust for land or water
        wind_spd <- wind_spd * friction_factor
    }

    return(wind_spd)
}

#' calculate_wind_gust calculates the wind gust speed (meters/second) from 
#' the sustained wind speed (meters/second) and the gust factor.
#' @param wind_spd sustained wind speed (meters/second)
#' @param gust_factor gust factor (meters/second)
#' @return wind gust speed (meters/second)
#' @noRd

calculate_wind_gust <- function (wind_spd, gust_factor) {
    gust_spd <- gust_factor * wind_spd
  
    return(gust_spd)
}

#' calculate_enhanced_fujita_scale returns the enhanced Fujita scale value
#' (0 to 5; -1 = no damage) based on the wind gust speed (meters/second).
#' @param gust_spd wind gust speed (meters/second)
#' @return enhanced Fujita scale value
#' @noRd

calculate_enhanced_fujita_scale <- function (gust_spd) {
    # get enhanced Fujita wind speeds
    ef <- get_fujita_wind_speeds()

    ef0 <- ef[[1]]
    ef1 <- ef[[2]]
    ef2 <- ef[[3]]
    ef3 <- ef[[4]]
    ef4 <- ef[[5]]
    ef5 <- ef[[6]]

    if (gust_spd < ef0) {
        ef_sca <- -1
    } else if (gust_spd < ef1) {
        ef_sca <- 0
    } else if (gust_spd < ef2) {
        ef_sca <- 1
    } else if (gust_spd < ef3) {
        ef_sca <- 2
    } else if (gust_spd < ef4) {
        ef_sca <- 3
    } else if (gust_spd < ef5) {
        ef_sca <- 4
    } else {
        ef_sca <- 5
    }
    
    return(ef_sca)
}

#' calculate_wind_speed_direction calculates the wind speed (meters/second), 
#' gust speed (meters/second), wind direction (degrees), and enhanced Fujita 
#' scale wind damage at a site.
#' @param sbear_vec vector of site bearings (degrees)
#' @param srange_vec vector of site ranges (kilometers)
#' @param lat_vec vector of hurricane latitudes (degrees)
#' @param bear_vec vector of hurricane bearings (degrees)
#' @param spd_vec vector of hurricane forward speeds (meters/second)
#' @param wmax_vec vector of maximum sustained wind speeds (meters/second)
#' @param inflow_angle cross-isobar inflow angle (degrees)
#' @param rmw radius of maximum wind (kilometers)
#' @param s_par scaling parameter
#' @param asymmetry_factor asymmetry factor
#' @param friction_factor friction factor
#' @param gust_factor gust factor
#' @return a list containing vectors of wind speed, gust speed, wind direction, 
#'   and enhanced Fujita value
#' @noRd

calculate_wind_speed_direction <- function(sbear_vec, srange_vec, lat_vec, 
    bear_vec, spd_vec, wmax_vec, inflow_angle, rmw, s_par, asymmetry_factor, 
    friction_factor, gust_factor) {
 
    num <- length(sbear_vec)

    # initialize vectors
    wspd_vec <- rep(0, num)
    gspd_vec <- rep(0, num)
    wdir_vec <- rep(0, num)
    ef_vec   <- rep(0, num)

    for (i in 1:num) {
        # wind speed
        wspd_vec[i] <- calculate_wind_speed(sbear_vec[i], srange_vec[i], lat_vec[i], 
            bear_vec[i], spd_vec[i], wmax_vec[i], rmw, s_par, asymmetry_factor, 
            friction_factor)
  
        # gust speed
        gspd_vec[i] <- calculate_wind_gust(wspd_vec[i], gust_factor)
  
        # wind direction
        wdir_vec[i] <- calculate_wind_direction (lat_vec[i], sbear_vec[i], inflow_angle)
  
        # enhanced Fujita scale
        ef_vec[i] <- calculate_enhanced_fujita_scale(gspd_vec[i])
    }

    return(list(wspd_vec, gspd_vec, wdir_vec, ef_vec))
}

#' get_standard_date_time creates a vector of standard datetimes in the 
#' format YYYY-MM-DDThh:mm from vectors of years and Julian days.
#' @param yr_vec vector of years
#' @param jd_vec vector of Julian days
#' @return vector of standard datetimes
#' @noRd

get_standard_date_time <- function(yr_vec, jd_vec) {
    # get integer & fraction of Julian date
    jd_int_vec <- trunc(jd_vec)
    jd_frac_vec <- jd_vec - jd_int_vec

    # get date in standard format
    date_vec <- as.Date(jd_int_vec - 1, origin=paste(yr_vec, "-01-01", sep=""))

    # get hours & minutes
    min_tot_vec <- round(jd_frac_vec * 1440)
    hour_vec <- trunc(min_tot_vec / 60)
    min_vec <- round(min_tot_vec - hour_vec * 60)

    # convert numbers to strings
    hh_vec <- sprintf("%02d", hour_vec)
    mm_vec <- sprintf("%02d", min_vec)

    # add column for datetime in standard format
    dt_vec <- paste(as.character(date_vec), "T", hh_vec, ":", mm_vec, sep="")

    return(dt_vec)
}

#' get_peak_values returns a data frame of peak values for a given
#' hurricane and site.
#' @param hur_id hurricane id
#' @param site_name name of site
#' @param dt_vec vector of datetime values
#' @param wdir_vec vector of wind direction values (degrees)
#' @param wspd_vec vector of wind speed values (meters/second)
#' @param gspd_vec vector of gust speed values (meters/second)
#' @param efsca_vec vector of enhanced Fujita scale values
#' @return data frame of peak values
#' @noRd

get_peak_values <- function(hur_id, site_name, dt_vec, wdir_vec, wspd_vec, 
    gspd_vec, efsca_vec) {
  
    # get time step in minutes
    h1 <- as.integer(substr(dt_vec[1], 12, 13))
    m1 <- as.integer(substr(dt_vec[1], 15, 16))
    t1 <- h1 * 60 + m1

    h2 <- as.integer(substr(dt_vec[2], 12, 13))
    m2 <- as.integer(substr(dt_vec[2], 15, 16))
    t2 <- h2 * 60 + m2

    time_step <- t2 - t1

    # get peak wind
    index <- which.max(wspd_vec)

    dt <- dt_vec[index]
    wdir <- wdir_vec[index]
    wspd <- wspd_vec[index]
    gspd <- gspd_vec[index]
    efsca <- efsca_vec[index]

    # get wind duration in hours
    ef0 <- sum(efsca_vec >= 0) * time_step/60
    ef1 <- sum(efsca_vec >= 1) * time_step/60
    ef2 <- sum(efsca_vec >= 2) * time_step/60
    ef3 <- sum(efsca_vec >= 3) * time_step/60
    ef4 <- sum(efsca_vec >= 4) * time_step/60
    ef5 <- sum(efsca_vec >= 5) * time_step/60

    # create data fame of peak values
    kk <- data.frame(site_name, hur_id, dt, wdir, wspd, gspd, efsca, ef0, ef1, ef2, 
        ef3, ef4, ef5)

    return(kk)
}

#' get_regional_peak_wind calculates peak values for wind speed (meters/second), 
#' enhanced Fujita scale, wind direction (degrees), cardinal wind direction (1-8),
#' and duration of EF0, EF1, EF2, EF3, EF4, and EF5 winds (minutes) for a given 
#  hurricane over a region. Results are returned in a raster brick.
#' @param hur_id hurricane id
#' @param lat_vec vector of hurricane latitudes (degrees)
#' @param lon_vec vector of hurricane longitudes (degrees)
#' @param wmax_vec vector of maximum sustained wind speeds (meters/second)
#' @param bear_vec vector of hurricane bearings (degrees)
#' @param spd_vec vector of hurricane forward speeds (meters/second)
#' @param width whether to use width parameters for the specified hurricane
#' @param time_step time step (minutes)
#' @param water whether to calculate values over water
#' @param msg whether to use message to display progress
#' @return a raster brick containing 6 raster layers
#' @noRd

get_regional_peak_wind <- function(hur_id, lat_vec, lon_vec, wmax_vec, bear_vec, 
    spd_vec, width, time_step, water, msg=TRUE) {
  
    # get path
    hur_path <- get_path()

    # get number of rows
    num <- length(lat_vec)

    # read land-water file
    land_water_file <- paste(hur_path, "/input/land_water.tif", sep="")
    check_file_exists(land_water_file)
    land_water <- raster::raster(land_water_file)

    # get regional values
    nrows <- dim(land_water)[1]
    ncols <- dim(land_water)[2]

    lat_min <- raster::extent(land_water)[3]
    lat_max <- raster::extent(land_water)[4]
    cell_y <- (lat_max - lat_min)/nrows 

    lon_min <- raster::extent(land_water)[1]
    lon_max <- raster::extent(land_water)[2]
    cell_x <- (lon_max - lon_min)/ncols

    # create arrays for peak values
    ss <- matrix(0, nrows, ncols)  # wind speed (m/s)
    ff <- matrix(0, nrows, ncols)  # enhanced Fujita scale
    dd <- matrix(0, nrows, ncols)  # wind direction (degrees)
    cc <- matrix(0, nrows, ncols)  # cardinal wind direction (1-8)
    f0 <- matrix(0, nrows, ncols)  # duration of EF0 winds (minutes)
    f1 <- matrix(0, nrows, ncols)  # duration of EF1 winds (minutes)
    f2 <- matrix(0, nrows, ncols)  # duration of EF2 winds (minutes)
    f3 <- matrix(0, nrows, ncols)  # duration of EF3 winds (minutes)
    f4 <- matrix(0, nrows, ncols)  # duration of EF4 winds (minutes)
    f5 <- matrix(0, nrows, ncols)  # duration of EF5 winds (minutes)

    xx <- matrix(0, nrows, ncols)  # floating point wind speed (m/s)

    # create matrix from raster
    land_water_matrix <- raster::as.matrix(land_water)
  
    # read parameters file
    pars <- read_parameter_file(hur_id, width)
    rmw <- pars[1]
    s_par <- pars[2]

    # get fixed model parameters by cover type (water=1, land=2)
    asymmetry <- c(get_fixed_model_parameters(1)[1], get_fixed_model_parameters(2)[1])
    inflow <- c(get_fixed_model_parameters(1)[2], get_fixed_model_parameters(2)[2])
    friction <- c(get_fixed_model_parameters(1)[3], get_fixed_model_parameters(2)[3])
    gust <- c(get_fixed_model_parameters(1)[4], get_fixed_model_parameters(2)[4])

    # get maximum wind speed over track
    wmax_track <- get_maximum_wind_speed(hur_id)
  
    # get maximum range for gale winds
    range_maximum <- get_maximum_range(wmax_track, rmw, s_par)

    # record total elapsed time
    start_time <- Sys.time()

    # calculate peak wind speed & direction and gale & hurricane duration for each location
    for (i in 1:nrows) {
        for (j in 1:ncols) {
            # get cover type from land_water layer
            cover_type <- land_water_matrix[nrows-i+1, j]

            if (cover_type == 2 || water == TRUE) {
                # get site latitude & longitude
                site_latitude <- lat_min + (i - 0.5)*cell_y
                site_longitude <- lon_min + (j - 0.5)*cell_x

                # get fixed parameter values
                asymmetry_factor <- asymmetry[cover_type]
                inflow_angle <- inflow[cover_type]
                friction_factor <- friction[cover_type]
                gust_factor <- gust[cover_type]

                for (k in 1:num) {
                    hur_latitude  <- lat_vec[k]
                    hur_longitude <- lon_vec[k]
  
                    # site range
                    site_range <- calculate_range(site_latitude, site_longitude, 
                        hur_latitude, hur_longitude)

                    # skip if too far away
                    if (site_range < range_maximum) {
                        # site bearing
                        site_bear <- calculate_bearing(site_latitude, site_longitude,
                            hur_latitude, hur_longitude)

                        # wind speed (m/s)
                        wspd <- calculate_wind_speed(site_bear, site_range, hur_latitude, 
                            bear_vec[k], spd_vec[k], wmax_vec[k], rmw, s_par, asymmetry_factor, 
                            friction_factor)

                        # update values if gale or higher
                        if (wspd >= 17.5) {
                            # enhanced Fujita scale
                            gspd <- calculate_wind_gust(wspd, gust_factor)
                            fsca <- calculate_enhanced_fujita_scale(gspd)

                            # update duration (minutes)
                            if (fsca >= 0) {
                                f0[(nrows-i+1), j] <- f0[(nrows-i+1), j] + time_step
                            }
                            if (fsca >= 1) {
                                f1[(nrows-i+1), j] <- f1[(nrows-i+1), j] + time_step
                            }
                            if (fsca >= 2) {
                                f2[(nrows-i+1), j] <- f2[(nrows-i+1), j] + time_step
                            }
                            if (fsca >= 3) {
                                f3[(nrows-i+1), j] <- f3[(nrows-i+1), j] + time_step
                            }
                            if (fsca >= 4) {
                                f4[(nrows-i+1), j] <- f4[(nrows-i+1), j] + time_step
                            }
                            if (fsca >= 5) {
                                f5[(nrows-i+1), j] <- f5[(nrows-i+1), j] + time_step
                            }

                            # update peak peak_values
                            if (xx[(nrows-i+1), j] < wspd) {
                                xx[(nrows-i+1), j] <- wspd

                                ss[(nrows-i+1), j] <- as.integer(round(wspd))

                                # wind direction (degrees)
                                wdir <- calculate_wind_direction(hur_latitude, site_bear, inflow_angle)
                                dd[(nrows-i+1), j] <- as.integer(round(wdir))
                            }
                        }
                    }
                }
            }   
        }
      
        # report progress
        if (msg == TRUE) {
            x <- round(i*100/nrows)
            if (x %% 10 == 0) {
                message(paste("\r", x, "%"), appendLF=FALSE)
            }
        }
    }
  
    # calculate other values
    for (i in 1:nrows) {
        for (j in 1:ncols) {
            # get cover type from land_water layer
            cover_type <- land_water_matrix[nrows-i+1, j]

            if (cover_type == 2 || water == TRUE) {
                wspd <- xx[(nrows-i+1), j]

                # update values if gale or higher
                if (wspd >= 17.5) {
                    # get fixed parameter values     
                    gust_factor <- gust[cover_type]

                    # enhanced Fujita scale
                    gspd <- calculate_wind_gust(wspd, gust_factor)
                    fsca <- calculate_enhanced_fujita_scale(gspd)
                    ff[(nrows-i+1), j] <- fsca + 2

                    # cardinal wind direction (1 = north, 2 = northeast, etc)
                    wdir <- dd[(nrows-i+1), j]
                    cdir <- floor((wdir+22.5)/45) + 1
                    if (cdir > 8) {
                        cdir <- 1
                    }
                    cc[(nrows-i+1), j] <- cdir
                }
            }
        }
    }

    # add a zero value so wind compass colors match for water and no water
    cc[1, ncols] <- 0

    # create raster layers
    ss_raster <- raster::raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, 
        ymn=lat_min, ymx=lat_max, vals=ss)
  
    ff_raster <- raster::raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, 
        ymn=lat_min, ymx=lat_max, vals=ff)
  
    dd_raster <- raster::raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, 
        ymn=lat_min, ymx=lat_max, vals=dd)
  
    cc_raster <- raster::raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, 
        ymn=lat_min, ymx=lat_max, vals=cc)

    f0_raster <- raster::raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, 
        ymn=lat_min, ymx=lat_max, vals=f0)

    f1_raster <- raster::raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, 
        ymn=lat_min, ymx=lat_max, vals=f1)

    f2_raster <- raster::raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, 
        ymn=lat_min, ymx=lat_max, vals=f2)

    f3_raster <- raster::raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, 
        ymn=lat_min, ymx=lat_max, vals=f3)

    f4_raster <- raster::raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, 
        ymn=lat_min, ymx=lat_max, vals=f4)

    f5_raster <- raster::raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, 
        ymn=lat_min, ymx=lat_max, vals=f5)

    # create raster brick
    hur_brick <- raster::brick(ss_raster, ff_raster, dd_raster, cc_raster, f0_raster, 
        f1_raster, f2_raster, f3_raster, f4_raster, f5_raster)

    # report elapsed time
    if (msg == TRUE) {
        elapsed_time <- format_time_difference_hms(start_time, Sys.time())
        message(paste("\r", elapsed_time), appendLF=FALSE)
    }

    return(hur_brick)
}

#' get_regional_datetime calculates wind speed (meters/second), enhanced 
#' Fujita scale, wind direction (degrees), and cardinal wind direction 
#' (1-8) for a given hurricane over a region at a specified datetime. Results 
#' are returned in a raster brick with 4 layers.
#' @param hur_id hurricane id
#' @param lat hurricane latitude (degrees)
#' @param lon hurricane longitude (degrees)
#' @param wmax maximum sustained wind speed (meters/second)
#' @param bear hurricane bearing (degrees)
#' @param spd hurricane forward speed (meters/second)
#' @param width whether to use width parameters for the specified hurricane
#' @param water whether to calculate values over water
#' @return a raster brick containing 4 raster layers
#' @noRd

get_regional_datetime <- function(hur_id, lat, lon, wmax, bear, spd, width, 
    water) {
  
    # get path
    hur_path <- get_path()

    # read land-water file
    land_water_file <- paste(hur_path, "/input/land_water.tif", sep="")
    check_file_exists(land_water_file)
    land_water <- raster::raster(land_water_file)

    # get regional values
    nrows <- dim(land_water)[1]
    ncols <- dim(land_water)[2]

    lat_min <- raster::extent(land_water)[3]
    lat_max <- raster::extent(land_water)[4]
    cell_y <- (lat_max - lat_min)/nrows 

    lon_min <- raster::extent(land_water)[1]
    lon_max <- raster::extent(land_water)[2]
    cell_x <- (lon_max - lon_min)/ncols

    # create arrays for values
    ss <- matrix(0, nrows, ncols)  # wind speed (m/s)
    ff <- matrix(0, nrows, ncols)  # enhanced Fujita scale
    dd <- matrix(0, nrows, ncols)  # wind direction (degrees)
    cc <- matrix(0, nrows, ncols)  # cardinal wind direction (1-8)

    # create matrix from raster
    land_water_matrix <- raster::as.matrix(land_water)
  
    # read parameters file
    pars <- read_parameter_file(hur_id, width)
    rmw <- pars[1]
    s_par <- pars[2]

    # get fixed model parameters by cover type (water=1, land=2)
    asymmetry <- c(get_fixed_model_parameters(1)[1], get_fixed_model_parameters(2)[1])
    inflow <- c(get_fixed_model_parameters(1)[2], get_fixed_model_parameters(2)[2])
    friction <- c(get_fixed_model_parameters(1)[3], get_fixed_model_parameters(2)[3])
    gust <- c(get_fixed_model_parameters(1)[4], get_fixed_model_parameters(2)[4])

    # get maximum wind speed over track
    wmax_track <- get_maximum_wind_speed(hur_id)
  
    # get maximum range for gale winds
    range_maximum <- get_maximum_range(wmax_track, rmw, s_par)

    # record total elasped time
    start_time <- Sys.time()

    # calculate wind speed & direction for each location
    for (i in 1:nrows) {
        for (j in 1:ncols) {
            # get cover type from land_water layer
            cover_type <- land_water_matrix[nrows-i+1, j]

            if (cover_type == 2 || water == TRUE) {
                # get site latitude & longitude
                site_latitude <- lat_min + (i - 0.5)*cell_y
                site_longitude <- lon_min + (j - 0.5)*cell_x

                # get fixed parameter values
                asymmetry_factor <- asymmetry[cover_type]
                inflow_angle <- inflow[cover_type]
                friction_factor <- friction[cover_type]
                gust_factor <- gust[cover_type]

                hur_latitude  <- lat
                hur_longitude <- lon
  
                # site range
                site_range <- calculate_range(site_latitude, site_longitude, 
                    hur_latitude, hur_longitude)

                # skip if too far away
                if (site_range < range_maximum) {
                    # site bearing
                    site_bear <- calculate_bearing(site_latitude, site_longitude,
                        hur_latitude, hur_longitude)

                    # wind speed (m/s)
                    wspd <- calculate_wind_speed(site_bear, site_range, hur_latitude, 
                        bear, spd, wmax, rmw, s_par, asymmetry_factor, friction_factor)

                    # update values if gale or higher
                    if (wspd >= 17.5) {
                        # wind speed (m/s)
                        ss[(nrows-i+1), j] <- as.integer(round(wspd))

                        # wind direction (degrees)
                        wdir <- calculate_wind_direction(hur_latitude, site_bear, inflow_angle)
                        dd[(nrows-i+1), j] <- as.integer(round(wdir))
                    }
                }
            }
        }
      
        # report progress
        x <- round(i*100/nrows)
        if (x %% 10 == 0) {
            message(paste("\r", x, "%"), appendLF=FALSE)
        }
    }
  
    # calculate other values
    for (i in 1:nrows) {
        for (j in 1:ncols) {
            # get cover type from land_water layer
            cover_type <- land_water_matrix[nrows-i+1, j]

            if (cover_type == 2 || water == TRUE) {
                wspd <- ss[(nrows-i+1), j]

                # update values if gale or higher
                if (wspd >= 17.5) {
                    # get fixed parameter values     
                    gust_factor <- gust[cover_type]

                    # enhanced Fujita scale
                    gspd <- calculate_wind_gust(wspd, gust_factor)
                    fsca <- calculate_enhanced_fujita_scale(gspd)
                    ff[(nrows-i+1), j] <- fsca + 2

                    # cardinal wind direction (1 = north, 2 = northeast, etc)
                    wdir <- dd[(nrows-i+1), j]
                    cdir <- floor((wdir+22.5)/45) + 1
                    if (cdir > 8) {
                        cdir <- 1
                    }
                    cc[(nrows-i+1), j] <- cdir
                }
            }
        }
    }

    # add a zero value so wind compass colors match for water and no water
    cc[1, ncols] <- 0

    # create raster layers
    ss_raster <- raster::raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, 
        ymn=lat_min, ymx=lat_max, vals=ss)
  
    ff_raster <- raster::raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, 
        ymn=lat_min, ymx=lat_max, vals=ff)
  
    dd_raster <- raster::raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, 
        ymn=lat_min, ymx=lat_max, vals=dd)
  
    cc_raster <- raster::raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, 
        ymn=lat_min, ymx=lat_max, vals=cc)

    # create raster brick
    hur_brick <- raster::brick(ss_raster, ff_raster, dd_raster, cc_raster)

    # report elapsed time
    elapsed_time <- format_time_difference_hms(start_time, Sys.time())
    message(paste("\r", elapsed_time), appendLF=FALSE)

    return(hur_brick)
}

#' get_regional_summary_csv compiles regional results for all hurricanes.
#' Results are returned as a data frame of hurricane ids and maximum enhanced 
#' Fujita scale values.
#' @param inter_path path to intermediate results
#' @return a data frame of summary values
#' @noRd

get_regional_summary_csv <- function(inter_path) {
    # get path
    hur_path <- get_path()

    # read ids file
    ids_file <- paste(hur_path, "/input/ids.csv", sep="")
    check_file_exists(ids_file)
    ii <- utils::read.csv(ids_file, header=TRUE)
    names(ii)[1] <- "hur_id"
    ii_rows <- nrow(ii)

    # create data frame of peak Fujita values across region
    kk <- data.frame(hur_id=character(ii_rows), efmax=numeric(ii_rows))

    # record values for each hurricane
    for (i in 1:ii_rows) {
        # get hurricane name
        hur_id <- ii$hur_id[i]

        # read regional hurricane file in GeoTiff format
        hur_brick_file <- paste(inter_path, "/", hur_id, ".tif", sep="")
        check_file_exists(hur_brick_file)
        hur_brick <- raster::brick(hur_brick_file)

        # get enhanced Fujita scale layer
        ff_layer <- raster::subset(hur_brick, 2)  # enhanced Fujita scale

        # update peak Fujita value
        efmax <- raster::maxValue(ff_layer) - 2
    kk[i, ] <- c(hur_id, efmax)
    }

    return(kk)
}

#' get_regional_summary_tif compiles regional results for all hurricanes.
#' Results are returned as a raster brick with 7 layers representing the 
#' maximum Fujita value and the number of storms for each Fujita value.
#' @param inter_path path to intermediate results
#' @return raster brick of summary values
#' @noRd

get_regional_summary_tif <- function(inter_path) {
    # get path
    hur_path <- get_path()

    # read ids file
    ids_file <- paste(hur_path, "/input/ids.csv", sep="")
    check_file_exists(ids_file)
    ii <- utils::read.csv(ids_file, header=TRUE)
    names(ii)[1] <- "hur_id"
    ii_rows <- nrow(ii)

    # read land-water file
    land_water_file <- paste(hur_path, "/input/land_water.tif", sep="")
    check_file_exists(land_water_file)
    land_water <- raster::raster(land_water_file)
    land_water_matrix <- raster::as.matrix(land_water)

    # get regional values
    nrows <- dim(land_water)[1]
    ncols <- dim(land_water)[2]

    lat_min <- raster::extent(land_water)[3]
    lat_max <- raster::extent(land_water)[4]

    lon_min <- raster::extent(land_water)[1]
    lon_max <- raster::extent(land_water)[2]

    # create arrays for enhanced Fujita values
    efm <- matrix(0, nrows, ncols)
    ef0 <- matrix(0, nrows, ncols)
    ef1 <- matrix(0, nrows, ncols)
    ef2 <- matrix(0, nrows, ncols)
    ef3 <- matrix(0, nrows, ncols)
    ef4 <- matrix(0, nrows, ncols)
    ef5 <- matrix(0, nrows, ncols)

    # record values for each hurricane
    for (i in 1:ii_rows) {
        # get hurricane name
        hur_id <- ii$hur_id[i]

        # read regional hurricane file in GeoTiff format
        hur_brick_file <- paste(inter_path, "/", hur_id, ".tif", sep="")
        check_file_exists(hur_brick_file)
        hur_brick <- raster::brick(hur_brick_file)

        # get enhanced Fujita scale layer
        ff_layer <- raster::subset(hur_brick, 2)  # enhanced Fujita scale
        ff_layer_matrix <- raster::as.matrix(ff_layer)

        # update enhanced Fujita scale
        for (j in 1:nrows) {
            for (k in 1:ncols) {
            val <- ff_layer_matrix[j, k]

            if (val > 0 && efm[j, k] < val) {
                efm[j, k] <- val
            }

            if (val == 2) {
                ef0[j, k] <- ef0[j, k] + 1
      
            } else if (val == 3) {
                ef0[j, k] <- ef0[j, k] + 1
                ef1[j, k] <- ef1[j, k] + 1
      
            } else if (val == 4) {
                ef0[j, k] <- ef0[j, k] + 1
                ef1[j, k] <- ef1[j, k] + 1
                ef2[j, k] <- ef2[j, k] + 1

            } else if (val == 5) {
                ef0[j, k] <- ef0[j, k] + 1
                ef1[j, k] <- ef1[j, k] + 1
                ef2[j, k] <- ef2[j, k] + 1
                ef3[j, k] <- ef3[j, k] + 1
        
            } else if (val == 6) {
                ef0[j, k] <- ef0[j, k] + 1
                ef1[j, k] <- ef1[j, k] + 1
                ef2[j, k] <- ef2[j, k] + 1
                ef3[j, k] <- ef3[j, k] + 1
                ef4[j, k] <- ef4[j, k] + 1

            } else if (val == 7) {
                ef0[j, k] <- ef0[j, k] + 1
                ef1[j, k] <- ef1[j, k] + 1
                ef2[j, k] <- ef2[j, k] + 1
                ef3[j, k] <- ef3[j, k] + 1
                ef4[j, k] <- ef4[j, k] + 1          
                ef5[j, k] <- ef5[j, k] + 1          
            }
        }
        }
    }

    # create raster layers
    efm_raster <- raster::raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, 
        ymn=lat_min, ymx=lat_max, vals=efm)

    ef0_raster <- raster::raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, 
        ymn=lat_min, ymx=lat_max, vals=ef0)
  
    ef1_raster <- raster::raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, 
        ymn=lat_min, ymx=lat_max, vals=ef1)
  
    ef2_raster <- raster::raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, 
        ymn=lat_min, ymx=lat_max, vals=ef2)
  
    ef3_raster <- raster::raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, 
        ymn=lat_min, ymx=lat_max, vals=ef3)

    ef4_raster <- raster::raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, 
        ymn=lat_min, ymx=lat_max, vals=ef4)

    ef5_raster <- raster::raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, 
        ymn=lat_min, ymx=lat_max, vals=ef5)

    # create raster brick
    sum_brick <- raster::brick(efm_raster, ef0_raster, ef1_raster, ef2_raster, ef3_raster, ef4_raster, ef5_raster)

    return(sum_brick)
}

#' get_values_at_datetime returns a data frame of modeled values for the
#' specified datetime. The data frame includes hurricane latitude (degrees)
#' longitude (degrees), maximum sustained wind speed (meters/second), forward 
#' speed (meters/second), and bearing (degrees).
#' @param hur_id hurricane id
#' @param tt data frame of track data
#' @param dt datetime in the format YYYY-MM-DDThh:mm
#' @return a data frame of modeled values
#' @noRd

get_values_at_datetime <- function(hur_id, tt, dt) {
    # interpolate hurricane location & max wind speed
    mm <- interpolate_hurricane_location_max_wind(tt, time_step=1)
    yr_vec <- mm[[1]]
    jd_vec <- mm[[2]]
    lat_vec <- mm[[3]]
    lon_vec <- mm[[4]]
    wmax_vec <- mm[[5]]
    dt_vec <- get_standard_date_time(yr_vec, jd_vec)
  
    # get values for specified datetime
    index <- which(dt_vec == dt) 
  
    # abort if no match
    if (length(index) == 0) {
        stop("Datetime not found", call. = FALSE)
    }

    # interpolate hurricane speed & bearing
    mm <- interpolate_hurricane_speed_bearing(tt, jd_vec)
    spd_vec <- mm[[1]]
    bear_vec <- mm[[2]]

    # extract values
    lat <- lat_vec[index]
    lon <- lon_vec[index]
    wmax <- wmax_vec[index]
    spd <- spd_vec[index]
    bear <- bear_vec[index]

    # create data fame
    pp <- data.frame(lat, lon, wmax, spd, bear)

    return(pp)
}

#' get_track_lat_lon returns a data frame of track data for the specified hurricane
#' if the maximum enhanced Fujita value exceeds a specified value.
#' @param hur_id hurricane id
#' @param fuj_min minimum enhanced Fujita value
#' @param tt a data frame of track data (all hurricanes)
#' @param kk a data frame of summary data
#' @return a data frame of track data (this hurricane)
#' @noRd

get_track_lat_lon <- function(hur_id, fuj_min, tt, kk) {
    efmax <- kk[kk$hur_id == hur_id, "efmax"]

    if (length(efmax) == 0) {
        return(NULL)
  
    } else if (efmax < fuj_min) {
        return(NULL)
  
    } else {
        xx <- tt[tt$hur_id == hur_id, ]
        return(xx)
    }
}


### UTILITY FUNCTIONS #####################################

#' @title
#' Utility Functions
#' @description
#' hurrecon_set_path sets the path for the current set of model runs.
#' @param hur_path path for current set of model runs
#' @return no return value
#' @export
#' @rdname utility

hurrecon_set_path <- function(hur_path) {
    if (hur_path == "") {
        stop("Need to enter a path", call. = FALSE)

    } else if (dir.exists(hur_path) == FALSE) {
        stop("Path does not exist", call. = FALSE)
    }

    hur_env[["hur_path"]] <- hur_path
    message(paste("Path set to", hur_path))
}

#' @description
#' hurrecon_get_path returns the current path for a set of model runs.
#' @return current path
#' @export
#' @rdname utility

hurrecon_get_path <- function() {
    if (exists("hur_path", envir=hur_env)) {
        hur_path <- hur_env[["hur_path"]]

        message(hur_path)
        invisible(hur_path)

    } else {
        message("Path not set")
        invisible(NULL)
    }        
}

#' @description
#' hurrecon_create_land_water creates a land-water raster file in GeoTiff 
#' format from boundary files in shapefile format. The land-water file
#' (land_water.tif) is assumed to be aligned with lines of latitude and 
#' longitude.  Boundary files are assumed to be named boundary.* on the vector 
#' subdirectory. This function requires a reclasification file (reclassify.csv)
#' with 3 columns (from, to, becomes) on the vector subdirectory. For more details,
#' see documentation for the R raster and reclassify functions. The land-water
#' file is created on the input subdirectory.
#' @param nrows number of rows
#' @param ncols number of columns
#' @param xmn minimum longitude (degrees)
#' @param xmx maximum longitude (degrees)
#' @param ymn minimum latitude (degrees)
#' @param ymx maximum latitude (degrees)
#' @param save whether to save results to a GeoTiff file
#' @param hur_path path for current set of model runs
#' @return land-water raster
#' @export
#' @rdname utility

hurrecon_create_land_water <- function(nrows, ncols, xmn, xmx, ymn, ymx, 
    save=TRUE, hur_path=NULL) {
    
    # get path
    if (!is.null(hur_path)) {
        hurrecon_set_path(hur_path)
    } else {
        hur_path <- get_path()
    }

    # announcement
    message(paste("... Creating land-water ..."))

    # create new raster
    raster1 <- raster::raster(nrows=nrows, ncols=ncols, xmn=xmn, xmx=xmx, ymn=ymn, ymx=ymx, vals=0)

    # read vector boundary file
    boundaries_file <- paste(hur_path, "/vector/boundaries.shp", sep="")
    boundaries <- rgdal::readOGR(boundaries_file)

    # rasterize vector file
    raster2 <- raster::rasterize(boundaries, raster1)

    # read reclassify file
    reclassify_file <- paste(hur_path, "/vector/reclassify.csv", sep="")
    rcl <- utils::read.csv(reclassify_file)

    # convert to matrix
    rcl <- as.matrix(rcl, ncol=3, byrow=TRUE)

    # reclassify raster
    land_water <- raster::reclassify(raster2, rcl)

    # save to file
    if (save == TRUE) {
        land_water_file <- paste(hur_path, "/input/land_water.tif", sep="")
        rgdal::setCPLConfigOption("GDAL_PAM_ENABLED", "FALSE")
        raster::writeRaster(land_water, land_water_file, overwrite=TRUE)
    }

    # calculate cell dimensions in kilometers
    lat_avg <- (ymx + ymn)/2

    cell_height <- 111*(ymx-ymn)/nrows
    cell_width <- 111*(xmx-xmn)*cos(lat_avg*pi/180)/ncols

    message(paste("Cell height =", round(cell_height) , "kilometers"))
    message(paste("Cell width  =", round(cell_width), "kilometers"))

    invisible(land_water)
}

#' @description
#' hurrecon_reformat_hurdat2 reformats a HURDAT2 file from the National 
#' Hurricane Center for use with the HURRECON model. The input file is assumed
#' to be in space-delimited text format. The output file (hurdat2_tracks.csv)
#' contains full track information for each hurricane plus columns for standard 
#' datetime and Julian day with fraction. Hurricane IDs are reformatted to
#' facilitate sorting by year. The user must specify the path and the name
#' of the HURDAT2 file.
#' @param path path for input & output files
#' @param hurdat2_file name of HURDAT2 file
#' @param save whether to save results to a CSV file
#' @return a data frame of track data
#' @export
#' @rdname utility

hurrecon_reformat_hurdat2 <- function(path, hurdat2_file, save=TRUE) {
    # input & output files
    if (path[length(path)] != "/")  {
        path <- paste(path, "/", sep="")
    }
    
    hurdat2_file <- paste(path, hurdat2_file, sep="")
    check_file_exists(hurdat2_file)

    track_file <- paste(path, "hurdat2_tracks.csv", sep="")

    # announcement
    message(paste("... Reformatting Hurdat2 ..."))

    # read hurdat2 file
    file_in <- file(hurdat2_file)
    hurdat <- readLines(file_in)
    nlines <- length(hurdat)

    # close hurdat2 file
    close(file_in)

    # create data frame
    tracks <- data.frame(hur_id=character(nlines), name=character(nlines), date=character(nlines), 
        time=character(nlines), date_time=character(nlines), jd=numeric(nlines), 
        status=character(nlines), latitude=numeric(nlines), longitude=numeric(nlines), 
        wind_max=numeric(nlines))

    colnames(tracks) <- c("hur_id", "name", "date", "time", "date_time", "jd", "status", "latitude", 
        "longitude", "wind_max")

    # current line number in hurdat
    line_num <- 0
    # current row number in tracks
    tracks_index <- 0

    while (line_num < nlines) {
        # get hurricane id, name, and number of positions
        line_num <- line_num + 1
        row <- strsplit(hurdat[line_num], ",")[[1]] 
        hur_id <- get_hur_id(trimws(row[1]))
        name <- trimws(row[2])
        positions <- as.numeric(trimws(row[3]))

        # process observations
        for (i in 1:positions) {
            line_num <- line_num + 1
            row <- strsplit(hurdat[line_num], ",")[[1]]
            date <- trimws(row[1])
            time <- trimws(row[2])
            status <- trimws(row[4])

            lat <- trimws(row[5])
            latitude <- as.numeric(substr(lat, 1, nchar(lat)-1))

            lon <- trimws(row[6])
            longitude <- -as.numeric(substr(lon, 1, nchar(lon)-1))

            wind_max <- as.numeric(trimws(row[7]))

            # convert knots to meters per second
            wind_max <- round(0.514444 * wind_max, 1)

            # get peak wind
            if (i == 1) {
                wind_peak <- wind_max
            } else {
                if (wind_peak < wind_max) {
                    wind_peak <- wind_max
                }
            }

            date_time <- ""
            jd <- 0

            # add to tracks
            tracks_index <- tracks_index + 1
            tracks[tracks_index, ] <- c(hur_id, name, date, time, date_time, jd, status, latitude, longitude, wind_max)
        }

        # report progress
        x <- round(line_num*100/nlines)
        if (x %% 10 == 0) {
            message(paste("\r", x, "%"), appendLF=FALSE)
        }
    }

    # remove empty lines
    tracks <- tracks[(tracks$hur_id != ""), ]

    # add datetime
    tracks$date2 <- as.Date(paste(substr(tracks$date, 1, 4), "-", substr(tracks$date, 5, 6), "-", 
        substr(tracks$date, 7, 8), sep=""))
    tracks$hour <- as.numeric(tracks$time) %/% 100
    tracks$minute <- as.numeric(tracks$time) %% 100

    tracks$date_time <- paste(tracks$date2, "T", sprintf("%02d", tracks$hour), ":",
        sprintf("%02d", tracks$minute), sep="")
  
    # add Julian day
    tracks$jd <- as.numeric(format(tracks$date2, "%j")) + tracks$hour/24 + tracks$minute/1440

    # remove unnecessary columns
    tracks[ , c("date", "time", "date2", "hour", "minute")] <- list(NULL)

    # save to file
    if (save == TRUE) {
        utils::write.csv(tracks, track_file, row.names=FALSE)
    }

    # get number of storms
    ii <- tracks[ , c("hur_id", "name")]
    ii <- unique(ii)
    ii_rows <- nrow(ii)

    # display number of storms
    message(paste("\nNumber of storms =", ii_rows))
    message(paste("Number of observations =", tracks_index))

    invisible(tracks)
}

#' @description
#' hurrecon_extract_tracks extracts track data from an input track file
#' (input_tracks.csv) created from HURDAT2 using hurrecon_reformat_hurdat2
#' or created from other sources with the same file structure. The geographic 
#' window used to select hurricanes is set by the land-water file and is optionally
#' extended by the margin parameter. Selection begins by identifying all positions
#' in the window where winds reach or exceed hurricane speed (33 meters/second). 
#' If at least one such position exists, the track is extended to include one 
#' position before and one position after the first and last hurricane position 
#' in the window, if possible. If the resulting track contains at least two positions 
#' and the maximum sustained wind speed equals or exceeds wind_min, the track is 
#' included. For included storms, summary data are written to ids.csv, track data 
#' are written to tracks.csv, and track data for all positions are written to 
#' tracks_all.csv.
#' @param margin an optional extension of the geographic window set by the
#' land-water file (degrees)
#' @param wind_min the minimum value of maximum sustained wind speed 
#' (meters/second)
#' @param status whether to limit search to storms with hurricane status
#' @param save whether to save results to CSV files
#' @param hur_path path for current set of model runs
#' @return a list of three track-related data frames
#' @export
#' @rdname utility

hurrecon_extract_tracks <- function(margin=0, wind_min=33, status=TRUE, 
    save=TRUE, hur_path=NULL) {
    
    # get path
    if (!is.null(hur_path)) {
        hurrecon_set_path(hur_path)
    } else {
        hur_path <- get_path()
    }

    # announcement
    message(paste("... Extracting tracks ..."))

    # output files
    ids_file <- paste(hur_path, "/input/ids.csv", sep="")
    track_file <- paste(hur_path, "/input/tracks.csv", sep="")
    track_all_file <- paste(hur_path, "/input/tracks_all.csv", sep="")

    # read input tracks file
    input_track_file <- paste(hur_path, "/input/input_tracks.csv", sep="")
    check_file_exists(input_track_file)
    tt <- utils::read.csv(input_track_file, header=TRUE)
    tt_rows <- nrow(tt)

    # get ids
    ii <- tt[ , c("hur_id", "name")]
    ii <- unique(ii)
    ii_rows <- nrow(ii)

    # read land-water file
    land_water_file <- paste(hur_path, "/input/land_water.tif", sep="")
    check_file_exists(land_water_file)
    land_water <- raster::raster(land_water_file)

    # get window coordinates
    lon_min <- raster::extent(land_water)[1] - margin
    lon_max <- raster::extent(land_water)[2] + margin
 
    lat_min <- raster::extent(land_water)[3] - margin
    lat_max <- raster::extent(land_water)[4] + margin

    # create data frames
    ids <- data.frame(hur_id=character(ii_rows), name=character(ii_rows), 
        positions=numeric(ii_rows), wind_peak=numeric(ii_rows))

    tracks <- data.frame(hur_id=character(tt_rows), name=character(tt_rows), 
        date_time=character(tt_rows), jd=numeric(tt_rows), status=character(tt_rows), 
        latitude=numeric(tt_rows), longitude=numeric(tt_rows), wind_max=numeric(tt_rows))

    tracks_all <- data.frame(hur_id=character(tt_rows), date_time=character(tt_rows),
        latitude=numeric(tt_rows), longitude=numeric(tt_rows))

    colnames(ids) <- c("hur_id", "name", "positions", "wind_peak")

    colnames(tracks) <- c("hur_id", "name", "date_time", "jd", "status", "latitude", 
        "longitude", "wind_max")

    colnames(tracks_all) <- c("hur_id", "date_time", "latitude", "longitude")

    # subset each track
    ids_index <- 0
    tracks_index <- 0
    tracks_all_index <- 0

    for (i in 1:ii_rows) {
        # get hurricane id & name
        hur_id <- ii[i, "hur_id"]
        name <- ii[i, "name"]

        # check if in window
        if (status == TRUE) {
            index <- which(tt$hur_id == hur_id & tt$latitude >= lat_min & tt$latitude <= lat_max & tt$longitude >= lon_min & tt$longitude <= lon_max & tt$wind_max >= 33 & tt$status == "HU")
        } else {
            index <- which(tt$hur_id == hur_id & tt$latitude >= lat_min & tt$latitude <= lat_max & tt$longitude >= lon_min & tt$longitude <= lon_max & tt$wind_max >= 33)
        }
        
        index_all <- which(tt$hur_id == hur_id)

        # get start & end position
        if (length(index) > 0) {
            start_index <- min(index)

            if (start_index > 1) {
                if (tt$hur_id[start_index - 1] == hur_id) {
                    start_index <- start_index - 1
                }
            }

            end_index <- max(index)

            if (end_index < tt_rows) {
                if (tt$hur_id[end_index + 1] == hur_id) {
                    end_index <- end_index + 1
                }
            }

            # subset by start & end position
            xx <- tt[start_index:end_index, ]
            positions <- nrow(xx)
            wind_peak <- max(xx$wind_max)
 
            zz <- tt[index_all, ]
            zz <- zz[ , c("hur_id", "date_time", "latitude", "longitude")]

            # store id & tracks if at least 2 positions & exceeds minimum wind speed
            if (positions > 1 && wind_peak >= wind_min) {
                ids_index <- ids_index + 1
                ids[ids_index, ] <- c(hur_id, name, positions, wind_peak)
      
                for (j in 1:nrow(xx)) {
                    tracks_index <- tracks_index + 1
                    tracks[tracks_index, ] <- xx[j, ]   
                }

                for (j in 1:nrow(zz)) {
                    tracks_all_index <- tracks_all_index + 1
                    tracks_all[tracks_all_index, ] <- zz[j, ]
                }
            }
        }

        # report progress
        x <- round(i*100/ii_rows)
        if (x %% 10 == 0) {
            message(paste("\r", x, "%"), appendLF=FALSE)
        }
    }

    # remove empty lines
    ids <- ids[(ids$hur_id != ""), ]
    tracks <- tracks[(tracks$hur_id != ""), ]
    tracks_all <- tracks_all[(tracks_all$hur_id != ""), ]
  
    # save to files
    if (save == TRUE) {
        utils::write.csv(ids, ids_file, row.names=FALSE)
        utils::write.csv(tracks, track_file, row.names=FALSE)
        utils::write.csv(tracks_all, track_all_file, row.names=FALSE)
    }

    # display number of storms
    message(paste("\nNumber of storms =", nrow(ids)))
    message(paste("Number of observations =", nrow(tracks)))

    invisible(list(ids, tracks, tracks_all))
}


### MODELING FUNCTIONS ####################################

#' @title
#' Modeling Functions
#' @description
#' hurrecon_model_site calculates wind speed (meters/second), gust speed 
#' (meters/second), wind direction (degrees), and enhanced Fujita scale wind 
#' damage for a given hurricane and site. If width is TRUE, the radius of 
#' maximum wind (rmw) and scaling parameter (s_par) for this hurricane 
#' are used; otherwise values for ALL are used. If save is TRUE, results are 
#' saved to a CSV file on the site subdirectory.
#' @param hur_id hurricane id
#' @param site_name name of site
#' @param width whether to use width parameters for the specified hurricane
#' @param time_step time step (minutes)
#' @param msg whether to use message to display progress
#' @param save whether to save results to a CSV file
#' @param hur_path path for current set of model runs
#' @return a data frame of results
#' @export
#' @examples
#' hur_path <- system.file("", package="HurreconR", mustWork=TRUE)
#' hurrecon_model_site(hur_id="AL1935-03", site_name="Miami FL", time_step=60, 
#' msg=FALSE, save=FALSE, hur_path=hur_path)
#' @rdname modeling

hurrecon_model_site <- function(hur_id, site_name, width=FALSE, time_step=1, 
    msg=TRUE, save=TRUE, hur_path=NULL) { 

    # get path
    if (!is.null(hur_path)) {
        hurrecon_set_path(hur_path)
    } else {
        hur_path <- get_path()
    }

    # announcement
    if (msg == TRUE) {
        message(paste("... Modeling site ..."))
    }

    # record total elapsed time
    start_time <- Sys.time()

    # read sites file
    sites <- read_site_file(site_name)
    site_latitude <- sites[1]
    site_longitude <- sites[2]
    cover_type <- sites[3]

    # read parameters file
    pars <- read_parameter_file(hur_id, width)
    rmw <- pars[1]
    s_par <- pars[2]
  
    # get fixed parameters
    fixed <- get_fixed_model_parameters(cover_type)
    asymmetry_factor <- fixed[1]
    inflow_angle <- fixed[2]
    friction_factor <- fixed[3]
    gust_factor <- fixed[4]

    # read hurricane track file
    tt <- read_hurricane_track_file(hur_id)
  
    # interpolate hurricane location & max wind speed
    mm <- interpolate_hurricane_location_max_wind(tt, time_step)
    yr_vec <- mm[[1]]
    jd_vec <- mm[[2]]
    lat_vec <- mm[[3]]
    lon_vec <- mm[[4]]
    wmax_vec <- mm[[5]]
  
    # get number of rows
    mm_rows <- length(yr_vec)

    # interpolate hurricane speed & bearing
    mm <- interpolate_hurricane_speed_bearing(tt, jd_vec)
    spd_vec <- mm[[1]]
    bear_vec <- mm[[2]]
  
    # calculate range & bearing from site to hurricane center
    mm <- calculate_site_range_bearing(lat_vec, lon_vec, site_latitude, site_longitude)
    srange_vec <- mm[[1]]
    sbear_vec <- mm[[2]]
    
    # calculate wind speed, wind direction & enhanced Fujita scale at site
    mm <- calculate_wind_speed_direction(sbear_vec, srange_vec, lat_vec, bear_vec, 
        spd_vec, wmax_vec, inflow_angle, rmw, s_par, asymmetry_factor, friction_factor, 
        gust_factor)
    wspd_vec <- mm[[1]]
    gspd_vec <- mm[[2]]
    wdir_vec <- mm[[3]]
    ef_vec   <- mm[[4]]
  
    # get standard date & time
    dt_vec <- get_standard_date_time(yr_vec, jd_vec)
 
    # get constant parameters
    rmw_vec  <- rep(rmw, length=mm_rows)
    spar_vec <- rep(s_par, length=mm_rows)

    # create data frame
    mm <- data.frame(dt_vec, yr_vec, jd_vec, lat_vec, lon_vec, wmax_vec, bear_vec, spd_vec, 
        sbear_vec, srange_vec, rmw_vec, spar_vec, wdir_vec, wspd_vec, gspd_vec, ef_vec)

    colnames(mm) <- c("date_time", "year", "jd", "latitude", "longitude", "wind_max", 
        "hur_bear", "hur_spd", "site_bear", "site_range", "rmw", "s_par", "wind_dir", 
        "wind_spd", "gust_spd", "ef_sca")

    # display total elapsed time
    if (msg == TRUE) {
        message(paste(format_time_difference_ms(start_time, Sys.time()), "ms"))
    }
    
    # output
    if (save == TRUE) {
        # save modeled data to CSV file
        site_name2 <- gsub(" ", "_", site_name)
        modeled_file <- paste(hur_path, "/site/", hur_id, "_", site_name2, ".csv", sep="")
        utils::write.csv(mm, modeled_file, quote=FALSE, row.names=FALSE)
        
        if (msg == TRUE) {
            message(paste("Saving to", modeled_file))
        }
    }
  
    # return modeled data as data frame
    invisible(mm)
}

#' @description
#' hurrecon_model_site_all creates a table of peak values for all hurricanes
#' for a given site. If width is TRUE, the radius of maximum wind (rmw) and 
#' scaling parameter (s_par) specified for each hurricane is used; otherwise 
#' values for ALL are used. If save is TRUE, results are saved to a CSV file
#' on the site-all subdirectory.
#' @param site_name name of site
#' @param width whether to use width parameters for the specified hurricane
#' @param time_step time step (minutes)
#' @param save whether to save results to a CSV file
#' @param hur_path path for current set of model runs
#' @return a data frame of results
#' @export
#' @rdname modeling

hurrecon_model_site_all <- function(site_name, width=FALSE, time_step=1, 
    save=TRUE, hur_path=NULL) {

    # get path
    if (!is.null(hur_path)) {
        hurrecon_set_path(hur_path)
    } else {
        hur_path <- get_path()
    }

    # announcement
    message(paste("... Modeling site all ..."))

    # read ids file
    ids_file <- paste(hur_path, "/input/ids.csv", sep="")
    check_file_exists(ids_file)
    ii <- utils::read.csv(ids_file, header=TRUE)
    names(ii)[1] <- "hur_id"
    ii_rows <- nrow(ii)

    # initialize vectors
    snam_vec <- rep("", length=ii_rows)
    hid_vec <- rep("", length=ii_rows)
    dt_vec <- rep("", length=ii_rows)
    wdir_vec <- rep(0, length=ii_rows)
    wspd_vec <- rep(0, length=ii_rows)
    gspd_vec <- rep(0, length=ii_rows)
    efsca_vec <- rep(0, length=ii_rows)
    ef0_vec <- rep(0, length=ii_rows)
    ef1_vec <- rep(0, length=ii_rows)
    ef2_vec <- rep(0, length=ii_rows)
    ef3_vec <- rep(0, length=ii_rows)
    ef4_vec <- rep(0, length=ii_rows)
    ef5_vec <- rep(0, length=ii_rows)

    # record total elasped time
    start_time <- Sys.time()

    # get peak values for each hurricane
    for (i in 1:ii_rows) {
        # get hurricane name
        hur_id <- ii$hur_id[i]

        # get modeled output
        mm <- hurrecon_model_site(hur_id, site_name, width, time_step, msg=FALSE,
            save=FALSE)

        # get peak values
        pk <- get_peak_values(hur_id, site_name, mm$date_time, mm$wind_dir, mm$wind_spd, 
            mm$gust_spd, mm$ef_sca)
    
        snam_vec[i] <- as.character(pk$site_name[1])
        hid_vec[i] <- as.character(pk$hur_id[1])
        dt_vec[i] <- as.character(pk$dt[1])
        wdir_vec[i] <- pk$wdir[1]
        wspd_vec[i] <- pk$wspd[1]
        gspd_vec[i] <- pk$gspd[1]
        efsca_vec[i] <- pk$efsca[1]
        ef0_vec[i] <- pk$ef0[1]
        ef1_vec[i] <- pk$ef1[1]
        ef2_vec[i] <- pk$ef2[1]
        ef3_vec[i] <- pk$ef3[1]
        ef4_vec[i] <- pk$ef4[1]
        ef5_vec[i] <- pk$ef5[1]
    
        # report progress
        x <- round(i*100/ii_rows)
        if (x %% 10 == 0) {
            message(paste("\r", x, "%"), appendLF=FALSE)
        }
    }

    # create data frame for peak values
    peak_values <- data.frame(snam_vec, hid_vec, dt_vec, wdir_vec, wspd_vec, gspd_vec,
        efsca_vec, ef0_vec, ef1_vec, ef2_vec, ef3_vec, ef4_vec, ef5_vec)

    colnames(peak_values) <- c("site_name", "hur_id", "date_time", "wind_dir", "wind_spd", 
        "gust_spd", "ef_sca", "ef0", "ef1", "ef2", "ef3", "ef4", "ef5")

    elapsed_time <- format_time_difference_hms(start_time, Sys.time())
    message(paste("\r", elapsed_time), appendLF=FALSE)

    # output
    if (save == TRUE) {
        # save modeled data to CSV file
        site_name2 <- gsub(" ", "_", site_name)
        site_peak_file <- paste(hur_path, "/site-all/", site_name2, "_Peak_Values.csv", sep="")
        utils::write.csv(peak_values, site_peak_file, quote=FALSE, row.names=FALSE)

        message(paste("\nSaving to", site_peak_file))
    }
  
    # return modeled data as data frame
    invisible(peak_values)
}

#' @description
#' hurrecon_model_region calculates peak wind speed (meters/second), peak 
#' enhanced Fujita scale, peak wind direction (degrees), peak cardinal wind 
#' direction, and duration of EF0, EF1, EF2, EF3, EF4, and EF5 winds (minutes)
#' for a given hurricane over a region. If width is TRUE, the radius of maximum 
#' wind (rmw) and scaling parameter (s_par) specified for each hurricane is used; 
#' otherwise values for ALL are used. If time_step is NULL, the time step is 
#' calculated. If water is FALSE, results are calculated for land areas only. 
#' If save is TRUE, results are saved as a GeoTiff file on the region subdirectory.
#' @param hur_id hurricane id
#' @param width whether to use width parameters for the specified hurricane
#' @param time_step time step (minutes)
#' @param water whether to caculate results over water
#' @param msg whether to use message to display progress
#' @param save whether to save results to a GeoTiff file
#' @param hur_path path for current set of model runs
#' @return a brick of 6 rasters
#' @export
#' @rdname modeling

hurrecon_model_region <- function(hur_id, width=FALSE, time_step=NULL, water=FALSE, 
    msg=TRUE, save=TRUE, hur_path=NULL) {

    # get path
    if (!is.null(hur_path)) {
        hurrecon_set_path(hur_path)
    } else {
        hur_path <- get_path()
    }

    # announcement
    if (msg == TRUE) {
        message(paste("... Modeling region ..."))
    }
 
    # get time step if necessary
    if (is.null(time_step)) {
        time_step <- get_time_step()
    }

    if (msg == TRUE) {
        message(paste("Time step =", time_step, "minutes"))
    }

    # read hurricane track file
    tt <- read_hurricane_track_file(hur_id)

    # interpolate hurricane location & max wind speed
    mm <- interpolate_hurricane_location_max_wind(tt, time_step)
    jd_vec <- mm[[2]]
    lat_vec <- mm[[3]]
    lon_vec <- mm[[4]]
    wmax_vec <- mm[[5]]

    # interpolate hurricane speed & bearing
    mm <- interpolate_hurricane_speed_bearing(tt, jd_vec)
    spd_vec <- mm[[1]]
    bear_vec <- mm[[2]]

    # get modeled values over region
    hur_brick <- get_regional_peak_wind(hur_id, lat_vec, lon_vec, wmax_vec,
        bear_vec, spd_vec, width, time_step, water, msg)

    # output
    if (save == TRUE) {
        # save modeled values in a Geotiff file
        hur_tif_file = paste(hur_path, "/region/", hur_id, ".tif", sep="")
        rgdal::setCPLConfigOption("GDAL_PAM_ENABLED", "FALSE")
        raster::writeRaster(hur_brick, hur_tif_file, overwrite=TRUE)
    
        if (msg == TRUE) {
             message(paste("\nSaving to", hur_tif_file))
        }
    }
  
    # return modeled values as raster brick
    invisible(hur_brick)
}

#' @description
#' hurrecon_model_region_dt calculates wind speed (meters/second), enhanced
#' Fujita scale, wind direction (degrees), and cardinal wind direction for a
#' given hurricane over a region at a specified datetime. If width is TRUE, 
#' the radius of maximum wind (rmw) (kilometers) and scaling parameter (s_par) 
#  specified for the hurricane are used; otherwise values for ALL are used. 
#' If water is FALSE, results are calculated for land areas only. If save is 
#' TRUE, results are saved as a GeoTiff file on the region-dt subdirectory.
#' @param hur_id hurricane id
#' @param dt datetime in the format YYYY-MM-DDThh:mm
#' @param width whether to use width parameters for the specified hurricane
#' @param water whether to caculate results over water
#' @param save whether to save results to a GeoTiff file
#' @param hur_path path for current set of model runs
#' @return a brick of 4 rasters
#' @export
#' @rdname modeling

hurrecon_model_region_dt <- function(hur_id, dt, width=FALSE, water=FALSE, 
    save=TRUE, hur_path=NULL) {

    # get path
    if (!is.null(hur_path)) {
        hurrecon_set_path(hur_path)
    } else {
        hur_path <- get_path()
    }

    # announcement
    message(paste("... Modeling region dt ..."))

    # read hurricane track file
    tt <- read_hurricane_track_file(hur_id)

    # get values for specified datetime
    pp <- get_values_at_datetime(hur_id, tt, dt)

    # get modeled values over region
    hur_brick <- get_regional_datetime(hur_id, pp$lat[1], pp$lon[1], pp$wmax[1], 
        pp$bear[1], pp$spd[1], width, water)

    # output
    if (save == TRUE) {
        # save modeled values in a Geotiff file
        dt2 <- gsub(":", "", dt)
        hur_tif_file <- paste(hur_path, "/region-dt/", hur_id, " ", dt2, ".tif", sep="")
        rgdal::setCPLConfigOption("GDAL_PAM_ENABLED", "FALSE")
        raster::writeRaster(hur_brick, hur_tif_file, overwrite=TRUE)
    
        message(paste("\nSaving to", hur_tif_file))
    }
  
    # return modeled values as raster brick
    invisible(hur_brick)
}

#' @description
#' hurrecon_model_region_all calculates peak wind speed (meters/second), 
#' peak enhanced Fujita scale, peak wind direction (degrees), peak cardinal 
#' wind direction, duration of gale winds (minutes), and duration of hurricane
#' winds (minutes) over a region for all hurricanes. If width is TRUE, the 
#' radius of maximum wind (rmw) and scaling parameter (s_par) specified for 
#' each hurricane is used; otherwise values for ALL are used. If time_step is 
#' NULL, the time step is calculated. If water is FALSE, results are calculated 
#' for land areas only. If save is TRUE, intermediate results for each hurricane 
#' are saved as GeoTiff files on the region-all subdirectory, along with summary 
#' results for all hurricanes (summary.csv, summary.tif). If save is FALSE,
#' intermediate results are saved to the R session temporary directory.
#' @param width whether to use width parameters for the specified hurricane
#' @param time_step time step (minutes)
#' @param water whether to calculate results over water
#' @param save whether to save results to file
#' @param hur_path path for current set of model runs
#' @return a list containing a data frame and a raster brick of summary values
#' @export
#' @rdname modeling

hurrecon_model_region_all <- function(width=FALSE, time_step=NULL, water=FALSE, 
    save=TRUE, hur_path=NULL) {
  
    # get path
    if (!is.null(hur_path)) {
        hurrecon_set_path(hur_path)
    } else {
        hur_path <- get_path()
    }

    # get path for intermediate results
    if (save == TRUE) {
        inter_path <- paste(hur_path, "/region-all", sep="")

    } else {
        temp_path <- normalizePath(tempdir(), winslash = "/", mustWork = TRUE)
        inter_path <- paste(temp_path, "/region-all", sep="")

        if (!dir.exists(inter_path)) {
            dir.create(inter_path)
        }
    }

    # announcement
    message(paste("... Modeling region all ..."))

    # get time step if necessary
    if (is.null(time_step)) {
        time_step <- get_time_step()
    }

    message(paste("Time step =", time_step, "minutes"))

    # read ids file
    ids_file <- paste(hur_path, "/input/ids.csv", sep="")
    check_file_exists(ids_file)
    ii <- utils::read.csv(ids_file, header=TRUE)
    names(ii)[1] <- "hur_id"
    ii_rows <- nrow(ii)

    # record total elasped time
    start_time <- Sys.time()

    # get regional estimate for each hurricane
    for (i in 1:ii_rows) {
        # get hurricane name
        hur_id <- ii$hur_id[i]

        # report progress
        x <- round((i-1)*100/ii_rows)
        message(paste("\r", x, "%"), appendLF=FALSE)

        # get modeled values over region
        hur_brick <- hurrecon_model_region(hur_id, width, time_step, water, 
            msg=FALSE, save=FALSE)

        # save modeled values in a Geotiff file
        hur_tif_file <- paste(inter_path, "/", hur_id, ".tif", sep="")
        rgdal::setCPLConfigOption("GDAL_PAM_ENABLED", "FALSE")
        raster::writeRaster(hur_brick, hur_tif_file, overwrite=TRUE)
    }

    # get summary.csv file
    kk <- get_regional_summary_csv(inter_path)

    # get summary.tif file
    sum_brick <- get_regional_summary_tif(inter_path)

    # display total elapsed time
    elapsed_time <- format_time_difference_hms(start_time, Sys.time())
    message(paste("\r", elapsed_time), appendLF=FALSE)

    # save summary files
    if (save == TRUE) {
        peak_file <- paste(hur_path, "/region-all/summary.csv", sep="")
        utils::write.csv(kk, peak_file, row.names=FALSE)

        sum_brick_file <- paste(hur_path, "/region-all/summary.tif", sep="")
        rgdal::setCPLConfigOption("GDAL_PAM_ENABLED", "FALSE")
        raster::writeRaster(sum_brick, sum_brick_file, overwrite=TRUE)

        reg_all_dir <- paste(hur_path, "/region-all/", sep="")
        message(paste("\nSaving to", reg_all_dir))
    }

    # return a list of summary results
    invisible(list(kk, sum_brick))
}


### SUMMARIZING FUNCTIONS #################################

#' @title
#' Summarizing Functions
#' @description
#' hurrecon_summarize_land_water displays information about the current
#' land-water file (land_water.tif).
#' @param console whether to display results in console
#' @param hur_path path for current set of model runs
#' @return a string containing summary information
#' @export
#' @rdname summarizing

hurrecon_summarize_land_water <- function(console=TRUE, hur_path=NULL) {
    # get path
    if (!is.null(hur_path)) {
        hurrecon_set_path(hur_path)
    } else {
        hur_path <- get_path()
    }

    # announcement
    message(paste("... Summarizing land-water ..."))

    # read land-water file
    land_water_file <- paste(hur_path, "/input/land_water.tif", sep="")
    check_file_exists(land_water_file)
    land_water <- raster::raster(land_water_file)
    land_water_matrix <- raster::as.matrix(land_water)

    # get regional values
    nrows <- dim(land_water)[1]
    ncols <- dim(land_water)[2]

    ymn <- raster::extent(land_water)[3]
    ymx <- raster::extent(land_water)[4]
    xmn <- raster::extent(land_water)[1]
    xmx <- raster::extent(land_water)[2]

    # calculate cell dimensions in kilometers
    lat_avg <- (ymx + ymn)/2

    cell_height <- 111*(ymx-ymn)/nrows
    cell_width <- 111*(xmx-xmn)*cos(lat_avg*pi/180)/ncols

    # get default time step

    time_step <- get_time_step()

    st <- paste("Rows: ", nrows, "  Columns: ", ncols, "\n", sep="")
    st <- paste(st, "Latitude: ", round(ymn, 1), " to ", round(ymx, 1), " degrees\n", sep="")
    st <- paste(st, "Longitude: ", round(xmn, 1), " to ", round(xmx, 1), " degrees\n", sep="")
    st <- paste(st, "Cell height: ", round(cell_height), " kilometers\n", sep="")
    st <- paste(st, "Cell width: ", round(cell_width), " kilometers\n", sep="")
    st <- paste(st, "Time Step: ", time_step, " minutes\n", sep="")
  
    if (console == TRUE) {
        cat(st)
    }

    invisible(st)
}

#' @description
#' hurrecon_summarize_tracks displays information about the current ids file 
#' (ids.csv).
#' @param console whether to display results in console
#' @param hur_path path for current set of model runs
#' @return a string containing summary information
#' @export
#' @rdname summarizing

hurrecon_summarize_tracks <- function(console=TRUE, hur_path=NULL) {
    # get path
    if (!is.null(hur_path)) {
        hurrecon_set_path(hur_path)
    } else {
        hur_path <- get_path()
    }

    # announcement
    message(paste("... Summarizing tracks ..."))

    # read ids file
    ids_file <- paste(hur_path, "/input/ids.csv", sep="")
    check_file_exists(ids_file)
    ii <- utils::read.csv(ids_file, header=TRUE)
    ii_rows <- nrow(ii)

    positions_total <- sum(ii$positions)

    wind_peak_min <- min(ii$wind_peak)
    wind_peak_max <- max(ii$wind_peak)

    st <- paste("Number of storms = ", ii_rows, "\n", sep="")
    st <- paste(st, "Number of positions = ", positions_total, "\n", sep="")
    st <- paste(st, "Minimum peak wind = ", wind_peak_min, "m/s\n", sep="")
    st <- paste(st, "Maximum peak wind = ", wind_peak_max, "m/s\n", sep="")

    if (console == TRUE) {
        cat(st)
    }

    invisible(st)
}

#' @description
#' hurrecon_summarize_site displays peak values for a given hurricane
#' and site.
#' @param hur_id hurricane id
#' @param site_name name of site
#' @param console whether to display results in console
#' @param hur_path path for current set of model runs
#' @return a string containing summary information
#' @export
#' @rdname summarizing

hurrecon_summarize_site <- function(hur_id, site_name, console=TRUE, hur_path=NULL) {
    # get path
    if (!is.null(hur_path)) {
        hurrecon_set_path(hur_path)
    } else {
        hur_path <- get_path()
    }

    # announcement
    message(paste("... Summarizing site ..."))

    # read data
    site_name2 <- gsub(" ", "_", site_name)
    modeled_name <- paste(hur_id, site_name)
    modeled_file <- paste(hur_path, "/site/", hur_id, "_", site_name2, ".csv", sep="")
    check_file_exists(modeled_file)
    mm <- utils::read.csv(modeled_file, header=TRUE)

    # get peak values
    pk <- get_peak_values(hur_id, site_name, mm$date_time, mm$wind_dir, mm$wind_spd, 
        mm$gust_spd, mm$ef_sca)

    # display peak values

    st <- paste(modeled_name, "\n", sep="")
    st <- paste(st, "PEAK: ", as.character(pk$dt[1]), "\n", sep="")
    st <- paste(st, "Wind dir: ", round(pk$wdir[1], 0), " deg\n", sep="")
    st <- paste(st, "Wind spd: ", round(pk$wspd[1], 0), " m/s\n", sep="")
    st <- paste(st, "Gust spd: ", round(pk$gspd[1], 0), " m/s\n", sep="")

    if (pk$ef0[1] > 0) {
        st <- paste(st, "EF0: ", round(pk$ef0[1], 1), " hours\n", sep="")
    }
    if (pk$ef1[1] > 0) {
        st <- paste(st, "EF1: ", round(pk$ef1[1], 1), " hours\n", sep="")
    }
    if (pk$ef2[1] > 0) {
        st <- paste(st, "EF2: ", round(pk$ef2[1], 1), " hours\n", sep="")
    }
    if (pk$ef3[1] > 0) {
        st <- paste(st, "EF3: ", round(pk$ef3[1], 1), " hours\n", sep="")
    }
    if (pk$ef4[1] > 0) {
        st <- paste(st, "EF4: ", round(pk$ef4[1], 1), " hours\n", sep="")
    }
    if (pk$ef5[1] > 0) {
        st <- paste(st, "EF5: ", round(pk$ef5[1], 1), " hours\n", sep="")
    }

    if (console == TRUE) {
        cat(st)
    }

    invisible(st)
}


### PLOTTING FUNCTIONS ####################################

#' @title
#' Plotting Functions
#' @description
#' hurrecon_plot_site creates a time-series plot (wind speed, gust 
#' speed, or wind direction as a function of datetime) or a scatter 
#' plot (wind speed or gust speed as a function of wind direction) 
#' for a given hurricane and site. Optional start and end datetimes 
#' may be specified. X-variables: datetime or wind_direction. 
#' Y-variables: wind_speed, gust_speed, or wind_direction.
#' @param hur_id hurricane id
#' @param site_name name of site
#' @param start_datetime optional start datetime (YYYY-MM-DD hh:mm)
#' @param end_datetime optional end datetime (YYYY-MM-DD hh:mm)
#' @param xvar dependent variable
#' @param yvar independent variable
#' @param adjust whether to subtract 360 degrees from wind directions
#' greater than 180 degrees in scatter plot
#' @param legend_loc legend location
#' @param title optional title
#' @param hur_path path for current set of model runs
#' @return no return value
#' @export
#' @rdname plotting

hurrecon_plot_site <- function(hur_id, site_name, start_datetime='', 
    end_datetime='', xvar="datetime", yvar="wind_speed", adjust=FALSE,
    legend_loc="topright", title="", hur_path=NULL) {

    # get path
    if (!is.null(hur_path)) {
        hurrecon_set_path(hur_path)
    } else {
        hur_path <- get_path()
    }

    # announcement
    message(paste("... Plotting site ..."))

    # get enhanced Fujita wind speeds
    ef <- get_fujita_wind_speeds()

    ef0 <- ef[[1]]
    ef1 <- ef[[2]]
    ef2 <- ef[[3]]
    ef3 <- ef[[4]]
    ef4 <- ef[[5]]
    ef5 <- ef[[6]]

    # get enhanced Fujita colors
    ef_col <- get_fujita_colors()

    efx_col <- ef_col[[7]]
    ef0_col <- ef_col[[1]]
    ef1_col <- ef_col[[2]]
    ef2_col <- ef_col[[3]]
    ef3_col <- ef_col[[4]]
    ef4_col <- ef_col[[5]]
    ef5_col <- ef_col[[6]]
  
    # check legend location
    if (!check_legend_location(legend_loc)) {
        legend_loc <- "topright"
    }

    # read data
    site_name2 <- gsub(" ", "_", site_name)    
    modeled_file <- paste(hur_path, "/site/", hur_id, "_", site_name2, ".csv", sep="")
    check_file_exists(modeled_file)
    mm <- utils::read.csv(modeled_file, header=TRUE)
    mm_rows <- nrow(mm)

    # add datetime
    mm$date <- substr(mm$date_time, 1, 10)
    mm$time <- paste(substr(mm$date_time, 12, 16), ":00", sep="")
    mm$dt <- as.POSIXct(paste(mm$date, " ", mm$time, sep=""))

    # x variable
    if (xvar == "datetime") {
        plot_type <- "time_series"
        x_var <- "dt"
        x_label <- "Datetime (UTC)"
   
    } else if (xvar == "wind_direction") {
        plot_type <- "scatter_plot"
        x_var <- "wind_dir"
        x_label <- "Wind Direction (deg)"
  
    } else {
        stop("xvar must be datetime or wind_direction", call. = FALSE)
    }

    # y variable
    if (yvar == "wind_speed") {
        y_var <- "wind_spd"
        y_label <- "Wind Speed (m/s)"

    } else if (yvar == "gust_speed") {
        y_var <- "gust_spd"
        y_label <- "Gust Speed (m/s)"

    } else if (yvar == "wind_direction") {
        y_var <- "wind_dir"
        y_label <- "Wind Direction (deg)"

    } else {
        stop("yvar must be wind_speed, gust_speed, or wind_direction", call. = FALSE)
    }

    # adjust wind direction
    if (plot_type == "scatter_plot" && adjust == TRUE) {
        mm$wind_dir2 <- mm$wind_dir

        for (i in 1:mm_rows) {
            if (mm$wind_dir2[i] > 180) {
                mm$wind_dir2[i] <- mm$wind_dir2[i] - 360
            }
        }

        x_var <- "wind_dir2"
    }

    # subset by datetime range
    if (start_datetime != "") {
        sdate <- start_datetime
    } else {
        sdate <- mm$dt[1]
    }

    if (end_datetime != "") {
        edate <- end_datetime
    } else {
        edate <- mm$dt[mm_rows]
    }

    mm_plot <- mm[(mm$dt >= sdate & mm$dt <= edate), ]

    # get variable ranges
    rows <- nrow(mm_plot)

    values <- mm_plot[ , x_var]
    xmin <- min(values, na.rm=TRUE)
    xmax <- max(values, na.rm=TRUE)
    xlim <- c(xmin, xmax)
  
    values <- mm_plot[ , y_var]
    ymin <- min(values, na.rm=TRUE)
    ymax <- max(values, na.rm=TRUE)
    ylim <- c(ymin, ymax)

    # subset by enhanced Fujita value
    mm_plot_efx <- mm_plot[mm_plot$gust_spd < ef0, ]
    mm_plot_ef0 <- mm_plot[(mm_plot$gust_spd >= ef0 & mm_plot$gust_spd < ef1), ]
    mm_plot_ef1 <- mm_plot[(mm_plot$gust_spd >= ef1 & mm_plot$gust_spd < ef2), ]
    mm_plot_ef2 <- mm_plot[(mm_plot$gust_spd >= ef2 & mm_plot$gust_spd < ef3), ]
    mm_plot_ef3 <- mm_plot[(mm_plot$gust_spd >= ef3 & mm_plot$gust_spd < ef4), ]
    mm_plot_ef4 <- mm_plot[(mm_plot$gust_spd >= ef4 & mm_plot$gust_spd < ef5), ]
    mm_plot_ef5 <- mm_plot[(mm_plot$gust_spd >= ef5), ]

    gust_max <- max(mm_plot$gust_spd)

    # get title
    main_title <- title

    if (main_title == "") {
        main_title <- paste(hur_id, site_name)
    }

    # create plot
    oldpar <- graphics::par(no.readonly=TRUE)
    on.exit(graphics::par(oldpar))

    graphics::par(mar=c(5.1, 5.6, 4.1, 2.1))

    if (plot_type == "time_series") {
        plot(xaxt="n", type="n", xlim, ylim, cex.main=1.7, cex.lab=1.7, xlab=x_label, 
            ylab=y_label, main=main_title) 

        graphics::axis.POSIXct(1, mm_plot$dt, format="%m-%d %H")

    } else {
        plot(type="n", xlim, ylim, cex.main=1.7, cex.lab=1.7, xlab=x_label, 
        ylab=y_label, main=main_title)
    }

    graphics::points(mm_plot_efx[c(x_var, y_var)], pch=16, cex=1.0, col=efx_col)
    graphics::points(mm_plot_ef0[c(x_var, y_var)], pch=16, cex=1.0, col=ef0_col)
    graphics::points(mm_plot_ef1[c(x_var, y_var)], pch=16, cex=1.0, col=ef1_col)
    graphics::points(mm_plot_ef2[c(x_var, y_var)], pch=16, cex=1.0, col=ef2_col)
    graphics::points(mm_plot_ef3[c(x_var, y_var)], pch=16, cex=1.0, col=ef3_col)
    graphics::points(mm_plot_ef4[c(x_var, y_var)], pch=16, cex=1.0, col=ef4_col)
    graphics::points(mm_plot_ef5[c(x_var, y_var)], pch=16, cex=1.0, col=ef5_col)

    # create legend
    labs <- c("No damage")
    cols <- c(efx_col)

    if (gust_max >= ef0) {
        labs <- append(labs, "EF0 damage")
        cols <- append(cols, ef0_col)
    }
    if (gust_max >= ef1) {
        labs <- append(labs, "EF1 damage")
        cols <- append(cols, ef1_col)
    }
    if (gust_max >= ef2) {
        labs <- append(labs, "EF2 damage")
        cols <- append(cols, ef2_col)
    }
    if (gust_max >= ef3) {
        labs <- append(labs, "EF3 damage")
        cols <- append(cols, ef3_col)
    }
    if (gust_max >= ef4) {
        labs <- append(labs, "EF4 damage")
        cols <- append(cols, ef4_col)
    }
    if (gust_max >= ef5) {
        labs <- append(labs, "EF5 damage")
        cols <- append(cols, ef5_col)
    }

    graphics::legend(legend_loc, NULL, labs, cols, cex=0.7)
}

#' @description
#' hurrecon_plot_site_all creates a time-series plot of peak values 
#' for all hurricanes for a given site. Optional start and end years
#' may be specified. Variables to plot: wind_speed, gust_speed, or
#' wind_direction.
#' @param site_name name of site
#' @param start_year optional start year
#' @param end_year optional end year
#' @param var variable to plot
#' @param legend_loc legend location
#' @param title optional title
#' @param hur_path path for current set of model runs
#' @return no return value
#' @export
#' @rdname plotting

hurrecon_plot_site_all <- function(site_name, start_year='', end_year='', 
    var="wind_speed", legend_loc="topright", title="", hur_path=NULL) {

    # get path
    if (!is.null(hur_path)) {
        hurrecon_set_path(hur_path)
    } else {
        hur_path <- get_path()
    }

    # announcement
    message(paste("... Plotting site all ..."))

    # get enhanced Fujita wind speeds
    ef <- get_fujita_wind_speeds()

    ef0 <- ef[[1]]
    ef1 <- ef[[2]]
    ef2 <- ef[[3]]
    ef3 <- ef[[4]]
    ef4 <- ef[[5]]
    ef5 <- ef[[6]]

    # get enhanced Fujita colors
    ef_col <- get_fujita_colors()

    efx_col <- ef_col[[7]]
    ef0_col <- ef_col[[1]]
    ef1_col <- ef_col[[2]]
    ef2_col <- ef_col[[3]]
    ef3_col <- ef_col[[4]]
    ef4_col <- ef_col[[5]]
    ef5_col <- ef_col[[6]]

    # check legend location
    if (!check_legend_location(legend_loc)) {
        legend_loc <- "topright"
    }

    # read data
    site_name2 <- gsub(" ", "_", site_name)
    peak_file <- paste(hur_path, "/site-all/", site_name2, "_Peak_Values.csv", sep="")
    check_file_exists(peak_file)
    kk <- utils::read.csv(peak_file, header=TRUE)
    kk_rows <- nrow(kk)

    # get axis labels
    x_var <- "year"
    x_label <- "Year"

    if (var == "wind_speed") {
        y_var <- "wind_spd"
        y_label <- "Wind Speed (m/s)"
    } else if (var == "gust_speed") {
        y_var <- "gust_spd"
        y_label <- "Gust Speed (m/s)"
    } else if (var == "wind_direction") {
        y_var <- "wind_dir"
        y_label <- "Wind Direction (deg)"
    } else {
        stop("var must be wind_speed, gust_speed, or wind_direction", call. = FALSE)
    }

    # subset by year
    kk$year <- as.numeric(substr(kk$date_time, 1, 4))

    if (start_year != "") {
        syear <- start_year
    } else {
        syear <- kk$year[1]
    }

    if (end_year != "") {
        eyear <- end_year
    } else {
        eyear <- kk$year[kk_rows]
    }

    kk_plot <- kk[(kk$year >= syear & kk$year <= eyear), ]

    # get variable ranges
    values <- kk_plot[ , x_var]
    xmin <- min(values, na.rm=TRUE)
    xmax <- max(values, na.rm=TRUE)
    xlim <- c(xmin, xmax)
 
    values <- kk_plot[ , y_var]
    ymin <- min(values, na.rm=TRUE)
    ymax <- max(values, na.rm=TRUE)
    ylim <- c(ymin, ymax)

    # subset by enhanced Fujita value
    kk_plot_efx <- kk_plot[kk_plot$gust_spd < ef0, ]
    kk_plot_ef0 <- kk_plot[(kk_plot$gust_spd >= ef0 & kk_plot$gust_spd < ef1), ]
    kk_plot_ef1 <- kk_plot[(kk_plot$gust_spd >= ef1 & kk_plot$gust_spd < ef2), ]
    kk_plot_ef2 <- kk_plot[(kk_plot$gust_spd >= ef2 & kk_plot$gust_spd < ef3), ]
    kk_plot_ef3 <- kk_plot[(kk_plot$gust_spd >= ef3 & kk_plot$gust_spd < ef4), ]
    kk_plot_ef4 <- kk_plot[(kk_plot$gust_spd >= ef4 & kk_plot$gust_spd < ef5), ]
    kk_plot_ef5 <- kk_plot[(kk_plot$gust_spd >= ef5), ]

    gust_max <- max(kk_plot$gust_spd)

    # get title
    main_title <- title

    if (main_title == "") {
        main_title <- site_name
    }
 
    # create plot
    oldpar <- graphics::par(no.readonly=TRUE)
    on.exit(graphics::par(oldpar))

    graphics::par(mar=c(5.1, 5.6, 4.1, 2.1))

    plot(type="n", xlim, ylim, cex.main=1.7, cex.lab=1.7, xlab=x_label, 
        ylab=y_label, main=main_title)

    graphics::points(kk_plot_efx[c(x_var, y_var)], pch=16, cex=1.5, col=efx_col)
    graphics::points(kk_plot_ef0[c(x_var, y_var)], pch=16, cex=1.5, col=ef0_col)
    graphics::points(kk_plot_ef1[c(x_var, y_var)], pch=16, cex=1.5, col=ef1_col)
    graphics::points(kk_plot_ef2[c(x_var, y_var)], pch=16, cex=1.5, col=ef2_col)
    graphics::points(kk_plot_ef3[c(x_var, y_var)], pch=16, cex=1.5, col=ef3_col)
    graphics::points(kk_plot_ef4[c(x_var, y_var)], pch=16, cex=1.5, col=ef4_col)
    graphics::points(kk_plot_ef5[c(x_var, y_var)], pch=16, cex=1.5, col=ef5_col)

    # create legend
    labs <- c("No damage")
    cols <- c(efx_col)

    if (gust_max >= ef0) {
        labs <- append(labs, "EF0 damage")
        cols <- append(cols, ef0_col)
    }
    if (gust_max >= ef1) {
        labs <- append(labs, "EF1 damage")
        cols <- append(cols, ef1_col)
    }
    if (gust_max >= ef2) {
        labs <- append(labs, "EF2 damage")
        cols <- append(cols, ef2_col)
    }
    if (gust_max >= ef3) {
        labs <- append(labs, "EF3 damage")
        cols <- append(cols, ef3_col)
    } 
    if (gust_max >= ef4) {
        labs <- append(labs, "EF4 damage")
        cols <- append(cols, ef4_col)
    }
    if (gust_max >= ef5) {
        labs <- append(labs, "EF5 damage")
        cols <- append(cols, ef5_col)
    }

    graphics::legend(legend_loc, NULL, labs, cols, cex=0.7)
}

#' @description
#' hurrecon_plot_tracks creates a regional plot of the land-water file
#' and selected hurricane tracks.
#' @param select show all positions (all), only positions used as
#' model input (model), or none (none)
#' @param wind_min the minimum value of maximum sustained wind speed 
#' (meters/second)
#' @param title optional title
#' @param colormap color palette
#' @param hur_path path for current set of model runs
#' @return no return value
#' @export
#' @rdname plotting

hurrecon_plot_tracks <- function(select="all", wind_min=33, title="", 
    colormap="default", hur_path=NULL) {
    
    # get path
    if (!is.null(hur_path)) {
        hurrecon_set_path(hur_path)
    } else {
        hur_path <- get_path()
    }

    # announcement
    message(paste("... Plotting tracks ..."))

    # read land-water file
    land_water_file <- paste(hur_path, "/input/land_water.tif", sep="")
    check_file_exists(land_water_file)
    land_water <- raster::raster(land_water_file)

    # get vector boundary file
    boundaries_file <- paste(hur_path, "/vector/boundaries.shp", sep="")
    check_file_exists(boundaries_file)
    boundaries <- rgdal::readOGR(boundaries_file)

    # get hurricane tracks
    ids_file <- paste(hur_path, "/input/ids.csv", sep="")
    check_file_exists(ids_file)
    ii <- utils::read.csv(ids_file, header=TRUE)
    names(ii)[1] <- "hur_id"

    track_file <- paste(hur_path, "/input/tracks.csv", sep="")
    check_file_exists(track_file)
    tt <- utils::read.csv(track_file, header=TRUE)
    names(tt)[1] <- "hur_id"

    track_all_file <- paste(hur_path, "/input/tracks_all.csv", sep="")
    check_file_exists(track_all_file)
    tt_all <- utils::read.csv(track_all_file, header=TRUE)
    names(tt_all)[1] <- "hur_id"

    # get palette
    if (length(colormap) == 1) {
        cmap <- rev(grDevices::terrain.colors(255))

    } else {
        cmap <- colormap
    }

    # set titles
    xlab <- "Longitude (degrees)"
    ylab <- "Latitude (degrees)"
    
    main_title <- title

    if (main_title == "") {
        main_title <- paste("Hurricane Tracks (", wind_min, " m/s)", sep="")
    }

    # create plot
    oldpar <- graphics::par(no.readonly=TRUE)
    on.exit(graphics::par(oldpar))

    graphics::par(mar=c(5.1, 4.6, 4.1, 2.1))
  
    vals <- c(0, 1, 2)
    labs <- c("", "water", "land")

    raster::plot(land_water, xlab=xlab, ylab=ylab, main=main_title,
        axis.args=list(at=vals, labels=labs), 
        legend.args=list(text='  cover', line=1), col=cmap)
    raster::plot(boundaries, add=TRUE)

    if (select == "all") {
        for (i in 1:nrow(ii)) {
            if (ii$wind_peak[i] >= wind_min) {
                hur_id <- ii[i, "hur_id"]
                xx <- tt_all[tt_all$hur_id == hur_id, ]
                graphics::lines(xx$longitude, xx$latitude, col="grey")
            }
        }
    
    } else if (select == "model"){
        for (i in 1:nrow(ii)) {
            if (ii$wind_peak[i] >= wind_min) {
                hur_id <- ii[i, "hur_id"]
                xx <- tt[tt$hur_id == hur_id, ]
                graphics::lines(xx$longitude, xx$latitude, col="grey")
            }
        }
    }
}

#' @description
#' hurrecon_plot_region creates regional plots of peak wind speed, peak 
#' enhanced Fujita scale, peak wind direction, peak cardinal wind direction,
#' and duration of EF0, EF1, EF2, EF3, EF4, and EF5 winds for a given hurricane.
#' Variables to plot: wind_speed, fujita_scale, wind_direction, wind_compass, 
#' ef0_duration, ef1_duration, ef2_duration, ef3_duration, ef4_duration,
#' and ef5_duration.
#' @param hur_id hurricane id
#' @param var variable to plot
#' @param region_all whether to plot results from hurrecon_model_region_all
#' @param positions whether to plot original positions
#' @param title optional title
#' @param colormap color palette
#' @param hur_path path for current set of model runs
#' @return no return value
#' @export
#' @rdname plotting

hurrecon_plot_region <- function(hur_id, var="fujita_scale", region_all=FALSE,
    positions=FALSE, title="", colormap="default", hur_path=NULL) {
  
    # get path
    if (!is.null(hur_path)) {
        hurrecon_set_path(hur_path)
    } else {
        hur_path <- get_path()
    }

    # announcement
    message(paste("... Plotting region ..."))
 
    # get subdirectory
    if (region_all == TRUE) {
        subdir <- "region-all"
    } else {
        subdir <- "region"
    }

    # read raster brick file in GeoTiff format
    hur_tif_file <- paste(hur_path, "/", subdir, "/", hur_id, ".tif", sep="")
    check_file_exists(hur_tif_file)
    hur_brick <- raster::brick(hur_tif_file)

    # get individual layers
    ss_layer <- raster::subset(hur_brick, 1)  # wind speed (m/s)
    ff_layer <- raster::subset(hur_brick, 2)  # enhanced Fujita scale
    dd_layer <- raster::subset(hur_brick, 3)  # wind direction (degrees)
    cc_layer <- raster::subset(hur_brick, 4)  # cardinal wind direction (1-8)
    f0_layer <- raster::subset(hur_brick, 5)  # duration of EF0 winds (minutes)
    f1_layer <- raster::subset(hur_brick, 6)  # duration of EF1 winds (minutes)
    f2_layer <- raster::subset(hur_brick, 7)  # duration of EF2 winds (minutes)
    f3_layer <- raster::subset(hur_brick, 8)  # duration of EF3 winds (minutes)
    f4_layer <- raster::subset(hur_brick, 9)  # duration of EF4 winds (minutes)
    f5_layer <- raster::subset(hur_brick, 10) # duration of EF5 winds (minutes)

    # get vector boundary file
    boundaries_file <- paste(hur_path, "/vector/boundaries.shp", sep="")
    check_file_exists(boundaries_file)
    boundaries <- rgdal::readOGR(boundaries_file)

    # get hurricane tracks
    track_all_file <- paste(hur_path, "/input/tracks_all.csv", sep="")
    check_file_exists(track_all_file)
    zz <-utils::read.csv(track_all_file, header=TRUE)
    names(zz)[1] <- "hur_id"
    index <- which(zz$hur_id == hur_id)
    tt_all <- zz[index, ]

    # get values, labels & colors for enhanced Fujita scale plot
    ef_col <- get_fujita_colors()

    ef0_col <- ef_col[[1]]
    ef1_col <- ef_col[[2]]
    ef2_col <- ef_col[[3]]
    ef3_col <- ef_col[[4]]
    ef4_col <- ef_col[[5]]
    ef5_col <- ef_col[[6]]
    efx_col <- ef_col[[7]]

    ff_all_vals <- c(0, 1, 2, 3, 4, 5, 6, 7)
    ff_all_labs <- c("", "None", "EF0", "EF1", "EF2", "EF3", "EF4", "EF5")
    ff_all_cols <- c("white", efx_col, ef0_col, ef1_col, ef2_col, ef3_col, ef4_col, ef5_col)

    ff_min <- raster::minValue(ff_layer)
    ff_max <- raster::maxValue(ff_layer)

    ff_vals <- c(ff_all_vals[ff_min+1])
    ff_labs <- c(ff_all_labs[ff_min+1])
    ff_cols <- c(ff_all_cols[ff_min+1])

    if (ff_max > ff_min) {
        for (i in (ff_min+2):(ff_max+1)) {
            ff_vals <- append(ff_vals, ff_all_vals[i])
            ff_labs <- append(ff_labs, ff_all_labs[i])
            ff_cols <- append(ff_cols, ff_all_cols[i])
        }
    }

    # default palettes
    if (length(colormap) == 1) {
        if (colormap == "default") {
            if (var == "fujita_scale") {
                cmap <- ff_cols

            } else if (var == "wind_compass") {
                cmap <- grDevices::rainbow(9)
                cmap[1] <- "white"

            } else {
                cmap <- rev(grDevices::terrain.colors(255))
            }
        }  
    # user-specified palette
    } else {
        cmap <- colormap
        cmap[1] <- "white"
    }

    # set titles
    xlab <- "Longitude (degrees)"
    ylab <- "Latitude (degrees)"

    main_title <- title

    # create plot
    oldpar <- graphics::par(no.readonly=TRUE)
    on.exit(graphics::par(oldpar))

    graphics::par(mar=c(5.1, 4.6, 4.1, 2.1))
  
    if (var == "wind_speed") {
        if (raster::maxValue(ss_layer) > 0) {
            if (main_title == "") {
                main_title <- paste(hur_id, "Peak Wind Speed")
            }
            raster::plot(ss_layer, xlab=xlab, ylab=ylab, main=main_title, 
                legend.args=list(text='  m/sec', line=1), col=cmap)
            raster::plot(boundaries, add=TRUE)
            graphics::lines(tt_all$longitude, tt_all$latitude, col="brown")
            if (positions == TRUE) {
                graphics::points(tt_all$longitude, tt_all$latitude, pch=16, cex=0.5, col="brown")
            }
        } else {
            message("No wind speed")   
        }
    } else if (var == "fujita_scale") {
        if (raster::maxValue(ff_layer) > 0) {
            if (main_title == "") {
                main_title <- paste(hur_id, "Peak Fujita Scale")
            }
            arg <- list(at=ff_vals, labels=ff_labs)
            raster::plot(ff_layer, xlab=xlab, ylab=ylab, main=main_title, axis.args=arg, 
                legend.args=list(text='  EF Scale', line=1), col=cmap)
            raster::plot(boundaries, add=TRUE)
            graphics::lines(tt_all$longitude, tt_all$latitude, col="brown")
            if (positions == TRUE) {
                graphics::points(tt_all$longitude, tt_all$latitude, pch=16, cex=0.5, col="brown")
            }
        } else {
            message("No Fujita values")
        }


    } else if (var == "wind_direction") {
        if (raster::maxValue(dd_layer) > 0) {
            if (main_title == "") {
                main_title <- paste(hur_id, "Peak Wind Direction")
            }
            raster::plot(dd_layer, xlab=xlab, ylab=ylab, main=main_title, 
                legend.args=list(text='  degrees', line=1), col=cmap)
            raster::plot(boundaries, add=TRUE)
            graphics::lines(tt_all$longitude, tt_all$latitude, col="brown")
            if (positions == TRUE) {
                graphics::points(tt_all$longitude, tt_all$latitude, pch=16, cex=0.5, col="brown")
            }
        } else {
            message("No wind direction")   
        }
    
    } else if (var == "wind_compass") {
        if (raster::maxValue(cc_layer) > 0) {
            if (main_title == "") {
                main_title <- paste(hur_id, "Peak Wind Direction")
            }
            arg <- list(at=c(0,1,2,3,4,5,6,7,8), labels=c("","N","NE","E","SE","S","SW","W","NW"))
            raster::plot(cc_layer, xlab=xlab, ylab=ylab, main=main_title, axis.args=arg, 
                legend.args=list(text='  direction', line=1), col=cmap)
            raster::plot(boundaries, add=TRUE)
            graphics::lines(tt_all$longitude, tt_all$latitude, col="brown")
            if (positions == TRUE) {
                graphics::points(tt_all$longitude, tt_all$latitude, pch=16, cex=0.5, col="brown")
            }
        } else {
            message("No wind compass")
        }

    } else if (var == "ef0_duration") {
        if (raster::maxValue(f0_layer) > 0) {
            f0_layer <- f0_layer/60
            if (main_title == "") {
                main_title <- paste(hur_id, "EF0 Winds")
            }
            raster::plot(f0_layer, xlab=xlab, ylab=ylab, main=main_title, 
                legend.args=list(text='  hours', line=1), col=cmap)
            raster::plot(boundaries, add=TRUE)
            graphics::lines(tt_all$longitude, tt_all$latitude, col="brown")
            if (positions == TRUE) {
                graphics::points(tt_all$longitude, tt_all$latitude, pch=16, cex=0.5, col="brown")
            }
        } else {
            message("No EF0 winds")
        }

    } else if (var == "ef1_duration") {
        if (raster::maxValue(f1_layer) > 0) {
            f1_layer <- f1_layer/60
            if (main_title == "") {
                main_title <- paste(hur_id, "EF1 Winds")
            }
            raster::plot(f1_layer, xlab=xlab, ylab=ylab, main=main_title, 
                legend.args=list(text='  hours', line=1), col=cmap)
            raster::plot(boundaries, add=TRUE)
            graphics::lines(tt_all$longitude, tt_all$latitude, col="brown")
            if (positions == TRUE) {
                graphics::points(tt_all$longitude, tt_all$latitude, pch=16, cex=0.5, col="brown")
            }
        } else {
            message("No EF1 winds")
        }

    } else if (var == "ef2_duration") {
        if (raster::maxValue(f2_layer) > 0) {
            f2_layer <- f2_layer/60
            if (main_title == "") {
                main_title <- paste(hur_id, "EF2 Winds")
            }
            raster::plot(f2_layer, xlab=xlab, ylab=ylab, main=main_title, 
                legend.args=list(text='  hours', line=1), col=cmap)
            raster::plot(boundaries, add=TRUE)
            graphics::lines(tt_all$longitude, tt_all$latitude, col="brown")
            if (positions == TRUE) {
                graphics::points(tt_all$longitude, tt_all$latitude, pch=16, cex=0.5, col="brown")
            }
        } else {
            message("No EF2 winds")
        }

    } else if (var == "ef3_duration") {
        if (raster::maxValue(f3_layer) > 0) {
            f3_layer <- f3_layer/60
            if (main_title == "") {
                main_title <- paste(hur_id, "EF3 Winds")
            }
            raster::plot(f3_layer, xlab=xlab, ylab=ylab, main=main_title, 
                legend.args=list(text='  hours', line=1), col=cmap)
            raster::plot(boundaries, add=TRUE)
            graphics::lines(tt_all$longitude, tt_all$latitude, col="brown")
            if (positions == TRUE) {
                graphics::points(tt_all$longitude, tt_all$latitude, pch=16, cex=0.5, col="brown")
            }
        } else {
            message("No EF3 winds")
        }

    } else if (var == "ef4_duration") {
        if (raster::maxValue(f4_layer) > 0) {
            f4_layer <- f4_layer/60
            if (main_title == "") {
                main_title <- paste(hur_id, "EF4 Winds")
            }
            raster::plot(f4_layer, xlab=xlab, ylab=ylab, main=main_title, 
                legend.args=list(text='  hours', line=1), col=cmap)
            raster::plot(boundaries, add=TRUE)
            graphics::lines(tt_all$longitude, tt_all$latitude, col="brown")
            if (positions == TRUE) {
                graphics::points(tt_all$longitude, tt_all$latitude, pch=16, cex=0.5, col="brown")
            }
        } else {
            message("No EF4 winds")
        }

    } else if (var == "ef5_duration") {
        if (raster::maxValue(f5_layer) > 0) {
            f5_layer <- f5_layer/60
            if (main_title == "") {
                main_title <- paste(hur_id, "EF5 Winds")
            }
            raster::plot(f5_layer, xlab=xlab, ylab=ylab, main=main_title, 
                legend.args=list(text='  hours', line=1), col=cmap)
            raster::plot(boundaries, add=TRUE)
            graphics::lines(tt_all$longitude, tt_all$latitude, col="brown")
            if (positions == TRUE) {
                graphics::points(tt_all$longitude, tt_all$latitude, pch=16, cex=0.5, col="brown")
            }
        } else {
            message("No EF5 winds")
        }

    } else {
        stop("var must be wind_speed, fujita_scale, wind_direction, wind_compass, ef0_duration, 
            ef1_duration, ef2_duration, ef3_duration, ef4_duration, or ef5_duration", call. = FALSE)
    }
}

#' @description
#' hurrecon_plot_region_dt creates regional plots of enhanced Fujita scale, 
#' wind speed, wind direction, and cardinal wind direction for a given hurricane
#' at a specified datetime. Variables to plot: wind_speed, fujita_scale, 
#' wind_direction, or wind_compass.
#' @param hur_id hurricane id
#' @param dt datetime in the format YYYY-MM-DDThh:mm
#' @param var variable to plot
#' @param positions whether to plot original positions
#' @param title optional title
#' @param colormap color palette
#' @param hur_path path for current set of model runs
#' @return no return value
#' @export
#' @rdname plotting

hurrecon_plot_region_dt <- function(hur_id, dt, var="fujita_scale", positions=FALSE,
    title="", colormap="default", hur_path=NULL) {

    # get path
    if (!is.null(hur_path)) {
        hurrecon_set_path(hur_path)
    } else {
        hur_path <- get_path()
    }

    # announcement
    message(paste("... Plotting region dt ..."))
 
    # read raster brick file in GeoTiff format
    dt2 <- gsub(":", "", dt)
    hur_tif_file <- paste(hur_path, "/region-dt/", hur_id, " ", dt2, ".tif", sep="")
    check_file_exists(hur_tif_file)
    hur_brick <- raster::brick(hur_tif_file)

    # get individual layers
    ss_layer <- raster::subset(hur_brick, 1)  # wind speed (m/s)
    ff_layer <- raster::subset(hur_brick, 2)  # enhanced Fujita scale
    dd_layer <- raster::subset(hur_brick, 3)  # wind direction (degrees)
    cc_layer <- raster::subset(hur_brick, 4)  # cardinal wind direction (1-8)

    # get vector boundary file
    boundaries_file <- paste(hur_path, "/vector/boundaries.shp", sep="")
    check_file_exists(boundaries_file)
    boundaries <- rgdal::readOGR(boundaries_file)

    # get current location
    tt <- read_hurricane_track_file(hur_id)
    pp <- get_values_at_datetime(hur_id, tt, dt)

    # get hurricane track
    track_all_file <- paste(hur_path, "/input/tracks_all.csv", sep="")
    check_file_exists(track_all_file)
    zz <-utils::read.csv(track_all_file, header=TRUE)
    names(zz)[1] <- "hur_id"
    index <- which(zz$hur_id == hur_id)
    tt_all <- zz[index, ]

    # get values, labels & colors for enhanced Fujita scale plot
    ef_col <- get_fujita_colors()

    ef0_col <- ef_col[[1]]
    ef1_col <- ef_col[[2]]
    ef2_col <- ef_col[[3]]
    ef3_col <- ef_col[[4]]
    ef4_col <- ef_col[[5]]
    ef5_col <- ef_col[[6]]
    efx_col <- ef_col[[7]]

    ff_all_vals <- c(0, 1, 2, 3, 4, 5, 6, 7)
    ff_all_labs <- c("", "None", "EF0", "EF1", "EF2", "EF3", "EF4", "EF5")
    ff_all_cols <- c("white", efx_col, ef0_col, ef1_col, ef2_col, ef3_col, ef4_col, ef5_col)

    ff_min <- raster::minValue(ff_layer)
    ff_max <- raster::maxValue(ff_layer)

    ff_vals <- c(ff_all_vals[ff_min+1])
    ff_labs <- c(ff_all_labs[ff_min+1])
    ff_cols <- c(ff_all_cols[ff_min+1])

    if (ff_max > ff_min) {
        for (i in (ff_min+2):(ff_max+1)) {
            ff_vals <- append(ff_vals, ff_all_vals[i])
            ff_labs <- append(ff_labs, ff_all_labs[i])
            ff_cols <- append(ff_cols, ff_all_cols[i])
        }
    }

    # default palettes
    if (length(colormap) == 1) {
        if (colormap == "default") {
            if (var == "fujita_scale") {
                cmap <- ff_cols

            } else if (var == "wind_compass") {
                cmap <- grDevices::rainbow(9)
                cmap[1] <- "white"

            } else {
                cmap <- rev(grDevices::terrain.colors(255))
            }
        }

    # user-specified palette
    } else {
        cmap <- colormap
        cmap[1] <- "white"
    }

    # set titles
    xlab <- "Longitude (degrees)"
    ylab <- "Latitude (degrees)"

    main_title <- title

    # create plot
    oldpar <- graphics::par(no.readonly=TRUE)
    on.exit(graphics::par(oldpar))

    graphics::par(mar=c(5.1, 4.6, 4.1, 2.1))
  
    if (var == "fujita_scale") {
        if (raster::maxValue(ff_layer) > 0) {
            if (main_title == "") {
                main_title <- paste(hur_id, "Fujita Scale", dt)
            }
            arg <- list(at=ff_vals, labels=ff_labs)
            raster::plot(ff_layer, xlab=xlab, ylab=ylab, main=main_title, axis.args=arg, 
                legend.args=list(text='  EF Scale', line=1), col=cmap)
            raster::plot(boundaries, add=TRUE)
            graphics::lines(tt_all$longitude, tt_all$latitude, col="brown")
            if (positions == TRUE) {
                graphics::points(tt_all$longitude, tt_all$latitude, pch=16, cex=0.5, col="brown")
            }
            graphics::points(pp$lon[1], pp$lat[1], pch=16, cex=1, col="brown")
        } else {
            message("No Fujita values")
        }

    } else if (var == "wind_speed") {
        if (raster::maxValue(ss_layer) > 0) {
            if (main_title == "") {
                main_title <- paste(hur_id, "Wind Speed", dt)
            }
            raster::plot(ss_layer, xlab=xlab, ylab=ylab, main=main_title, 
                legend.args=list(text='  m/sec', line=1), col=cmap)
            raster::plot(boundaries, add=TRUE)
            graphics::lines(tt_all$longitude, tt_all$latitude, col="brown")
            if (positions == TRUE) {
                graphics::points(tt_all$longitude, tt_all$latitude, pch=16, cex=0.5, col="brown")
            }
            graphics::points(pp$lon[1], pp$lat[1], pch=16, cex=1, col="brown")
        } else {
            message("No wind speed")
        }

    } else if (var == "wind_direction") {
        if (raster::maxValue(dd_layer) > 0) {
            if (main_title == "") {
                main_title <- paste(hur_id, "Wind Direction", dt)
            }
            raster::plot(dd_layer, xlab=xlab, ylab=ylab, main=main_title, 
                legend.args=list(text='  degrees', line=1), col=cmap)
            raster::plot(boundaries, add=TRUE)
            graphics::lines(tt_all$longitude, tt_all$latitude, col="brown")
            if (positions == TRUE) {
                graphics::points(tt_all$longitude, tt_all$latitude, pch=16, cex=0.5, col="brown")
            }
            graphics::points(pp$lon[1], pp$lat[1], pch=16, cex=1, col="brown")
        } else {
            message("No wind direction")   
        }

    } else if (var == "wind_compass") {
        if (raster::maxValue(cc_layer) > 0) {
            if (main_title == "") {
                main_title <- paste(hur_id, "Wind Direction", dt)
            }
            arg <- list(at=c(0,1,2,3,4,5,6,7,8), labels=c("","N","NE","E","SE","S","SW","W","NW"))
            raster::plot(cc_layer, xlab=xlab, ylab=ylab, main=main_title, axis.args=arg, 
                legend.args=list(text='  direction', line=1), col=cmap)
            raster::plot(boundaries, add=TRUE)
            graphics::lines(tt_all$longitude, tt_all$latitude, col="brown")
            if (positions == TRUE) {
                graphics::points(tt_all$longitude, tt_all$latitude, pch=16, cex=0.5, col="brown")
            }
            graphics::points(pp$lon[1], pp$lat[1], pch=16, cex=1, col="brown")
        } else {
            message("No wind compass")
        }

    } else {
        stop("var must be wind_speed, fujita_scale, wind_direction, or wind_compass", call. = FALSE)
    }
}

#' @description
#' hurrecon_plot_region_all creates regional plots of maximum enhanced Fujita
#' value and number of storms for each enhanced Fujita value for all hurricanes.
#' Variables to plot: efmax, ef0, ef1, ef2, ef3, ef4, or ef5.
#' @param var variable to plot
#' @param tracks whether to also plot hurricane tracks
#' @param title optional title
#' @param colormap color palette
#' @param hur_path path for current set of model runs
#' @return no return value
#' @export
#' @rdname plotting

hurrecon_plot_region_all <- function(var="efmax", tracks=FALSE, title="",
    colormap="default", hur_path=NULL) {
    
    # get path
    if (!is.null(hur_path)) {
        hurrecon_set_path(hur_path)
    } else {
        hur_path <- get_path()
    }

    # announcement
    message(paste("... Plotting region all ..."))
 
    # read summary file in GeoTiff format
    sum_tif_file <- paste(hur_path, "/region-all/", "summary.tif", sep="")
    check_file_exists(sum_tif_file)
    sum_brick <- raster::brick(sum_tif_file)

    # get individual layers
    efm_layer <- raster::subset(sum_brick, 1)
    ef0_layer <- raster::subset(sum_brick, 2)
    ef1_layer <- raster::subset(sum_brick, 3)
    ef2_layer <- raster::subset(sum_brick, 4)
    ef3_layer <- raster::subset(sum_brick, 5)
    ef4_layer <- raster::subset(sum_brick, 6)
    ef5_layer <- raster::subset(sum_brick, 7)

    # get vector boundary file
    boundaries_file <- paste(hur_path, "/vector/boundaries.shp", sep="")
    check_file_exists(boundaries_file)
    boundaries <- rgdal::readOGR(boundaries_file)

    # get values, labels & colors for enhanced Fujita scale plot
    ef_col <- get_fujita_colors()

    ef0_col <- ef_col[[1]]
    ef1_col <- ef_col[[2]]
    ef2_col <- ef_col[[3]]
    ef3_col <- ef_col[[4]]
    ef4_col <- ef_col[[5]]
    ef5_col <- ef_col[[6]]
    efx_col <- ef_col[[7]]

    efm_all_vals <- c(0, 1, 2, 3, 4, 5, 6, 7)
    efm_all_labs <- c("", "None", "EF0", "EF1", "EF2", "EF3", "EF4", "EF5")
    efm_all_cols <- c("white", efx_col, ef0_col, ef1_col, ef2_col, ef3_col, ef4_col, ef5_col)

    efm_min <- raster::minValue(efm_layer)
    efm_max <- raster::maxValue(efm_layer)

    efm_vals <- c(efm_all_vals[efm_min+1])
    efm_labs <- c(efm_all_labs[efm_min+1])
    efm_cols <- c(efm_all_cols[efm_min+1])

    if (efm_max > efm_min) {
        for (i in (efm_min+2):(efm_max+1)) {
        efm_vals <- append(efm_vals, efm_all_vals[i])
        efm_labs <- append(efm_labs, efm_all_labs[i])
        efm_cols <- append(efm_cols, efm_all_cols[i])
        }
    }

    # default palettes
    if (length(colormap) == 1) {
        if (colormap == "default") {
            if (var == "efmax") {
                cmap <- efm_cols

            } else {
                cmap <- rev(grDevices::terrain.colors(255))
            }
        }  

    #user-specified palette
    } else {
        cmap <- colormap
        cmap[1] <- "white"
    }

    # set titles
    xlab <- "Longitude (degrees)"
    ylab <- "Latitude (degrees)"

    main_title <- title

    # get hurricane tracks
    if (tracks) {
        ids_file <- paste(hur_path, "/input/ids.csv", sep="")
        check_file_exists(ids_file)
        ii <- utils::read.csv(ids_file, header=TRUE)
        names(ii)[1] <- "hur_id"

        track_all_file <- paste(hur_path, "/input/tracks_all.csv", sep="")
        check_file_exists(track_all_file)
        tt_all <- utils::read.csv(track_all_file, header=TRUE)
        names(tt_all)[1] <- "hur_id"

        summary_file <- paste(hur_path, "/region-all/summary.csv", sep="")
        check_file_exists(summary_file)
        kk <- utils::read.csv(summary_file, header=TRUE)
        names(kk)[1] <- "hur_id"
    }
  
    # create plot
    oldpar <- graphics::par(no.readonly=TRUE)
    on.exit(graphics::par(oldpar))

    graphics::par(mar=c(5.1, 4.6, 4.1, 2.1))

    if (var == "efmax") {
        if (raster::maxValue(efm_layer) > 0) {
            if (main_title == "") {
                main_title <- "Peak Fujita Scale"
            }
            arg <- list(at=efm_vals, labels=efm_labs)
            raster::plot(efm_layer, xlab=xlab, ylab=ylab, main=main_title, axis.args=arg, 
                legend.args=list(text='  EF Scale', line=1), col=cmap)
            raster::plot(boundaries, add=TRUE)
            if (tracks) {
                for (i in 1:nrow(ii)) {
                    hur_id <- ii[i, "hur_id"]
                    fuj_min <- 0
                    xx <- get_track_lat_lon(hur_id, fuj_min, tt_all, kk)
                    if (!is.null(xx)) {
                        graphics::lines(xx$longitude, xx$latitude, col="grey")
                    }
                }
            }
        } else {
            message("No Fujita values")
        }

    } else if (var == "ef0") {
        if (raster::maxValue(ef0_layer) > 0) {
            if (main_title == "") {
                main_title <- "Fujita Scale 0"
            }
            raster::plot(ef0_layer, xlab=xlab, ylab=ylab, main=main_title, 
                legend.args=list(text='  storms', line=1), col=cmap)
            raster::plot(boundaries, add=TRUE)
            if (tracks) {
                for (i in 1:nrow(ii)) {
                    hur_id <- ii[i, "hur_id"]
                    fuj_min <- 0
                    xx <- get_track_lat_lon(hur_id, fuj_min, tt_all, kk)
                    if (!is.null(xx)) {
                        graphics::lines(xx$longitude, xx$latitude, col="grey")
                    }         
                }
            }
        } else {
            message("No F0 values")
        }
  
    } else if (var == "ef1") {
        if (raster::maxValue(ef1_layer) > 0) {
            if (main_title == "") {
                main_title <- "Fujita Scale 1"
            }
            raster::plot(ef1_layer, xlab=xlab, ylab=ylab, main=main_title, 
                legend.args=list(text='  storms', line=1), col=cmap)
            raster::plot(boundaries, add=TRUE)
            if (tracks) {
                for (i in 1:nrow(ii)) {
                    hur_id <- ii[i, "hur_id"]
                    fuj_min <- 1
                    xx <- get_track_lat_lon(hur_id, fuj_min, tt_all, kk)
                    if (!is.null(xx)) {
                        graphics::lines(xx$longitude, xx$latitude, col="grey")
                    }         
                }
            }
        } else {
            message("No F1 values")
        }

    } else if (var == "ef2") {
        if (raster::maxValue(ef2_layer) > 0) {
            if (main_title == "") {
                main_title <- "Fujita Scale 2"
            }
            raster::plot(ef2_layer, xlab=xlab, ylab=ylab, main=main_title, 
                legend.args=list(text='  storms', line=1), col=cmap)
            raster::plot(boundaries, add=TRUE)
            if (tracks) {
                for (i in 1:nrow(ii)) {
                    hur_id <- ii[i, "hur_id"]
                    fuj_min <- 2
                    xx <- get_track_lat_lon(hur_id, fuj_min, tt_all, kk)
                    if (!is.null(xx)) {
                        graphics::lines(xx$longitude, xx$latitude, col="grey")
                    }         
                }
            }
        } else {
            message("No F2 values")
        }
     
    } else if (var == "ef3") {
        if (raster::maxValue(ef3_layer) > 0) {
            if (main_title == "") {
                main_title <- "Fujita Scale 3"
            }
            raster::plot(ef3_layer, xlab=xlab, ylab=ylab, main=main_title, 
                legend.args=list(text='  storms', line=1), col=cmap)
            raster::plot(boundaries, add=TRUE)
            if (tracks) {
                for (i in 1:nrow(ii)) {
                    hur_id <- ii[i, "hur_id"]
                    fuj_min <- 3
                    xx <- get_track_lat_lon(hur_id, fuj_min, tt_all, kk)
                    if (!is.null(xx)) {
                        graphics::lines(xx$longitude, xx$latitude, col="grey")
                    }         
                }
            }
        } else {
            message("No F3 values")
        }

    } else if (var == "ef4") {
        if (raster::maxValue(ef4_layer) > 0) {
            if (main_title == "") {
                main_title <- "Fujita Scale 4"
            }
            raster::plot(ef4_layer, xlab=xlab, ylab=ylab, main=main_title, 
                legend.args=list(text='  storms', line=1), col=cmap)
            raster::plot(boundaries, add=TRUE)
            if (tracks) {
                for (i in 1:nrow(ii)) {
                    hur_id <- ii[i, "hur_id"]
                    fuj_min <- 4
                    xx <- get_track_lat_lon(hur_id, fuj_min, tt_all, kk)
                    if (!is.null(xx)) {
                        graphics::lines(xx$longitude, xx$latitude, col="grey")
                    }
                }         
            }
        } else {
            message("No F4 values")
        }

    } else if (var == "ef5") {
            if (main_title == "") {
                main_title <- "Fujita Scale 5"
            }
        if (raster::maxValue(ef5_layer) > 0) {
            raster::plot(ef5_layer, xlab=xlab, ylab=ylab, main=main_title, 
                legend.args=list(text='  storms', line=1), col=cmap)
            raster::plot(boundaries, add=TRUE)
            if (tracks) {
                for (i in 1:nrow(ii)) {
                    hur_id <- ii[i, "hur_id"]
                    fuj_min <- 5
                    xx <- get_track_lat_lon(hur_id, fuj_min, tt_all, kk)
                    if (!is.null(xx)) {
                        graphics::lines(xx$longitude, xx$latitude, col="grey")
                    }         
                }
            }
        } else {
            message("No F5 values")
        }

    } else {
        stop("var must be efmax, ef0, ef1, ef2, ef3, ef4, or ef5", call. = FALSE)
    }
}

