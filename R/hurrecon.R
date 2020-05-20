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

# The HURRECON Model estimates wind speed, wind direction, enhanced Fujita 
# scale wind damage, and duration of gale and hurricane winds as a function
# of hurricane location and maximum wind speed.

# Emery R. Boose
# May 2020

# R version 4.0.0

# Required packages:
#  raster
#  rgdal


### INTERNAL FUNCTIONS ####################################

#' get_operating_system returns the current operating system type
#' (windows, osx, unix, linux, or unknown).
#' @return operating system type
#' @noRd

get_operating_system <- function() {
  if (!is.null(Sys.info())) {
    os <- toString(Sys.info()['sysname'])
    if (os == 'Darwin') os <- "osx"
  
  } else {
    os <- .Platform$OS.type
    
    if (grepl("^darwin", R.version$os)) {
      os <- "osx"
    } else if (grepl("linux-gnu", R.version$os)) {
      os <- "linux"
    }
  }
  
  os <- tolower(os)
  os_types <- c("windows", "osx", "unix", "linux")
  if (!(os %in% os_types)) os <- "unknown"

  return(os)
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

#' format_time_difference_hms returns a time difference formatted as
#' hours:minutes:seconds.
#' @param start_time start time
#' @param end_time end time
#' @return a time difference formatted as hh:mm:ss.
#' @noRd

format_time_difference_hms <- function(start_time, end_time) {
  dsec <- as.numeric(difftime(end_time, start_time, unit="secs"))
  
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
  t_diff <- formatC(1000*difftime(end_time, start_time, unit="secs"), width=3, format="f", digits=0, flag="0")

  return(t_diff)
}

#' check_file_exists displays an error message and stops execution if
#' the specified file does not exist.
#' @param file_name name of file
#' @return nothing
#' @noRd

check_file_exists <- function(file_name) {
  if (file.exists(file_name) == FALSE) {
    cat("\nFile not found:", file_name, "\n")
    stop()
  }
}

#' read_site_file reads a site file and returns a vector containing the
#' latitude (degrees), longitude (degrees), and cover type (water=1, land=2)
#' for the specified site.
#' @param site_name name of site
#' @return vector of latitude, longitude, and cover type
#' @noRd

read_site_file <- function(site_name) {
  cwd <- getwd()
  site_file <- paste(cwd, "/input/sites.csv", sep="")
  check_file_exists(site_file)
  ss <- read.csv(site_file)
  names(ss)[1] <- "site_name"

  # get site location & cover type
  index <- which(ss$site_name == site_name)

  if (length(index) == 0) {
    cat("\nSite not found\n")
    stop()
  }

  i <- min(index)
  site_latitude <- ss$latitude[i]
  site_longitude <- ss$longitude[i]
  cover_type <- ss$cover_type[i]
 
  return(c(site_latitude, site_longitude, cover_type))
}

#' read_parameter_file reads a parameter file and returns a vector containing
#' the radius of maximum wind (rmw) (kilometers) and profile exponent (s_par). 
#' If width is TRUE, parameters are returned for the specified hurricane, 
#' if available; otherwise parameters for ALL are returned.
#' @param hur_id hurricane id
#' @param width whether to use width parameters for the specified hurricane
#' @return vector of rmw and s_par
#' @noRd

read_parameter_file <- function(hur_id, width) {
  cwd <- getwd()
  par_file <- paste(cwd, "/input/parameters.csv", sep="")
  check_file_exists(par_file)
  pp <- read.csv(par_file)
  names(pp)[1] <- "hur_id"

  # get rmw & s_par parameters
  if (width) {
    index <- which(pp$hur_id == hur_id)
  } else {
    index <- which(pp$hur_id == "ALL")
  }

  if (length(index) == 0) {
    cat("Parameter file must contain an entry for ALL\n")
    stop()
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
    cat("\nCover type must be 1 (water) or 2 (land)\n")
    stop()
  }

  return(c(asymmetry_factor, inflow_angle, friction_factor, gust_factor))
}

#' get_time_step calculates the time step (minutes) for regional modeling, 
#' assuming a maximum hurricane forward speed of 20 meters per second (1200 
#' meters per minute). Values are rounded to the nearest 1, 2, 3, 5, 10, 15, 
#' 30, or 60 minutes.
#' @return time step
#' @noRd

get_time_step <- function() {
  # read land-water file
  cwd <- getwd()
  land_water_file <- paste(cwd, "/input/land_water.tif", sep="")
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
  # read hurricane track file
  cwd <- getwd()
  track_file <- paste(cwd, "/input/tracks.csv", sep="")
  check_file_exists(track_file)
  zz <-read.csv(track_file, header=TRUE, stringsAsFactors=FALSE)
  names(zz)[1] <- "hur_id"

  # subset by hurricane name
  tt <- zz[(zz$hur_id == hur_id), ]

  if (nrow(tt) == 0) {
    cat("Hurricane not in track file\n")
    stop()
  }

  return(tt)
}

#' interpolate_hurricane_location_max_wind performs a linear interpolation
#' of hurricane latitude & longitude (degrees) and maximum sustained wind
#' speed (meters/second) using data from a hurricane track file and a 
#' specified time step.
#' @param tt data frame of track data
#' @param time_step time step (minutes)
#' @return a data frame of interpolated data
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

  # create data frame for modeled data
  all_rows = length(jd_vec)

  mm <- data.frame(date_time=character(all_rows), year=integer(all_rows), 
    jd=numeric(all_rows), latitude=numeric(all_rows), longitude=numeric(all_rows), 
    wind_max=numeric(all_rows), hur_bear=numeric(all_rows), hur_spd=numeric(all_rows), 
    site_bear=numeric(all_rows), site_range=numeric(all_rows), rmw=numeric(all_rows), 
    s_par=numeric(all_rows), wind_dir=numeric(all_rows), wind_spd=numeric(all_rows), 
    gust_spd=numeric(all_rows), ef_sca=numeric(all_rows), stringsAsFactors=FALSE)

  mm$year <- substr(tt$date_time[1], 1, 4)

  mm$jd<- jd_vec
  mm$latitude<- lat_vec
  mm$longitude<- lon_vec
  mm$wind_max<- wmax_vec

  return(mm)
}

#' estimate_range uses the Pythagorean equation to estimate the range 
#' (kilometers) from one point to another based on the latitude & longitude 
#' of each point. Note: overestimates range unless on same meridian.
#' @param lat1 latitude of first point
#' @param lon1 longitude of first point
#' @param lat2 latitude of second point
#' @param lon2 longitude of second point
#' @return range
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
#' @param lat1 latitude of first point
#' @param lon1 longitude of first point
#' @param lat2 latitude of second point
#' @param lon2 longitude of second point
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
    if (lon1 > 90 && lon2 < -90) lon2 <- lon2 + 360
    if (lon1 < -90 && lon2 > 90) lon1 <- lon1 + 360

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
#' @param lat1 latitude of first point
#' @param lon1 longitude of first point
#' @param lat2 latitude of second point
#' @param lon2 longitude of second point
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
    if (lon1 > 90 && lon2 < -90) lon2 <- lon2 + 360
    if (lon1 < -90 && lon2 > 90) lon1 <- lon1 + 360

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
  # read hurricane track file
  cwd <- getwd()
  track_file <- paste(cwd, "/input/tracks.csv", sep="")
  check_file_exists(track_file)
  zz <-read.csv(track_file, header=TRUE, stringsAsFactors=FALSE)
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
#' @param s_par profile constant
#' @return range in kilometers
#' @noRd

get_maximum_range <- function(wmax, rmw, s_par) {
  rang <- rmw
  wspd <- 100

  while (wspd > 17.5) {
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
#' @param mm data frame of modeled values
#' @return data frame of modeled values
#' @noRd

interpolate_hurricane_speed_bearing <- function(tt, mm) {
  tt_rows <- nrow(tt)
  mm_rows <- nrow(mm)

  # create data frame for mid-segment values
  vv <- data.frame(jd=numeric(tt_rows-1), hur_bear=numeric(tt_rows-1), 
    hur_spd=numeric(tt_rows-1), stringsAsFactors=FALSE)

  # calculate mid-segment hurricane speed & bearing
  for (i in (1:(tt_rows-1))) {
    hur_range <- calculate_range(tt$latitude[i], tt$longitude[i],
      tt$latitude[i+1], tt$longitude[i+1])
  
    hur_bear <- calculate_bearing(tt$latitude[i], tt$longitude[i],
      tt$latitude[i+1], tt$longitude[i+1])
    
    interval_sec <- (tt$jd[i+1] - tt$jd[i]) * 1440 * 60
    
    vv$jd[i] <- tt$jd[i] + (tt$jd[i+1] - tt$jd[i])/2
    vv$hur_spd[i] <- (1000*hur_range)/interval_sec
    vv$hur_bear[i] <- hur_bear
  }
  
  vv_rows <- nrow(vv)
  
  # initialize vectors
  bear_vec <- vector()
  spd_vec <- vector()

  # interpolate hurricane speed & bearing for each segment
  for (i in 1:(vv_rows+1)) {
    # before mid-point of 1st segment
    if (i == 1) {
      index <- which(mm$jd <= vv$jd[1])
      new_rows <- length(index)
      
      bear <- rep(vv$hur_bear[1], new_rows)
      spd <- rep(vv$hur_spd[1], new_rows)
      
      bear_vec <- append(bear_vec, bear)
      spd_vec <- append(spd_vec, spd)

    # interpolate between mid-points
    } else if (i <= vv_rows) {
      index <- which((mm$jd > vv$jd[i-1]) & (mm$jd <= vv$jd[i]))
      new_rows <- length(index)

      # bearing
      b1 <- vv$hur_bear[i-1]
      b2 <- vv$hur_bear[i]

      if (b2 - b1 > 180) {
        b1 <- b1 + 360
      } else if (b1 - b2 > 180) {
        b2 <- b2 + 360
      }
        
      bear <- seq(from=b1, to=b2, length.out=new_rows)        

      # speed
      spd <- seq(from=vv$hur_spd[i-1], to=vv$hur_spd[i], length.out=new_rows)
 
      bear_vec <- append(bear_vec, bear)
      spd_vec <- append(spd_vec, spd)

    # after mid-point of last segment
    } else {
      index <- which(mm$jd > vv$jd[vv_rows])
      new_rows = length(index)

      bear <- rep(vv$hur_bear[vv_rows], new_rows)
      spd <- rep(vv$hur_spd[vv_rows], new_rows)

      bear_vec <- append(bear_vec, bear)
      spd_vec <- append(spd_vec, spd)
    }
  }  

  # adjust bearing as needed
  for (i in 1:length(bear_vec)) {
    if (bear_vec[i] < 0) bear_vec[i] <- bear_vec[i] + 360
    if (bear_vec[i] > 360) bear_vec[i] <- bear_vec[i] - 360
  }

  # add to modeled data frame
  mm$hur_bear <- bear_vec
  mm$hur_spd <- spd_vec

  return(mm)
}

#' calculate_site_range_bearing calculates the range (kilometers) and bearing
#' (degrees) from a site to the hurricane center.
#' @param mm data frame of modeled values
#' @param site_latitude latitude of site
#' @param site_longitude longitude of site
#' @return data frame of modeled values
#' @noRd

calculate_site_range_bearing <- function(mm, site_latitude, site_longitude) {
  mm_rows <- nrow(mm)
  
  for (i in 1:mm_rows) {
    mm$site_range[i] <- calculate_range(site_latitude, site_longitude, 
      mm$latitude[i], mm$longitude[i])
    
    mm$site_bear[i] <- calculate_bearing(site_latitude, site_longitude, 
      mm$latitude[i], mm$longitude[i])
  }
  
  return(mm)
}

#' calculate_wind_direction calculates the wind direction (degrees) at the
#' specified site.
#' @param hurr_lat latitude of hurricane (degrees)
#' @param site_bear bearing from site to hurricane center (degrees)
#' @param inflow_angle cross-isobar inflow angle (degrees)
#' @return calculated wind direction
#' @noRd

calculate_wind_direction <- function (hur_lat, site_bear, inflow_angle) {
  # northern hemisphere: tangent minus inflow angle
  if (hur_lat > 0) {
    wind_dir <- site_bear - 90 - inflow_angle
    if (wind_dir < 0) wind_dir <- wind_dir + 360
  
  # southern hemisphere: tangent plus inflow angle
  } else {
    wind_dir <- site_bear + 90 + inflow_angle
    if (wind_dir > 360) wind_dir <- wind_dir - 360
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
#' @param s_par profile exponent
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
    
    if (Z < 0) Z <- 0
      
    # sustained wind speed at site
    wind_spd <- Z * sqrt(X * exp(1 - X))

    # adjust for land or water
    wind_spd <- wind_spd * friction_factor
  }

  return(wind_spd)
}

#' calculate_wind_gust calculates the wind gust speed (meters/second) from 
#' the sustained wind speed (meters/second) and the gust factor.
#' @param wind_spd sustained wind speed
#' @param gust_factor gust factor
#' @return wind gust speed
#' @noRd

calculate_wind_gust <- function (wind_spd, gust_factor) {
  gust_spd <- gust_factor * wind_spd
  
  return(gust_spd)
}

#' calculate_enhanced_fujita_scale returns the enhanced Fujita scale value
#' based on the wind gust speed (meters/second)
#' @param gust_spd wind gust speed
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

#' calculate_wind_speed_direction calculates the wind speed, gust speed, wind
#' direction, and enhanced Fujita scale wind damage at a site.
#' @param mm data frame of modeled values
#' @param inflow_angle cross-isobar inflow angle (degrees)
#' @param cover_type cover type (1=water, 2=land)
#' @param rmw radius of maximum winds (kilometers)
#' @param s_par profile exponent
#' @param asymmetry_factor asymmetry factor
#' @param friction_factor friction factor
#' @param gust_factor gust factor
#' @return data frame of modeled values
#' @noRd

calculate_wind_speed_direction <- function(mm, inflow_angle, cover_type, rmw, 
  s_par, asymmetry_factor, friction_factor, gust_factor) {
 
  mm_rows <- nrow(mm)

  for (i in 1:mm_rows) {
    # wind speed
    mm$wind_spd[i] <- calculate_wind_speed(mm$site_bear[i], mm$site_range[i], 
      mm$latitude[i], mm$hur_bear[i], mm$hur_spd[i], mm$wind_max[i], rmw, 
      s_par, asymmetry_factor, friction_factor)
  
    # gust speed
    mm$gust_spd[i] <- calculate_wind_gust(mm$wind_spd[i], gust_factor)
  
    # wind direction
    mm$wind_dir[i] <- calculate_wind_direction (mm$latitude[i], 
      mm$site_bear[i], inflow_angle)
  
    # enhanced Fujita scale
    mm$ef_sca[i] <- calculate_enhanced_fujita_scale(mm$gust_spd[i])
  }

  mm$rmw <- rmw
  mm$s_par <- s_par

  return(mm)
}

#' add_standard_date_time adds a standard datetime column in the format
#' YYYY-MM-DDThh:mm to a data frame of modeled values.
#' @param mm data frame of modeled values
#' @return data frame of modeled values
#' @noRd

add_standard_date_time <- function(mm) {
  # get integer & fraction of Julian date
  mm$jd_int <- trunc(mm$jd)
  mm$jd_frac <- mm$jd - mm$jd_int

  # get date in standard format
  mm$date <- as.Date(mm$jd_int - 1, origin=paste(mm$year, "-01-01", sep=""))

  # get hours & minutes
  mm$min_tot <- round(mm$jd_frac * 1440)
  mm$hour <- trunc(mm$min_tot / 60)
  mm$min <- round(mm$min_tot - mm$hour * 60)

  # convert numbers to strings
  mm$hh <- sprintf("%02d", mm$hour)
  mm$mm <- sprintf("%02d", mm$min)

  # add column for datetime in standard format
  mm$date_time <- paste(as.character(mm$date), "T", mm$hh, ":", mm$mm, sep="")

  # remove unnecessary columns
  mm[ , c("jd_int", "jd_frac", "date", "min_tot", "hour", "min", 
    "hh", "mm")] <- list(NULL)
  
  return(mm)
}

#' get_peak_values returns a data frame of peak values for a given
#' hurricane and site.
#' @param hur_id hurricane id
#' @param site_name name of site
#' @param mm data frame of modeled values
#' @return data frame of peak values
#' @noRd

get_peak_values <- function(hur_id, site_name, mm) {
  # get time step in minutes
  h1 <- as.integer(substr(mm$date_time[1], 12, 13))
  m1 <- as.integer(substr(mm$date_time[1], 15, 16))
  t1 <- h1 * 60 + m1

  h2 <- as.integer(substr(mm$date_time[2], 12, 13))
  m2 <- as.integer(substr(mm$date_time[2], 15, 16))
  t2 <- h2 * 60 + m2

  time_step <- t2 - t1

  # get peak wind
  pk <- mm[mm$wind_spd == max(mm$wind_spd), ]

  date_time <- pk$date_time[1]
  wind_dir <- pk$wind_dir[1]
  wind_spd <- pk$wind_spd[1]
  gust_spd <- pk$gust_spd[1]
  ef_sca <- pk$ef_sca[1]

  # get wind duration in hours
  ef0_obs <- mm[mm$ef_sca >= 0, ]
  ef1_obs <- mm[mm$ef_sca >= 1, ]
  ef2_obs <- mm[mm$ef_sca >= 2, ]
  ef3_obs <- mm[mm$ef_sca >= 3, ]
  ef4_obs <- mm[mm$ef_sca >= 4, ]
  ef5_obs <- mm[mm$ef_sca >= 5, ]

  ef0 <- nrow(ef0_obs) * time_step/60
  ef1 <- nrow(ef1_obs) * time_step/60
  ef2 <- nrow(ef2_obs) * time_step/60
  ef3 <- nrow(ef3_obs) * time_step/60
  ef4 <- nrow(ef4_obs) * time_step/60
  ef5 <- nrow(ef5_obs) * time_step/60

  # create data fame of peak values
  kk <- data.frame(site_name, hur_id, date_time, wind_dir, wind_spd, gust_spd, ef_sca, 
    ef0, ef1, ef2, ef3, ef4, ef5)

  return(kk)
}

#' get_regional_peak_wind calculates the peak wind speed (meters/second), enhanced 
#' Fujita scale, wind direction (degrees), cardinal wind direction, gale wind duration 
#' (minutes), and hurricane wind duration (minutes) for a given hurricane over a region.
#' Results are returned in a raster brick with 6 layers.
#' @param hur_id hurricane id
#' @param mm data frame of modeled values
#' @param width whether to use width parameters for the specified hurricane
#' @param time_step time step (minutes)
#' @param water whether to calculate values over water
#' @param timing whether to report progress
#' @return a raster brick containing 6 raster layers
#' @noRd

get_regional_peak_wind <- function(hur_id, mm, width, time_step, water, timing) {
  # read land-water file
  cwd <- getwd()
  land_water_file <- paste(cwd, "/input/land_water.tif", sep="")
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
  gg <- matrix(0, nrows, ncols)  # duration of gale force winds (minutes)
  hh <- matrix(0, nrows, ncols)  # duration of hurricane winds (minutes)
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
  wmax <- get_maximum_wind_speed(hur_id)
  
  # get maximum range for gale winds
  range_maximum <- get_maximum_range(wmax, rmw, s_par)

  # record total elasped time if timing is TRUE
  if (timing == TRUE) start_time <- Sys.time()

  # calculate peak wind speed & direction and gale & hurricane duration for each location
  for (i in 1:nrows) {
    for (j in 1:ncols) {
      # get cover type from land_water layer
      cover_type <- land_water_matrix[nrows-i+1, j]

      if (cover_type == 2 || water == TRUE) {
        # get site latitude & longitude
        site_longitude <- lon_min + (j - 0.5)*cell_x
        site_latitude <- lat_min + (i - 0.5)*cell_y

        # get fixed parameter values
        asymmetry_factor <- asymmetry[cover_type]
        inflow_angle <- inflow[cover_type]
        friction_factor <- friction[cover_type]
        gust_factor <- gust[cover_type]

        for (k in 1:nrow(mm)) {
          hur_latitude  <- mm$latitude[k]
          hur_longitude <- mm$longitude[k]
  
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
              mm$hur_bear[k], mm$hur_spd[k], mm$wind_max[k], rmw, s_par, asymmetry_factor, 
              friction_factor)

            # update values if gale or higher
            if (wspd >= 17.5) {
              # update duration of gale force winds (minutes)
              gg[(nrows-i+1), j] <- gg[(nrows-i+1), j] + time_step
 
              # update duration of hurricane winds (minutes)
              if (wspd >= 33) {
                hh[(nrows-i+1), j] <- hh[(nrows-i+1), j] + time_step
              }

              # update peak values
              if (xx[(nrows-i+1), j] < wspd) {
                xx[(nrows-i+1), j] <- wspd

                ss[(nrows-i+1), j] <- as.integer(round(wspd))

                # wind direction (degrees)
                wdir <- calculate_wind_direction(mm$latitude[k], site_bear, inflow_angle)
                dd[(nrows-i+1), j] <- as.integer(round(wdir))
              }
            }
          }
        }
      }
    }
      
    # report progress
    if (timing == TRUE) {
      x <- round(i*100/nrows)
      if (x %% 10 == 0) cat("\r", x, "%")
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
          if (cdir > 8) cdir <- 1
          cc[(nrows-i+1), j] <- cdir
        }
      }
    }
  }

  # add a zero value so wind compass colors match for water and no water
  cc[1, ncols] <- 0

  # create raster layers
  ss_raster = raster::raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, 
    ymn=lat_min, ymx=lat_max, vals=ss)
  
  ff_raster = raster::raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, 
    ymn=lat_min, ymx=lat_max, vals=ff)
  
  dd_raster = raster::raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, 
    ymn=lat_min, ymx=lat_max, vals=dd)
  
  cc_raster = raster::raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, 
    ymn=lat_min, ymx=lat_max, vals=cc)

  gg_raster = raster::raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, 
    ymn=lat_min, ymx=lat_max, vals=gg)

  hh_raster = raster::raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, 
    ymn=lat_min, ymx=lat_max, vals=hh)

  # create raster brick
  hur_brick = raster::brick(ss_raster, ff_raster, dd_raster, cc_raster, gg_raster, hh_raster)

  # report elapsed time
  if (timing == TRUE) {
    elapsed_time <- format_time_difference_hms(start_time, Sys.time())
    cat("\r", elapsed_time, "\n")
  }

  return(hur_brick)
}

#' get_regional_summary_csv compiles regional results for all hurricanes.
#' Results are returned as a data frame of hurricane ids and maximum enhanced 
#' Fujita scale values.
#' @return data frame of summary values
#' @noRd

get_regional_summary_csv <- function() {
  # get current working directory
  cwd <- getwd()

  # read ids file
  ids_file <- paste(cwd, "/input/ids.csv", sep="")
  check_file_exists(ids_file)
  ii <- read.csv(ids_file, header=TRUE, stringsAsFactors=FALSE)
  names(ii)[1] <- "hur_id"
  ii_rows <- nrow(ii)

  # create data frame for peak Fujita value across region
  kk <- data.frame(hur_id=character(ii_rows), efmax=numeric(ii_rows), 
    stringsAsFactors=FALSE)

  # record values for each hurricane
  for (i in 1:ii_rows) {
    # get hurricane name
    hur_id <- ii$hur_id[i]

    # read regional hurricane file in GeoTiff format
    hur_brick_file <- paste(cwd, "/region-all/", hur_id, ".tif", sep="")
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
#' @return raster brick of summary values
#' @noRd

get_regional_summary_tif <- function() {
  # get current working directory
  cwd <- getwd()

  # read ids file
  ids_file <- paste(cwd, "/input/ids.csv", sep="")
  check_file_exists(ids_file)
  ii <- read.csv(ids_file, header=TRUE, stringsAsFactors=FALSE)
  names(ii)[1] <- "hur_id"
  ii_rows <- nrow(ii)

  # read land-water file
  land_water_file <- paste(cwd, "/input/land_water.tif", sep="")
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
    hur_brick_file <- paste(cwd, "/region-all/", hur_id, ".tif", sep="")
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

#' get_track_lat_lon returns a data frame of track data for the specified hurricane
#' if the maximum enhanced Fujita value exceeds a specified value.
#' @param hur_id hurricane id
#' @param fuj_min minimum enhanced Fujita value
#' @param tt a data frame of track data
#' @param kk a data frame of summary data
#' @return a data frame of track data
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
#' @param hur_path path for current model runs
#' @return no return value
#' @export
#' @examples
#' @rdname utility

hurrecon_set_path <- function(hur_path) {
  if (hur_path == "") {
    cat("\nNeed to enter a path\n")
    stop()

  } else if (dir.exists(hur_path) == FALSE) {
    cat("\nPath does not exist\n")
    stop()
  }

  setwd(hur_path)
  cat("Path set to", hur_path, "\n")
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
#' @return no return value
#' @export
#' @rdname utility

hurrecon_create_land_water <- function(nrows, ncols, xmn, xmx, ymn, ymx) {
  # get current working directory
  cwd <- getwd()

  # create new raster
  raster1 <- raster::raster(nrows=nrows, ncols=ncols, xmn=xmn, xmx=xmx, ymn=ymn, ymx=ymx, vals=0)

  # read vector boundary file
  boundaries_file <- paste(cwd, "/vector/boundaries.shp", sep="")
  boundaries <- rgdal::readOGR(boundaries_file)

  # rasterize vector file
  raster2 <- raster::rasterize(boundaries, raster1)

  # read reclassify file
  reclassify_file <- paste(cwd, "/vector/reclassify.csv", sep="")
  rcl <- read.csv(reclassify_file, stringsAsFactors=FALSE)

  # convert to matrix
  rcl <- as.matrix(rcl, ncol=3, byrow=TRUE)

  # reclassify raster
  land_water <- raster::reclassify(raster2, rcl)

  # write to file
  land_water_file <- paste(cwd, "/input/land_water.tif", sep="")
  rgdal::setCPLConfigOption("GDAL_PAM_ENABLED", "FALSE")
  raster::writeRaster(land_water, land_water_file, overwrite=TRUE)

  # calculate cell dimensions in kilometers
  lat_avg <- (ymx + ymn)/2

  cell_height <- 111*(ymx-ymn)/nrows
  cell_width <- 111*(xmx-xmn)*cos(lat_avg*pi/180)/ncols

  cat("Cell height =", round(cell_height) , "kilometers\n")
  cat("Cell width  =", round(cell_width), "kilometers\n")
}

#' @description
#' hurrecon_reformat_hurdat2 reformats a HURDAT2 file from the National 
#' Hurricane Center for use with the HURRECON model. The input file is assumed
#' to be in space-delimited text format. Two output files are created on the
#' input subdirectory: hurdat2_ids.csv contains a list of hurricanes including 
#' id, name, number of positions, and peak sustained wind speed (meters/second)
#' hurdat2_tracks.csv contains full track information for each hurricane 
#' from HURDAT2 plus columns for standard datetime and Julian day with fraction.
#' @param hurdat2_file name of HURDAT2 file
#' @param path optional path for HURDAT2 file
#' @return no return value
#' @export
#' @rdname utility

hurrecon_reformat_hurdat2 <- function(hurdat2_file, path=NULL) {
  # output files
  ids_file <- "hurdat2_ids.csv"
  tracks_file <- "hurdat2_tracks.csv"

  if (!is.null(path)) {
    if (path[length(path)] != "/") path <- paste(path, "/", sep="")
    hurdat2_file <- paste(path, hurdat2_file, sep="")
    ids_file <- paste(path, ids_file, sep="")
    tracks_file <- paste(path, tracks_file, sep="")
  }

  # read hurdat2 file
  file_in <- file(hurdat2_file)
  hurdat <- readLines(file_in)
  nlines <- length(hurdat)

  # close hurdat2 file
  close(file_in)

  # create data frames
  ids <- data.frame(hur_id=character(nlines), name=character(nlines), positions=numeric(nlines),
    wind_peak=numeric(nlines), stringsAsFactors=FALSE)

  tracks <- data.frame(hur_id=character(nlines), name=character(nlines), date=character(nlines), 
    time=character(nlines), date_time=character(nlines), jd=numeric(nlines), 
    status=character(nlines), latitude=numeric(nlines), longitude=numeric(nlines), 
    wind_max=numeric(nlines), stringsAsFactors=FALSE)

  colnames(ids) <- c("hur_id", "name", "positions", "wind_peak")

  colnames(tracks) <- c("hur_id", "name", "date", "time", "date_time", "jd", "status", "latitude", 
    "longitude", "wind_max")

  # current line number in hurdat
  line_num <- 0
  # current row number in ids
  ids_index <- 0
  # current row number in tracks
  tracks_index <- 0

  while (line_num < nlines) {
    # get hurricane id, name, and number of positions
    line_num <- line_num + 1
    row <- strsplit(hurdat[line_num], ",")[[1]] 
    hur_id <- raster::trim(row[1])
    name <- raster::trim(row[2])
    positions <- as.numeric(raster::trim(row[3]))

    # process observations
    for (i in 1:positions) {
      line_num <- line_num + 1
      row <- strsplit(hurdat[line_num], ",")[[1]]
      date <- raster::trim(row[1])
      time <- raster::trim(row[2])
      status <- raster::trim(row[4])

      lat <- raster::trim(row[5])
      latitude <- as.numeric(substr(lat, 1, nchar(lat)-1))

      lon <- raster::trim(row[6])
      longitude <- -as.numeric(substr(lon, 1, nchar(lon)-1))

      wind_max <- as.numeric(raster::trim(row[7]))

      # convert knots to meters per second
      wind_max <- round(0.514444 * wind_max, 1)

      # get peak wind
      if (i == 1) {
        wind_peak <- wind_max
      } else {
        if (wind_peak < wind_max) wind_peak <- wind_max
      }

      date_time <- ""
      jd <- 0

      # add to tracks
      tracks_index <- tracks_index + 1
      tracks[tracks_index, ] <- c(hur_id, name, date, time, date_time, jd, status, latitude, longitude, wind_max)
    }

    ids_index <- ids_index + 1
    ids[ids_index, ] <- c(hur_id, name, positions, wind_peak)

    # report progress
    x <- round(line_num*100/nlines)
    if (x %% 10 == 0) cat("\r", x, "%")
  }

  # remove empty lines
  ids <- ids[(ids$hur_id != ""), ]
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
  write.csv(ids, ids_file, row.names=FALSE)
  write.csv(tracks, tracks_file, row.names=FALSE)

  # display number of storms
  cat("\nNumber of storms = ", ids_index, "\n")
  cat("Number of observations = ", tracks_index, "\n")
}

#' @description
#' hurrecon_extract_tracks extracts hurricane ids and tracks from the two
#' files created by hurrecon_reformat_hurdat2 (hurdat2_ids.csv and 
#' hurdat2_tracks.csv). The geographic window used to select hurricanes is 
#' set by the land-water file and optionally extended by the margin parameter.
#' Selection begins by identifying all positions in the window where the hurricane
#' has "HU" (hurricane) status in HURDAT2.  If at least one such position exists,
#' the track is extended to include one position before and one position after
#' the window, if possible. If the resulting track contains at least two positions 
#' and the maximum sustained wind speed equals or exceeds wind_min, the track 
#' is included.
#' @param margin an optional extension of the geographic window set by the
#' land-water file (degrees)
#' @param wind_min the minimum value of maximum sustained wind speed 
#' (meters/second)
#' @return no return value
#' @export
#' @rdname utility

hurrecon_extract_tracks <- function(margin=0, wind_min=33) {
  # get current working directory
  cwd <- getwd()

  # output files
  ids_file <- paste(cwd, "/input/ids.csv", sep="")
  tracks_file <- paste(cwd, "/input/tracks.csv", sep="")

  # read hurdat2 ids file
  hurdat2_ids_file <- paste(cwd, "/input/hurdat2_ids.csv", sep="")
  check_file_exists(hurdat2_ids_file)
  ii <- read.csv(hurdat2_ids_file, header=TRUE, stringsAsFactors=FALSE)
  ii_rows = nrow(ii)

  # read hurdat2 tracks file
  hurdat2_tracks_file <- paste(cwd, "/input/hurdat2_tracks.csv", sep="")
  check_file_exists(hurdat2_tracks_file)
  tt <- read.csv(hurdat2_tracks_file, header=TRUE, stringsAsFactors=FALSE)
  tt_rows = nrow(tt)

  # read land-water file
  land_water_file <- paste(cwd, "/input/land_water.tif", sep="")
  check_file_exists(land_water_file)
  land_water <- raster::raster(land_water_file)

  # get window coordinates
  lon_min <- raster::extent(land_water)[1] - margin
  lon_max <- raster::extent(land_water)[2] + margin
 
  lat_min <- raster::extent(land_water)[3] - margin
  lat_max <- raster::extent(land_water)[4] + margin

  # create data frames
  ids <- data.frame(hur_id=character(ii_rows), name=character(ii_rows), 
    positions=numeric(ii_rows), wind_peak=numeric(ii_rows), stringsAsFactors=FALSE)

  tracks <- data.frame(hur_id=character(tt_rows), name=character(tt_rows), 
    date_time=character(tt_rows), jd=numeric(tt_rows), status=character(tt_rows), 
    latitude=numeric(tt_rows), longitude=numeric(tt_rows), wind_max=numeric(tt_rows), 
    stringsAsFactors=FALSE)

  colnames(ids) <- c("hur_id", "name", "positions", "wind_peak")

  colnames(tracks) <- c("hur_id", "name", "date_time", "jd", "status", "latitude", 
    "longitude", "wind_max")

  # subset each track
  ids_index <- 0
  tracks_index <- 0

  for (i in 1:ii_rows) {
    # get hurricane id & name
    hur_id <- ii[i, "hur_id"]
    name <- ii[i, "name"]

    # check if in window
    index <- which(tt$hur_id == hur_id & tt$latitude >= lat_min & tt$latitude <= lat_max & tt$longitude >= lon_min & tt$longitude <= lon_max & tt$status == "HU")

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
      
      # store id & tracks if at least 2 positions & exceeds minimum wind speed
      if (positions > 1 && wind_peak >= wind_min) {
        ids_index <- ids_index + 1
        ids[ids_index, ] <- c(hur_id, name, positions, wind_peak)
      
        for (j in 1:nrow(xx)) {
          tracks_index <- tracks_index + 1
          tracks[tracks_index, ] <- xx[j, ]
        }
      }
    }

    # report progress
    x <- round(i*100/ii_rows)
    if (x %% 10 == 0) cat("\r", x, "%")
  }

  # remove empty lines
  ids <- ids[(ids$hur_id != ""), ]
  tracks <- tracks[(tracks$hur_id != ""), ]
  
  # save to file
  write.csv(ids, ids_file, row.names=FALSE)
  write.csv(tracks, tracks_file, row.names=FALSE)

  # display number of storms
  cat("\nNumber of storms = ", nrow(ids), "\n")
  cat("Number of observations = ", nrow(tracks), "\n")
}


### MODELING FUNCTIONS ####################################

#' @title
#' Modeling Functions
#' @description
#' hurrecon_model_site calculates wind speed (meters/second), gust speed 
#' (meters/second), wind direction (degrees), and enhanced Fujita scale wind 
#' damage for a given hurricane and site. If width is TRUE, the radius of 
#' maximum wind (rmw) and profile exponent (s_par) for the given hurricane are
#' used, if available. If save is TRUE, results are saved to a CSV file on 
#' the site subdirectory; otherwise results are returned as a data frame.  
#' If timing is TRUE, the total elasped time is displayed.
#' @param hur_id hurricane id
#' @param site_name name of site
#' @param width whether to use width parameters for the specified hurricane
#' @param time_step time step (minutes)
#' @param save whether to save results to a CSV file
#' @param timing whether to display total elapsed time
#' @return a data frame of results if save is FALSE
#' @export
#' @examples
#' @rdname modeling

hurrecon_model_site <- function(hur_id, site_name, width=FALSE, time_step=1, save=TRUE, 
  timing=TRUE) { 

  # record total elapsed time if timing is TRUE
  if (timing == TRUE) start_time <- Sys.time()

  # get current working directory
  cwd <- getwd()

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
  track <- read_hurricane_track_file(hur_id)
  
  # interpolate hurricane location & max wind speed
  modeled <- interpolate_hurricane_location_max_wind(track, time_step)
  
  # interpolate hurricane speed & bearing
  modeled <- interpolate_hurricane_speed_bearing(track, modeled)
  
  # calculate range & bearing from site to hurricane center
  modeled <- calculate_site_range_bearing(modeled, site_latitude, site_longitude)
  
  # calculate wind speed, wind direction & enhanced Fujita scale at site
  modeled <- calculate_wind_speed_direction(modeled, inflow_angle, cover_type, rmw, 
    s_par, asymmetry_factor, friction_factor, gust_factor)
  
  # add standard date & time
  modeled <- add_standard_date_time(modeled)
  
  # display total elapsed time
  if (timing == TRUE) cat(format_time_difference_ms(start_time, Sys.time()), "ms\n")

  # output
  if (save == TRUE) {
    # save modeled data to CSV file
    modeled_file <- paste(cwd, "/site/", hur_id, " ", site_name, ".csv", sep="")
    write.csv(modeled, modeled_file, quote=FALSE, row.names=FALSE)
    cat("Saving to", modeled_file, "\n")
  
  } else {
    # return modeled data as data frame
    return(modeled)
  }
}

#' @description
#' hurrecon_model_site_all creates a table of peak values for all hurricanes
#' for a given site. If width is TRUE, the radius of maximum wind (rmw) and 
#' profile exponent (s_par) for the given hurricane are used, if available. 
#' If save is TRUE, results are saved to a CSV file on the site-all subdirectory; 
#' otherwise results are returned as a data frame.  If timing is TRUE, the 
#' total elasped time is displayed.
#' @param site_name name of site
#' @param width whether to use width parameters for the specified hurricane
#' @param time_step time step (minutes)
#' @param save whether to save results to a CSV file
#' @param timing whether to display total elapsed time
#' @return a data frame of results if save is FALSE
#' @export
#' @rdname modeling

hurrecon_model_site_all <- function(site_name, width=FALSE, time_step=1, save=TRUE,
    timing=TRUE) {

  # get current working directory
  cwd <- getwd()

  # read ids file
  ids_file <- paste(cwd, "/input/ids.csv", sep="")
  check_file_exists(ids_file)
  ii <- read.csv(ids_file, header=TRUE, stringsAsFactors=FALSE)
  names(ii)[1] <- "hur_id"
  ii_rows <- nrow(ii)

  # create data frame for peak values
  peak_values <- data.frame(site_name=character(ii_rows), hur_id=character(ii_rows), 
    date_time=character(ii_rows), wind_dir=numeric(ii_rows), wind_spd=numeric(ii_rows), 
    gust_spd=numeric(ii_rows), ef_sca=numeric(ii_rows), ef0=numeric(ii_rows), 
    ef1=numeric(ii_rows), ef2=numeric(ii_rows), ef3=numeric(ii_rows), ef4=numeric(ii_rows), 
    ef5=numeric(ii_rows), stringsAsFactors=FALSE)

  # record total elasped time if timing is TRUE
  if (timing == TRUE) start_time <- Sys.time()

  # get peak values for each hurricane
  for (i in 1:ii_rows) {
    # get hurricane name
    hur_id = ii$hur_id[i]

    # get modeled output
    mm <- hurrecon_model_site(hur_id, site_name, width, time_step, save=FALSE, 
      timing=FALSE)

    # get peak values
    pk <- get_peak_values(hur_id, site_name, mm)
    
    peak_values$site_name[i] <- as.character(pk$site_name[1])
    peak_values$hur_id[i] <- as.character(pk$hur_id[1])
    peak_values$date_time[i] <- as.character(pk$date_time[1])
    peak_values$wind_dir[i] <- pk$wind_dir[1]
    peak_values$wind_spd[i] <- pk$wind_spd[1]
    peak_values$gust_spd[i] <- pk$gust_spd[1]
    peak_values$ef_sca[i] <- pk$ef_sca[1]
    peak_values$ef0[i] <- pk$ef0[1]
    peak_values$ef1[i] <- pk$ef1[1]
    peak_values$ef2[i] <- pk$ef2[1]
    peak_values$ef3[i] <- pk$ef3[1]
    peak_values$ef4[i] <- pk$ef4[1]
    peak_values$ef5[i] <- pk$ef5[1]
    
    # report progress
    if (timing == TRUE) {
      x <- round(i*100/ii_rows)
      if (x %% 10 == 0) cat("\r", x, "%")
    }
  }

  if (timing == TRUE) {
    elapsed_time <- format_time_difference_hms(start_time, Sys.time())
    cat("\r", elapsed_time, "\n")
  }

  # output
  if (save == TRUE) {
    # save modeled data to CSV file
    site_peak_file = paste(cwd, "/site-all/", site_name, " Peak Values.csv", sep="")
    write.csv(peak_values, site_peak_file, quote=FALSE, row.names=FALSE)
    cat("Saving to", site_peak_file)
  
  } else {
    # return modeled data as data frame
    return(peak_values)
  }
}

#' @description
#' hurrecon_model_region calculates peak wind speed (meters/second), enhanced
#' Fujita scale, wind direction (degrees), cardinal wind direction, gale wind
#' duration (minutes), and hurricane wind duration (minutes) for a given 
#' hurricane over a region. If width is TRUE, the radius of maximum wind (rmw) 
#' and profile exponent (s_par) for the given hurricane are used, if available.
#' If time_step is NULL, the time step is calculated. If water is FALSE, results
#' are calculated for land areas only. If save is TRUE, results are saved as a 
#' GeoTiff file on the region subdirectory; otherwise results are returned as
#' a raster brick with 6 layers. If timing is TRUE, the total elasped time 
#' is displayed.
#' @param hur_id hurricane id
#' @param width whether to use width parameters for the specified hurricane
#' @param time_step time step (minutes)
#' @param water whether to caculate results over water
#' @param save whether to save results to a GeoTiff file
#' @param timing whether to display total elapsed time
#' @return a brick of 6 rasters if save is FALSE
#' @export
#' @rdname modeling

hurrecon_model_region <- function(hur_id, width=FALSE, time_step=NULL, water=FALSE, 
  save=TRUE, timing=TRUE) {

  # get current working directory
  cwd <- getwd()
 
  # get time step if necessary
  if (is.null(time_step)) {
    time_step <- get_time_step()
  }

  if (timing) {
    cat("Time step =", time_step, "minutes\n")
  }

  # read hurricane track file
  track <- read_hurricane_track_file(hur_id)

  # interpolate hurricane location & max wind speed
  modeled <- interpolate_hurricane_location_max_wind(track, time_step)

  # interpolate hurricane speed & bearing
  modeled <- interpolate_hurricane_speed_bearing(track, modeled)

  # get modeled values over region
  hur_brick <- get_regional_peak_wind(hur_id, modeled, width, time_step, water, timing)

  # output
  if (save == TRUE) {
    # save modeled values in a Geotiff file
    hur_tif_file = paste(cwd, "/region/", hur_id, ".tif", sep="")
    rgdal::setCPLConfigOption("GDAL_PAM_ENABLED", "FALSE")
    raster::writeRaster(hur_brick, hur_tif_file, overwrite=TRUE)

  } else {
    # return modeled values as raster brick
    return(hur_brick)
  }
}

#' @description
#' hurrecon_model_region_all calculates peak wind speed (meters/second), 
#' enhanced Fujita scale, wind direction (degrees), cardinal wind direction, 
#' duration of gale winds (minutes), and duration of hurricane winds (minutes) 
#' over a region for all hurricanes. If width is TRUE, the radius of maximum 
#' wind (rmw) and profile exponent (s_par) for the given hurricane are used, 
#' if available. If time_step is NULL, the time step is calculated. If water 
#' is FALSE, results are calculated for land areas only. Results for each 
#' hurricane are saved in a GeoTiff file on the region-all subdirectory. 
#' Summary results for all hurricanes (summary.csv, summary.tif) are also 
#' calculated and saved to the region-all subdirectory.
#' @param width whether to use width parameters for the specified hurricane
#' @param time_step time step (minutes)
#' @param water whether to calculate results over water
#' @return no return value
#' @export
#' @rdname modeling

hurrecon_model_region_all <- function(width=FALSE, time_step=NULL, water=FALSE) {
  # get current working directory
  cwd <- getwd()

  # get time step if necessary
  if (is.null(time_step)) {
    time_step <- get_time_step()
  }

  cat("Time step =", time_step, "minutes\n")

  # read ids file
  ids_file <- paste(cwd, "/input/ids.csv", sep="")
  check_file_exists(ids_file)
  ii <- read.csv(ids_file, header=TRUE, stringsAsFactors=FALSE)
  names(ii)[1] <- "hur_id"
  ii_rows <- nrow(ii)

  # record total elasped time
  start_time <- Sys.time()

  # get regional estimate for each hurricane
  for (i in 1:ii_rows) {
    # get hurricane name
    hur_id <- ii$hur_id[i]

    # report progress
    x <- round(i*100/ii_rows)
    cat("\r", x, "%")

    # get modeled values over region
    hur_brick <- hurrecon_model_region(hur_id, width, time_step, water, 
      save=FALSE, timing=FALSE)

    # save modeled values in a Geotiff file
    hur_tif_file = paste(cwd, "/region-all/", hur_id, ".tif", sep="")
    rgdal::setCPLConfigOption("GDAL_PAM_ENABLED", "FALSE")
    raster::writeRaster(hur_brick, hur_tif_file, overwrite=TRUE)
  }

  # get & save summary.csv file
  kk <- get_regional_summary_csv()
  peak_file <- paste(cwd, "/region-all/summary.csv", sep="")
  write.csv(kk, peak_file, row.names=FALSE)

  # get & save summary.tif file
  sum_brick <- get_regional_summary_tif()
  sum_brick_file <- paste(cwd, "/region-all/summary.tif", sep="")
  rgdal::setCPLConfigOption("GDAL_PAM_ENABLED", "FALSE")
  raster::writeRaster(sum_brick, sum_brick_file, overwrite=TRUE)

  # display total elapsed time
  elapsed_time <- format_time_difference_hms(start_time, Sys.time())
  cat("\r", elapsed_time, "\n")
}


### SUMMARIZING FUNCTIONS #################################

#' @title
#' Summarizing Functions
#' @description
#' hurrecon_summarize_land_water displays features of the current land-water
#' file (land_water.tif) in the console.
#' @return no return value
#' @export
#' @examples
#' @rdname summarizing

hurrecon_summarize_land_water <- function() {
  # get current working directory
  cwd <- getwd()

  # read land-water file
  land_water_file <- paste(cwd, "/input/land_water.tif", sep="")
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

  cat("Rows:", nrows, "  Columns:", ncols, "\n")
  cat("Latitude:", round(ymn, 1), "to", round(ymx, 1), "degrees\n")
  cat("Longitude:", round(xmn, 1), "to", round(xmx, 1), "degrees\n")
  cat("Cell height:", round(cell_height), "kilometers\n")
  cat("Cell width:", round(cell_width), "kilometers\n")
  cat("Time Step:", time_step, "minutes\n")
}

#' @description
#' hurrecon_summarize_tracks displays features of the current ids file 
#' (ids.csv) in the console.
#' @return no return value
#' @export
#' @rdname summarizing

hurrecon_summarize_tracks <- function() {
  # get current working directory
  cwd <- getwd()

  # read ids file
  ids_file <- paste(cwd, "/input/ids.csv", sep="")
  check_file_exists(ids_file)
  ii <- read.csv(ids_file, header=TRUE, stringsAsFactors=FALSE)
  ii_rows = nrow(ii)

  positions_total = sum(ii$positions)

  wind_peak_min = min(ii$wind_peak)
  wind_peak_max = max(ii$wind_peak)

  cat("Number of storms =", ii_rows, "\n")
  cat("Number of positions =", positions_total, "\n")
  cat("Minimum peak wind =", wind_peak_min, "m/s\n")
  cat("Maximum peak wind =", wind_peak_max, "m/s\n")
}

#' @description
#' hurrecon_summarize_site displays peak values for a given hurricane
#' and site in the console.
#' @param hur_id hurricane id
#' @param site_name name of site
#' @return no return value
#' @export
#' @rdname summarizing

hurrecon_summarize_site <- function(hur_id, site_name) {
  # get current working directory
  cwd <- getwd()

  # read data
  modeled_name <- paste(hur_id, site_name)
  modeled_file <- paste(cwd, "/site/", hur_id, " ", site_name, ".csv", sep="")
  check_file_exists(modeled_file)
  mm <- read.csv(modeled_file, header=TRUE, stringsAsFactors=FALSE)

  # get peak values
  pk <- get_peak_values(hur_id, site_name, mm)

  # print peak values
  cat(modeled_name, "\n")

  cat("PEAK:", as.character(pk$date_time[1]), "\n")
  cat("Wind dir:", round(pk$wind_dir[1], 0), "deg\n")
  cat("Wind spd:", round(pk$wind_spd[1], 0), "m/s\n")
  cat("Gust spd:", round(pk$gust_spd[1], 0), "m/s\n")

  if (pk$ef0[1] > 0) cat("EF0:", round(pk$ef0[1], 1), "hours\n")
  if (pk$ef1[1] > 0) cat("EF1:", round(pk$ef1[1], 1), "hours\n")
  if (pk$ef2[1] > 0) cat("EF2:", round(pk$ef2[1], 1), "hours\n")
  if (pk$ef3[1] > 0) cat("EF3:", round(pk$ef3[1], 1), "hours\n")
  if (pk$ef4[1] > 0) cat("EF4:", round(pk$ef4[1], 1), "hours\n")
  if (pk$ef5[1] > 0) cat("EF5:", round(pk$ef5[1], 1), "hours\n")
}


### PLOTTING FUNCTIONS ####################################

#' @title
#' Plotting Functions
#' @description
#' hurrecon_plot_site_ts creates a time-series plot of wind speed, gust 
#' speed, or wind direction as a function of datetime for a given 
#' hurricane and site. Optional start and end datetimes may be specified.
#' Variables to plot: wind_speed, gust_speed, or wind_direction.
#' @param hur_id hurricane id
#' @param site_name name of site
#' @param start_datetime optional start datetime (YYYY-MM-DD hh:mm)
#' @param end_datetime optional end datetime (YYYY-MM-DD hh:mm)
#' @param var variable to plot
#' @return no return value
#' @export
#' @examples
#' @rdname plotting

hurrecon_plot_site_ts <- function(hur_id, site_name, start_datetime='', 
  end_datetime='', var="wind_speed") {

  # get current working directory
  cwd <- getwd()

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
  
  # read data
  modeled_file <- paste(cwd, "/site/", hur_id, " ", site_name, ".csv", sep="")
  check_file_exists(modeled_file)
  mm <- read.csv(modeled_file, header=TRUE, stringsAsFactors=FALSE)
  mm_rows <- nrow(mm)

  # add datetime
  mm$date <- substr(mm$date_time, 1, 10)
  mm$time <- paste(substr(mm$date_time, 12, 16), ":00", sep="")
  mm$dt <- as.POSIXct(paste(mm$date, " ", mm$time, sep=""))

  # get axis labels
  x_var <- "dt"
  x_label <- paste("Datetime (UTC)", sep="")

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
    cat("\nvar must be wind_speed, gust_speed, or wind_direction\n")
    stop()
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

  main_label <- paste(hur_id, site_name)

  # create plot
  par(mar=c(5.1, 5.6, 4.1, 2.1))

  plot(xaxt="n", type="n", xlim, ylim, cex.main=1.7, cex.lab=1.7, xlab=x_label, 
    ylab=y_label, main=main_label) 

  axis.POSIXct(1, mm_plot$dt, format="%m-%d %H")

  points(mm_plot_efx[c(x_var, y_var)], pch=16, cex=1.0, col=efx_col)
  points(mm_plot_ef0[c(x_var, y_var)], pch=16, cex=1.0, col=ef0_col)
  points(mm_plot_ef1[c(x_var, y_var)], pch=16, cex=1.0, col=ef1_col)
  points(mm_plot_ef2[c(x_var, y_var)], pch=16, cex=1.0, col=ef2_col)
  points(mm_plot_ef3[c(x_var, y_var)], pch=16, cex=1.0, col=ef3_col)
  points(mm_plot_ef4[c(x_var, y_var)], pch=16, cex=1.0, col=ef4_col)
  points(mm_plot_ef5[c(x_var, y_var)], pch=16, cex=1.0, col=ef5_col)

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

  legend("topright", NULL, labs, cols, cex=0.7)
}

#' @description
#' hurrecon_plot_site_xy creates a scatter plot of wind speed or gust speed
#' as a function of wind direction for a given hurricane and site. If adjust 
#' is TRUE, 360 degrees are substracted from wind direction values greater 
#' than 180. Optional start and end datetimes may be specified. Variables to
#' plot: wind_speed or gust_speed.
#' @param hur_id hurricane id
#' @param site_name name of site
#' @param start_datetime optional start datetime in format YYYY-MM-DD hh:mm
#' @param end_datetime optional end datetime in format YYYY-MM-DD hh:mm
#' @param var variable to plot
#' @param adjust whether to subtract 360 degrees from wind direction.
#' @return no return value
#' @export
#' @rdname plotting

hurrecon_plot_site_xy <- function(hur_id, site_name, start_datetime='', 
  end_datetime='', var="wind_speed", adjust=FALSE) {

  # get current working directory
  cwd <- getwd()

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
 
  # read data
  modeled_file <- paste(cwd, "/site/", hur_id, " ", site_name, ".csv", sep="")
  check_file_exists(modeled_file)
  mm <- read.csv(modeled_file, header=TRUE, stringsAsFactors=FALSE)
  mm_rows <- nrow(mm)

  # add datetime
  mm$date <- substr(mm$date_time, 1, 10)
  mm$time <- paste(substr(mm$date_time, 12, 16), ":00", sep="")
  mm$dt <- as.POSIXct(paste(mm$date, " ", mm$time, sep=""))

  # get axis labels
  x_var <- "wind_dir"
  x_label <- "Wind Direction (deg)"

  if (var == "wind_speed") {
    y_var <- "wind_spd"
    y_label <- "Wind Speed (m/s)"
  } else if (var == "gust_speed") {
    y_var <- "gust_spd"
    y_label <- "Gust Speed (m/s)"
  } else {
    cat("\nvar must be wind_speed or gust_speed\n")
    stop()
  }

  # adjust wind direction
  if (adjust == TRUE) {
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

  main_label <- paste(hur_id, site_name)

  # create plot
  par(mar=c(5.1, 5.6, 4.1, 2.1))

  plot(type="n", xlim, ylim, cex.main=1.7, cex.lab=1.7, xlab=x_label, 
    ylab=y_label, main=main_label)

  points(mm_plot_efx[c(x_var, y_var)], pch=16, cex=1.0, col=efx_col)
  points(mm_plot_ef0[c(x_var, y_var)], pch=16, cex=1.0, col=ef0_col)
  points(mm_plot_ef1[c(x_var, y_var)], pch=16, cex=1.0, col=ef1_col)
  points(mm_plot_ef2[c(x_var, y_var)], pch=16, cex=1.0, col=ef2_col)
  points(mm_plot_ef3[c(x_var, y_var)], pch=16, cex=1.0, col=ef3_col)
  points(mm_plot_ef4[c(x_var, y_var)], pch=16, cex=1.0, col=ef4_col)
  points(mm_plot_ef5[c(x_var, y_var)], pch=16, cex=1.0, col=ef5_col)

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

  legend("topright", NULL, labs, cols, cex=0.7)
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
#' @return no return value
#' @export
#' @rdname plotting

hurrecon_plot_site_all <- function(site_name, start_year='', end_year='', 
  var="wind_speed") {

  # get current working directory
  cwd <- getwd()

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

  # read data
  peak_file <- paste(cwd, "/site-all/", site_name, " Peak Values.csv", sep="")
  check_file_exists(peak_file)
  kk <- read.csv(peak_file, header=TRUE, stringsAsFactors=FALSE)
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
    cat("\nvar must be wind_speed, gust_speed, or wind_direction\n")
    stop()
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

  main_label <- site_name
 
  # create plot
  par(mar=c(5.1, 5.6, 4.1, 2.1))

  plot(type="n", xlim, ylim, cex.main=1.7, cex.lab=1.7, xlab=x_label, 
    ylab=y_label, main=main_label)

  points(kk_plot_efx[c(x_var, y_var)], pch=16, cex=1.5, col=efx_col)
  points(kk_plot_ef0[c(x_var, y_var)], pch=16, cex=1.5, col=ef0_col)
  points(kk_plot_ef1[c(x_var, y_var)], pch=16, cex=1.5, col=ef1_col)
  points(kk_plot_ef2[c(x_var, y_var)], pch=16, cex=1.5, col=ef2_col)
  points(kk_plot_ef3[c(x_var, y_var)], pch=16, cex=1.5, col=ef3_col)
  points(kk_plot_ef4[c(x_var, y_var)], pch=16, cex=1.5, col=ef4_col)
  points(kk_plot_ef5[c(x_var, y_var)], pch=16, cex=1.5, col=ef5_col)

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

  legend("bottomleft", NULL, labs, cols, cex=0.7)
}

#' @description
#' hurrecon_plot_region creates regional plots of enhanced Fujita scale, 
#' peak wind speed, wind direction, cardinal wind direction, gale wind 
#' duration, and hurricane wind duration for a given hurricane. Variables
#' to plot: wind_speed, fujita_scale, wind_direction, wind_compass, 
#' gale_duration, or hurricane_duration.
#' @param hur_id hurricane id
#' @param var variable to plot
#' @return no return value
#' @export
#' @rdname plotting

hurrecon_plot_region <- function(hur_id, var="fujita_scale") {
  # get current working directory
  cwd <- getwd()
 
  # read raster brick file in GeoTiff format
  hur_tif_file = paste(cwd, "/region/", hur_id, ".tif", sep="")
  check_file_exists(hur_tif_file)
  hur_brick <- raster::brick(hur_tif_file)

  # get individual layers
  ss_layer <- raster::subset(hur_brick, 1)  # wind speed (m/s)
  ff_layer <- raster::subset(hur_brick, 2)  # enhanced Fujita scale
  dd_layer <- raster::subset(hur_brick, 3)  # wind direction (degrees)
  cc_layer <- raster::subset(hur_brick, 4)  # cardinal wind direction (1-8)
  gg_layer <- raster::subset(hur_brick, 5)  # duration of gale force winds (hours)
  hh_layer <- raster::subset(hur_brick, 6)  # duration of hurricane winds (hours)

  # get vector boundary file
  boundaries_file <- paste(cwd, "/vector/boundaries.shp", sep="")
  check_file_exists(boundaries_file)
  boundaries <- rgdal::readOGR(boundaries_file)

  # get hurricane track
  track_file <- paste(cwd, "/input/tracks.csv", sep="")
  check_file_exists(track_file)
  zz <-read.csv(track_file, header=TRUE, stringsAsFactors=FALSE)
  names(zz)[1] <- "hur_id"
  index <- which(zz$hur_id == hur_id)
  tt <- zz[index, ]

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

  # set titles
  xlab <- "Longitude (degrees)"
  ylab <- "Latitude (degrees)"

  # create plot
  par(mar=c(5.1, 4.6, 4.1, 2.1))
  
  if (var == "fujita_scale") {
    if (raster::maxValue(ff_layer) > 0) {
      main_label <- paste(hur_id, "Fujita Scale")
      arg <- list(at=ff_vals, labels=ff_labs)
      raster::plot(ff_layer, xlab=xlab, ylab=ylab, main=main_label, axis.args=arg, col=ff_cols)
      raster::plot(boundaries, add=TRUE)
      lines(tt$longitude, tt$latitude, col="grey")
    } else cat("\nNo Fujita values\n")   

  } else if (var == "wind_speed") {
    if (raster::maxValue(ss_layer) > 0) {
      main_label <- paste(hur_id, "Wind Speed (m/s)")
      raster::plot(ss_layer, xlab=xlab, ylab=ylab, main=main_label)
      raster::plot(boundaries, add=TRUE)
      lines(tt$longitude, tt$latitude, col="grey")
    } else cat("\nNo wind speed\n")   

  } else if (var == "wind_direction") {
    if (raster::maxValue(dd_layer) > 0) {
      main_label <- paste(hur_id, "Wind Direction (deg)")
      raster::plot(dd_layer, xlab=xlab, ylab=ylab, main=main_label)
      raster::plot(boundaries, add=TRUE)
      lines(tt$longitude, tt$latitude, col="grey")
    } else cat("\nNo wind direction\n")   
    
  } else if (var == "wind_compass") {
    if (raster::maxValue(cc_layer) > 0) {
      main_label <- paste(hur_id, "Wind Direction")
      arg <- list(at=c(0,1,2,3,4,5,6,7,8), labels=c("","N","NE","E","SE","S","SW","W","NW"))
      cols=rainbow(9)
      cols[1] <- "white"
      raster::plot(cc_layer, xlab=xlab, ylab=ylab, main=main_label, axis.args=arg, col=cols)
      raster::plot(boundaries, add=TRUE)
      lines(tt$longitude, tt$latitude, col="grey")
    } else cat("\nNo wind compass\n")   

  } else if (var == "gale_duration") {
    if (raster::maxValue(gg_layer) > 0) {
      main_label <- paste(hur_id, "Gale Winds (min)")
      raster::plot(gg_layer, xlab=xlab, ylab=ylab, main=main_label)
      raster::plot(boundaries, add=TRUE)
      lines(tt$longitude, tt$latitude, col="grey")
    } else cat("\nNo gale winds\n")   

  } else if (var == "hurricane_duration") {
    if (raster::maxValue(hh_layer) > 0) {
      main_label <- paste(hur_id, "Hurricane Winds (min)")
      raster::plot(hh_layer, xlab=xlab, ylab=ylab, main=main_label)
      raster::plot(boundaries, add=TRUE)
      lines(tt$longitude, tt$latitude, col="grey")
    } else cat("\nNo hurricane winds\n")   

  } else {
    cat("\nvar must be wind_speed, fujita_scale, wind_direction, wind_compass, gale_duration, or hurricane_duration\n")
    stop()
  }
}

#' @description
#' hurrecon_plot_region_all creates regional plots of maximum enhanced Fujita
#' value and number of storms for each enhanced Fujita value for all hurricanes.
#' Variables to plot: efmax, ef0, ef1, ef2, ef3, ef4, or ef5.
#' @param var variable to plot
#' @param tracks whether to also plot hurricane tracks
#' @return no return value
#' @export
#' @rdname plotting

hurrecon_plot_region_all <- function(var="efmax", tracks=FALSE) {
  # get current working directory
  cwd <- getwd()
 
  # read summary file in GeoTiff format
  sum_tif_file = paste(cwd, "/region-all/", "summary.tif", sep="")
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
  boundaries_file <- paste(cwd, "/vector/boundaries.shp", sep="")
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

  # set titles
  xlab <- "Longitude (degrees)"
  ylab <- "Latitude (degrees)"

  # get hurricane tracks
  if (tracks) {
    ids_file <- paste(cwd, "/input/ids.csv", sep="")
    check_file_exists(ids_file)
    ii <- read.csv(ids_file, header=TRUE, stringsAsFactors=FALSE)
    names(ii)[1] <- "hur_id"

    tracks_file <- paste(cwd, "/input/tracks.csv", sep="")
    check_file_exists(tracks_file)
    tt <- read.csv(tracks_file, header=TRUE, stringsAsFactors=FALSE)
    names(tt)[1] <- "hur_id"

    summary_file <- paste(cwd, "/region-all/summary.csv", sep="")
    check_file_exists(summary_file)
    kk <- read.csv(summary_file, header=TRUE, stringsAsFactors=FALSE)
    names(kk)[1] <- "hur_id"
  }
  
  # create plot
  par(mar=c(5.1, 4.6, 4.1, 2.1))

  if (var == "efmax") {
    if (raster::maxValue(efm_layer) > 0) {
      arg <- list(at=efm_vals, labels=efm_labs)
      raster::plot(efm_layer, xlab=xlab, ylab=ylab, main="Maximum Fujita Scale", axis.args=arg, col=efm_cols)
      raster::plot(boundaries, add=TRUE)
      if (tracks) {
        for (i in 1:nrow(ii)) {
          hur_id <- ii[i, "hur_id"]
          fuj_min <- 0
          xx <- get_track_lat_lon(hur_id, fuj_min, tt, kk)
          if (!is.null(xx)) {
            lines(xx$longitude, xx$latitude, col="grey")
          }         
        }
      }
    } else cat("\nNo Fujita values\n")

  } else if (var == "ef0") {
    if (raster::maxValue(ef0_layer) > 0) {
      raster::plot(ef0_layer, xlab=xlab, ylab=ylab, main="Fujita Scale 0 (storms)")
      raster::plot(boundaries, add=TRUE)
      if (tracks) {
        for (i in 1:nrow(ii)) {
          hur_id <- ii[i, "hur_id"]
          fuj_min <- 0
          xx <- get_track_lat_lon(hur_id, fuj_min, tt, kk)
          if (!is.null(xx)) {
            lines(xx$longitude, xx$latitude, col="grey")
          }         
        }
      }
    } else cat("\nNo F0 values\n")
  
  } else if (var == "ef1") {
    if (raster::maxValue(ef1_layer) > 0) {
      raster::plot(ef1_layer, xlab=xlab, ylab=ylab, main="Fujita Scale 1 (storms)")
      raster::plot(boundaries, add=TRUE)
      if (tracks) {
        for (i in 1:nrow(ii)) {
          hur_id <- ii[i, "hur_id"]
          fuj_min <- 1
          xx <- get_track_lat_lon(hur_id, fuj_min, tt, kk)
          if (!is.null(xx)) {
            lines(xx$longitude, xx$latitude, col="grey")
          }         
        }
      }
    } else cat("\nNo F1 values\n")

  } else if (var == "ef2") {
    if (raster::maxValue(ef2_layer) > 0) {
      raster::plot(ef2_layer, xlab=xlab, ylab=ylab, main="Fujita Scale 2 (storms)")
      raster::plot(boundaries, add=TRUE)
      if (tracks) {
        for (i in 1:nrow(ii)) {
          hur_id <- ii[i, "hur_id"]
          fuj_min <- 2
          xx <- get_track_lat_lon(hur_id, fuj_min, tt, kk)
          if (!is.null(xx)) {
            lines(xx$longitude, xx$latitude, col="grey")
          }         
        }
      }
    } else cat("\nNo F2 values\n")
     
  } else if (var == "ef3") {
    if (raster::maxValue(ef3_layer) > 0) {
      raster::plot(ef3_layer, xlab=xlab, ylab=ylab, main="Fujita Scale 3 (storms)")
      raster::plot(boundaries, add=TRUE)
      if (tracks) {
        for (i in 1:nrow(ii)) {
          hur_id <- ii[i, "hur_id"]
          fuj_min <- 3
          xx <- get_track_lat_lon(hur_id, fuj_min, tt, kk)
          if (!is.null(xx)) {
            lines(xx$longitude, xx$latitude, col="grey")
          }         
        }
      }
    } else cat("\nNo F3 values\n")

  } else if (var == "ef4") {
    if (raster::maxValue(ef4_layer) > 0) {
      raster::plot(ef4_layer, xlab=xlab, ylab=ylab, main="Fujita Scale 4 (storms)")
      raster::plot(boundaries, add=TRUE)
      if (tracks) {
        for (i in 1:nrow(ii)) {
          hur_id <- ii[i, "hur_id"]
          fuj_min <- 4
          xx <- get_track_lat_lon(hur_id, fuj_min, tt, kk)
          if (!is.null(xx)) {
            lines(xx$longitude, xx$latitude, col="grey")
          }         
        }
      }
    } else cat("\nNo F4 values\n")

  } else if (var == "ef5") {
    if (raster::maxValue(ef5_layer) > 0) {
      raster::plot(ef5_layer, xlab=xlab, ylab=ylab, main="Fujita Scale 5 (storms)")
      raster::plot(boundaries, add=TRUE)
      if (tracks) {
        for (i in 1:nrow(ii)) {
          hur_id <- ii[i, "hur_id"]
          fuj_min <- 5
          xx <- get_track_lat_lon(hur_id, fuj_min, tt, kk)
          if (!is.null(xx)) {
            lines(xx$longitude, xx$latitude, col="grey")
          }         
        }
      }
    } else cat("\nNo F5 values\n")

  } else {
    cat("\nvar must be efmax, ef0, ef1, ef2, ef3, ef4, or ef5\n")
    stop()
  }
}

