# validation sommaire du relevé pétoncle 
# évaluation des distances de draguage
# évaluation des profondeurs

# libraries 
library(sf)

# wrapper to get indices of missing values 
# where x is just a vector of values 
which_missing <- function(x){
    which(!complete.cases(x))
}


which_missingxy <- function(set_data){
  
  # find indice where values are missing, from any coord field
  all_coords <- list(set_data$start_latitude_dd,
                     set_data$start_longitude_dd,
                     set_data$end_latitude_dd,
                     set_data$end_longitude_dd)
  
  missingxy <- lapply(all_coords, which_missing)
  # at least one missing coord (from the 4 inputs) at those indices
  missingxy <- sort(unique(unlist(missingxy)))
  
}

which_missingdepth <- function(set_data){
  # find indice where values are missing, from any coord field
  all_depths <- list(set_data$start_depth_m,
                     set_data$end_depth_m)
  
  missingdepth <- lapply(all_depths, which_missing)
  # at least one missing coord (from the 4 inputs) at those indices
  missingdepth <- sort(unique(unlist(missingdepth)))
}


# build a small summary table to display missing coords
missing_coord_recap_table <- function(set_data, missing_indices){
  #define cols to display
  cols2display <- c("set_number",
                    "station_number",
                    "stratum",
                    "start_date",
                    "end_date",
                    "start_latitude_dd",
                    "start_longitude_dd",
                    "end_latitude_dd",
                    "end_longitude_dd",
                    "start_depth_m",
                    "end_depth_m"
                    )
  set_data[missing_indices, cols2display]
}



# create a table of suggested coords from the outlier_report 
suggestion_table <- function(outlier_report){
  
  datetime_start_ship_utc <- as.POSIXct(outlier_report$datetime$datetime_start_ship, tz="UTC")
  datetime_start_ship_et <-format(datetime_start_ship_utc,
                                  "%Y-%m-%d %H:%M:%S",
                                  tz="America/Toronto")
  datetime_end_ship_utc <- as.POSIXct(outlier_report$datetime$datetime_end_ship, tz="UTC")
  datetime_end_ship_et <-format(datetime_end_ship_utc, 
                                "%Y-%m-%d %H:%M:%S",
                                tz="America/Toronto")
  
  #suggestion table
  sug_table <- data.frame(outlier_report$datetime[,1:2],
                          outlier_report$coord_start_ship$geometry,
                          outlier_report$coord_end_ship$geometry,
                          datetime_ET = datetime_start_ship_et,
                          datetime_ET = datetime_end_ship_et
  )
}

# x is a vector of distance values
# returns a boolean
find_simple_outlier <- function(x){
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQRv <- IQR(x, na.rm = TRUE)
  multi <- 1.5
  lower <- Q1 - multi * IQRv
  upper <- Q3 + multi * IQRv
  outlier_mask <- x < lower | x > upper
}


# find the timelag between andes clock and the ship's clock
# will look for coords of both sources  that are close enough in space
# will compare the clock difference for all of those match
# returns the 
# if result is positive andes clock is set higher (in advance)
# if result is negative ship clock is set higher (in advance)
find_timelag <- function(andes_track, ship_track){
  
  # fx params
  th_time <- 4*60 #time threshold in seconds (prefilter to avoid useless computation)
  th_dist <- 5 # distance threshold in meters (real filter parameter)
  
  # ship time
  shipTime <- ship_track$time
  
  # number of trackpoints
  n_andes_track <- dim(andes_track)[1]
  
  # preallocate object to store result
  dummyCol <- rep(NA, n_andes_track)
  results <- data.frame(time=dummyCol, dist=dummyCol)
  
  for(i in 1:n_andes_track){
    
    andesGeom_i <- andes_track$geometry[i]
    andesTime_i <- andes_track$time[i]
    
    # filter using time threshold
    # to avoid calculating distance over ALL points
    # we dont want a good matching position that is too much far in time 
    
    # absolute time diff, aiming at a 0 time diff
    timeDifference <- abs(difftime(andesTime_i, shipTime, units = "secs"))
    minTime <- min(timeDifference)
    
    # create a subset to lighten up dist calculations
    maskTime <- timeDifference <= th_time
    ship_track_subset <- ship_track[maskTime,]
    
    if(sum(maskTime)>0){
      # then filter this time-window subset using a dist threshold
      coordDistance <- st_distance(andesGeom_i, ship_track_subset$geometry)
      minDist <- min(coordDistance)
      
      # if we get a close enough coord match let's go forward
      if(as.numeric(minDist) <= th_dist){
        # find the closest point in distance
        minIndex <- which.min(coordDistance)
        
        shipGeom_i <- ship_track_subset[minIndex,]$geometry
        shipTime_i <- ship_track_subset[minIndex,]$time
        
        timeDiff_i <- difftime(andesTime_i, shipTime_i, units = "secs") # secs
        distDiff_i <- st_distance(andesGeom_i, shipGeom_i) # meters
        
        results$time[i] <- timeDiff_i
        results$dist[i] <- distDiff_i
      }
    }
  }
  mask <- is.na(results$time)
  results_light <-results[!mask,]
  as.difftime(mean(results_light$time), units="secs")
  
}

# find the corresponding timestamp for a given set number
#set is the set number
#fishingevent is the dataframe of the fishingevent csv file
find_timestamp_from_fishingevent <- function(set_num, fishingevent_data){
  
  # find the two matching sets entries
  set_mask <- fishingevent_data$set_number == set_num

  # error flag
  if(sum(set_mask) != 2){
    stop("expecting exactly 2 match : start and end")
  }
  
  # subset the dataset  
  fishingevent_subset <- fishingevent_data[set_mask,]
  
  # find which one is start event and end event
  start_indice <- grep(pattern = "^Filet.*fond$", x = fishingevent_subset$event_type)
  end_indice <- grep(pattern = "^Remont.*filet$", x = fishingevent_subset$event_type)
  
  # dates as posix object 
  datetime_fishingevent <- c(start=as.POSIXct(fishingevent_subset$datetime[start_indice], tz="UTC"),
                             end=as.POSIXct(fishingevent_subset$datetime[end_indice], tz="UTC"))
}

# grep wrapper to find tows from the full waypoint layer
# ship wp layer include fishing station, ctd station, repeat station angle (start/end) and fishing+ctd respective start/end
# get only fishing entries from that full layer
ship_wp_find_tow <- function(ship_wp, mission_id){
  
  mission_id_compact <- gsub(pattern = "-", replacement = "", mission_id)
  year_pattern <- substr(mission_id_compact, 4, 7)
  
  search_pattern <- paste0("^", year_pattern, "-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}$")
  matching_indices <- grep(search_pattern, ship_wp$name)
  
  ship_wp_tows <- ship_wp[matching_indices,]

}

# add desc (!)
tow_distance <- function(set_data_filtered){

  # as sf object
  coord_start <- st_as_sf(
    data.frame(set_num= set_data_filtered$set_number,
               sta_num= set_data_filtered$station_number,
               sta_stratum= set_data_filtered$stratum,
               lon= set_data_filtered$start_longitude_dd,
               lat= set_data_filtered$start_latitude_dd),
    coords = c("lon", "lat"),
    crs = 4326
  )
  # as sf object
  coord_end <- st_as_sf(
    data.frame(set_num=set_data_filtered$set_number,
               sta_num=set_data_filtered$station_number,
               sta_stratum=set_data_filtered$stratum,
               lon=set_data_filtered$end_longitude_dd,
               lat=set_data_filtered$end_latitude_dd),
    coords = c("lon", "lat"),
    crs = 4326
  )
  
  towDistances_geom <- st_distance(coord_start, coord_end, by_element = T)
  towDistances <- as.numeric(towDistances_geom)
}


check_outliers <- function(set_data_outlier, ship_wp_tow, ship_wp_timecorrected){
  
  n_outliers <- dim(set_data_outlier)[1]
  
  # dates as posix object 
  datetime_andes_start <- as.POSIXct(set_data_outlier$start_date, tz="UTC")
  datetime_andes_end <- as.POSIXct(set_data_outlier$end_date, tz="UTC")
  
  # create sf objets from outliers
  coord_start <- st_as_sf(
    data.frame(set_num = set_data_outlier$set_number,
               sta_num = set_data_outlier$station_number,
               sta_stratum = set_data_outlier$stratum,
               date_utc = datetime_andes_start,
               lon = set_data_outlier$start_longitude_dd,
               lat = set_data_outlier$start_latitude_dd),
    coords = c("lon", "lat"),
    crs = 4326
  )
  
  coord_end <- st_as_sf(
    data.frame(set_num=set_data_outlier$set_number,
               sta_num=set_data_outlier$station_number,
               sta_stratum=set_data_outlier$stratum,
               date_utc=datetime_andes_end,
               lon=set_data_outlier$end_longitude_dd,
               lat=set_data_outlier$end_latitude_dd),
    coords = c("lon", "lat"),
    crs = 4326
  )
  
  # compute the dist (meters)
  tow_dist_outlier <- as.numeric(st_distance(coord_start, coord_end, by_element = T))
  
  # init ship based variables
  tow_dist_ship <- rep(NA, n_outliers) # distance values 
  start_coord_ship_indice <- rep(NA, n_outliers) #to store indices
  end_coord_ship_indice <- rep(NA, n_outliers) #to store indices
  datetime_ship_start <- as.POSIXct(rep(NA, n_outliers), origin = "1970-01-01")
  datetime_ship_end <- as.POSIXct(rep(NA, n_outliers), origin = "1970-01-01")
  
  # go through outliers and find corresponding ship waypoint
  for(i in 1:n_outliers){
    
    set_i <- set_data_outlier$set_number[i]
    sta_i <- set_data_outlier$station_number[i]
    
    # match start
    timediff <- datetime_andes_start[i] - ship_wp_timecorrected
    i_start <- which.min(abs(timediff))
    timediff_start <- min(abs(timediff))
    start_coord_ship_indice[i] <- i_start
      
    # match end
    timediff <- datetime_andes_end[i] - ship_wp_timecorrected
    i_end <- which.min(abs(timediff))
    timediff_end <- min(abs(timediff))
    end_coord_ship_indice[i] <- i_end
    
    # matching conflict : find the second time-based waypoint match
    if(i_start == i_end){
      mymsg <- sprintf("falling back on second best match at set=%s sta=%s", set_i, sta_i)
      warning(mymsg)
      
      #case 1
      if(timediff_start < timediff_end){
        
        #recompute diff
        timediff <- datetime_andes_end[i] - ship_wp_timecorrected
        i_end <- which.min(abs(timediff))
        #remove best match to fallback on number 2
        timediff <- timediff[-i_end]
        i_end <- which.min(abs(timediff))
        timediff_end <- min(abs(timediff))
        end_coord_ship_indice[i] <- i_end
        
      #case 2
      }else if(timediff_end < timediff_start){
        
        #recompute diff
        timediff <- datetime_andes_start[i] - ship_wp_timecorrected
        i_start <- which.min(abs(timediff))
        #remove best match to fallback on number 2
        timediff <- timediff[-i_start]
        i_start <- which.min(abs(timediff))
        timediff_start <- min(abs(timediff))
        start_coord_ship_indice[i] <- i_start
      }
    }
    
    # compute and write distance for outlier i
    tow_dist_ship[i] <- st_distance(ship_wp_tow$geometry[i_start], ship_wp_tow$geometry[i_end])
    
    # write ship datetime 
    datetime_ship_start[i] <- ship_wp_timecorrected[i_start]
    datetime_ship_end[i] <- ship_wp_timecorrected[i_end]
  }
  
  # table des distances
  distance_df <- data.frame(set=set_data_outlier$set_number,
                            station=set_data_outlier$station_number,
                            dist_andes= tow_dist_outlier,
                            dist_ship = tow_dist_ship)
  
  # table des datetime
  datetime_df <- data.frame(set=set_data_outlier$set_number,
                            station=set_data_outlier$station_number,
                            datetime_start_andes= format(datetime_andes_start, "%Y-%m-%d %H:%M:%S %Z", tz="UTC"),
                            datetime_end_andes = format(datetime_andes_end, "%Y-%m-%d %H:%M:%S %Z", tz="UTC"),
                            datetime_start_ship = format(datetime_ship_start, "%Y-%m-%d %H:%M:%S %Z", tz = "UTC"),
                            datetime_end_ship = format(datetime_ship_end, "%Y-%m-%d %H:%M:%S %Z", tz = "UTC"),
                            timediff_start = datetime_andes_start - datetime_ship_start,
                            timediff_end = datetime_andes_end - datetime_ship_end
  )
  
  # copy objects structure but replace values inside
  coord_start_ship <- coord_start
  coord_start_ship$geometry <- ship_wp_tow[start_coord_ship_indice,]$geometry #replae the geometry value
  coord_start_ship$date_utc <- format(datetime_ship_start, tz="UTC")
  
  coord_end_ship <- coord_end
  coord_end_ship$geometry <- ship_wp_tow[end_coord_ship_indice,]$geometry # replace the geometry value
  coord_end_ship$date_utc <- format(datetime_ship_end, tz="UTC")
  
  outlier_report <- list(dist=distance_df,
                         datetime=datetime_df,
                         coord_start_andes=coord_start,
                         coord_end_andes=coord_end,
                         coord_start_ship=coord_start_ship,
                         coord_end_ship=coord_end_ship
  )
  
}

# deal with missing coords
# suggest replacement values 
check_missingxy <- function(set_data_missingxy, ship_wp_tow, ship_wp_timecorrected, fishingevent_data){
  
  n <- dim(set_data_missingxy)[1]
  
  # --- ---
  # load "fishing events" csv to get the tow start/end timestamps
  # fishing events never miss a timestamp... (i don't know why)
  # even when their xy coordinates are wrong, the timestamp is actually good
  # so we can rely on them to get the correct andes timestamp 
  # --- --- 
  
  # first preallocate posix 
  datetime_andes_start <- as.POSIXct(rep(NA_real_, n),
                                         origin = "1970-01-01",
                                         tz = "UTC")
  datetime_andes_end <- as.POSIXct(rep(NA_real_, n),
                                       origin = "1970-01-01",
                                       tz = "UTC")
  
  # get timestamp from the fishing even dataset
  for( i in 1:n){
    set_i <- set_data_missingxy$set_number[i]
    dates_i <-find_timestamp_from_fishingevent(set_i, fishingevent_data)  
    datetime_andes_start[i] <- dates_i[1]
    datetime_andes_end[i] <- dates_i[2]
  }
  
  # init ship based variables
  tow_dist_ship <- rep(NA, n) # distance values 
  start_coord_ship_indice <- rep(NA, n) #to store indices
  end_coord_ship_indice <- rep(NA, n) #to store indices
  datetime_ship_start <- as.POSIXct(rep(NA, n), origin = "1970-01-01")
  datetime_ship_end <- as.POSIXct(rep(NA, n), origin = "1970-01-01")

  # go through missingxy and find corresponding ship waypoints
  for(i in 1:n){
    
    set_i <- set_data_missingxy$set_number[i]
    sta_i <- set_data_missingxy$station_number[i]
    
    # match start
    timediff <- datetime_andes_start[i] - ship_wp_timecorrected
    i_start <- which.min(abs(timediff))
    timediff_start <- min(abs(timediff))
    start_coord_ship_indice[i] <- i_start
    
    # match end
    timediff <- datetime_andes_end[i] - ship_wp_timecorrected
    i_end <- which.min(abs(timediff))
    timediff_end <- min(abs(timediff))
    end_coord_ship_indice[i] <- i_end
    
    # matching conflict : find the second time-based waypoint match
    if(i_start == i_end){
      mymsg <- sprintf("falling back on second best match at set=%s sta=%s", set_i, sta_i)
      warning(mymsg)
      
      # case 1
      if(timediff_start < timediff_end){
        
        # recompute diff
        timediff <- datetime_andes_end[i] - ship_wp_timecorrected
        i_end <- which.min(abs(timediff))
        # remove best match to fallback on number 2
        timediff <- timediff[-i_end]
        i_end <- which.min(abs(timediff))
        timediff_end <- min(abs(timediff))
        end_coord_ship_indice[i] <- i_end
        
      # case 2
      }else if(timediff_end < timediff_start){
        
        # recompute diff
        timediff <- datetime_andes_start[i] - ship_wp_timecorrected
        i_start <- which.min(abs(timediff))
        # remove best match to fallback on number 2
        timediff <- timediff[-i_start]
        i_start <- which.min(abs(timediff))
        timediff_start <- min(abs(timediff))
        start_coord_ship_indice[i] <- i_start
      }
    }
    
    # compute and write distance for missingxy at i
    tow_dist_ship[i] <- st_distance(ship_wp_tow$geometry[i_start], ship_wp_tow$geometry[i_end])
  
    # write  ship datetime 
    datetime_ship_start[i] <- ship_wp_timecorrected[i_start]
    datetime_ship_end[i] <- ship_wp_timecorrected[i_end]
  }
  
  # table des distances
  distance_df <- data.frame(set=set_data_missingxy$set_number,
                            station=set_data_missingxy$station_number,
                            dist_ship = tow_dist_ship)
  
  datetime_df <- data.frame(set=set_data_missingxy$set_number,
                            station=set_data_missingxy$station_number,
                            datetime_start_ship = format(datetime_ship_start, "%Y-%m-%d %H:%M:%S %Z", tz = "UTC"),
                            datetime_end_ship = format(datetime_ship_end, "%Y-%m-%d %H:%M:%S %Z", tz = "UTC")
  )
  
  # format sf object for start pos
  coord_start_ship <- st_as_sf(
    data.frame(set_num = set_data_missingxy$set_number,
               sta_num = set_data_missingxy$station_number,
               sta_stratum = set_data_missingxy$stratum,
               date_utc = format(datetime_ship_start, tz="UTC")
               ),
    geometry = st_geometry(ship_wp_tow[start_coord_ship_indice,]$geometry),
    crs = 4326
    )
  
  # format sf object for end pos
  coord_end_ship <- st_as_sf(
    data.frame(set_num = set_data_missingxy$set_number,
               sta_num = set_data_missingxy$station_number,
               sta_stratum = set_data_missingxy$stratum,
               date_utc = format(datetime_ship_end, tz="UTC")
               ),
    geometry = st_geometry(ship_wp_tow[end_coord_ship_indice,]$geometry),
    crs = 4326
  )
  
  # assemble the report in a list
  missingxy_report <- list(dist=distance_df,
                         datetime=datetime_df,
                         coord_start_andes=NULL,
                         coord_end_andes=NULL,
                         coord_start_ship=coord_start_ship,
                         coord_end_ship=coord_end_ship
  )
}

check_missingdepth <- function(set_data_missingdepth, depth_data, fishingevent_data){
  
  n <- dim(set_data_missingdepth)[1]
  
  # --- ---
  # load "fishing events" csv to get the tow start/end timestamps
  # fishing events never miss a timestamp... (i don't know why)
  # even when their xy coordinates (or depth) are wrong, the timestamp is actually good
  # so we can rely on them to get the correct andes timestamp 
  # --- --- 
  
  # first preallocate posix 
  datetime_andes_start <- as.POSIXct(rep(NA_real_, n),
                                     origin = "1970-01-01",
                                     tz = "UTC")
  datetime_andes_end <- as.POSIXct(rep(NA_real_, n),
                                   origin = "1970-01-01",
                                   tz = "UTC")
  
  
  # to posix
  datetime_sens <- as.POSIXct(depth_data$datetime, tz="UTC")
  
  # init sensor values
  start_sens_indice <- rep(NA, n) #to store indices
  end_sens_indice <- rep(NA, n) #to store indices
  datetime_sens_start <- as.POSIXct(rep(NA, n), origin = "1970-01-01")
  datetime_sens_end <- as.POSIXct(rep(NA, n), origin = "1970-01-01")
  depth_sens_start <- rep(NA,n) 
  depth_sens_end <- rep(NA,n)
  timediff_sens_start <- dt <- structure(rep(NA_real_, n),class = "difftime",units = "secs")
  timediff_sens_end <- dt <- structure(rep(NA_real_, n),class = "difftime",units = "secs")
  
  if(n>0){
    # get timestamp from the fishing even dataset
    for(i in 1:n){
      set_i <- set_data_missingdepth$set_number[i]
      dates_i <-find_timestamp_from_fishingevent(set_i, fishingevent_data)  
      datetime_andes_start[i] <- dates_i[1]
      datetime_andes_end[i] <- dates_i[2]
    }
    
    for(i in 1:n){
      
      set_i <- set_data_missingdepth$set_number[i]
      sta_i <- set_data_missingdepth$station_number[i]
      
      # match start
      timediff <- datetime_andes_start[i] - datetime_sens
      i_start <- which.min(abs(timediff))
      timediff_start <- min(abs(timediff))
      start_sens_indice[i] <- i_start
      timediff_sens_start[i] <- timediff_start 
      
      # match end
      timediff <- datetime_andes_end[i] - datetime_sens
      i_end <- which.min(abs(timediff))
      timediff_end <- min(abs(timediff))
      end_sens_indice[i] <- i_end
      timediff_sens_end[i] <- timediff_end
      
      # matching conflict : find the second time-based waypoint match
      if(i_start == i_end){
        mymsg <- sprintf("falling back on second best match (var=depth) at set=%s sta=%s", set_i, sta_i)
        warning(mymsg)
        
        # case 1
        if(timediff_start < timediff_end){
          
          # recompute diff
          timediff <- datetime_andes_end[i] - dateime_sens
          i_end <- which.min(abs(timediff))
          # remove best match to fallback on number 2
          timediff <- timediff[-i_end]
          i_end <- which.min(abs(timediff))
          timediff_end <- min(abs(timediff))
          end_sens_indice[i] <- i_end
          timediff_sens_end[i] <- timediff_end 
          
          # case 2
        }else if(timediff_end < timediff_start){
          
          # recompute diff
          timediff <- datetime_andes_start[i] - datetime_sens
          i_start <- which.min(abs(timediff))
          # remove best match to fallback on number 2
          timediff <- timediff[-i_start]
          i_start <- which.min(abs(timediff))
          timediff_start <- min(abs(timediff))
          start_sens_indice[i] <- i_start
          timediff_sens_start[i] <- timediff_start 
        }
      }
        
        # write  ship datetime 
        datetime_sens_start[i] <- datetime_sens[i_start]
        datetime_sens_end[i] <- datetime_sens[i_end]  
      
        depth_sens_start[i] <- depth_data$value[i_start]
        depth_sens_end[i] <- depth_data$value[i_end]
      
    }
  }
  
  # make the recap table report
  depth_df <- data.frame(
    set=set_data_missingdepth$set_number,
    station=set_data_missingdepth$station_number,
    depth_start_before=set_data_missingdepth$start_depth_m,
    depth_end_before=set_data_missingdepth$end_depth_m,
    depth_start_after=depth_sens_start,
    depth_end_after=depth_sens_end
  )
  
  date_df <- data.frame(
    set=set_data_missingdepth$set_number,
    station=set_data_missingdepth$station_number,
    date_start_before=format(datetime_andes_start, "%Y-%m-%d %H:%M:%S %Z", tz = "America/Toronto"),
    date_end_before=format(datetime_andes_end, "%Y-%m-%d %H:%M:%S %Z", tz = "America/Toronto"),
    date_start_after=format(datetime_sens_start, "%Y-%m-%d %H:%M:%S %Z", tz = "America/Toronto"),
    date_end_after=format(datetime_sens_end, "%Y-%m-%d %H:%M:%S %Z", tz = "America/Toronto"),
    timediff_start=timediff_sens_start,
    timediff_end=timediff_sens_end
  )
  
  new_values <- data.frame(
    set=set_data_missingdepth$set_number,
    station=set_data_missingdepth$station_number,
    date_start_after=format(datetime_sens_start, "%Y-%m-%d %H:%M:%S %Z", tz = "America/Toronto"),
    date_end_after=format(datetime_sens_end, "%Y-%m-%d %H:%M:%S %Z", tz = "America/Toronto"),
    depth_start_after=depth_sens_start,
    depth_end_after=depth_sens_end
  )
  
  # assemble the report in a list
  missingdepth_report <- list(depth=depth_df,
                              date=date_df,
                              new=new_values
  )
} 


# visual representation of the new suggested coords
plot_outlier_replacement <- function(report, sta_data){
  
  n_station <- dim(report$coord_start_ship)[1]
  
  # pop the navigational entries out of the data frame
  sta_mask <- sta_data$desc == "Navigational"
  sta_data <- sta_data[!sta_mask,]
  sta_name_numeric <- as.numeric(sta_data$name)
  
  #plot them all one by one to really see the 
  for(i in 1:n_station){
    
    sta_i <- report$coord_start_ship$sta_num[i]
    set_i <- report$coord_start_ship$set_num[i]
    
    # find the theorical position of this station
    sta_mask <- sta_name_numeric == sta_i
    sta_coord <- sta_data[sta_mask,]
      
    # error flag
    if(sum(sta_mask) != 1){
      mymsg <- sprintf("expecting only one match for station %s", sta_i)
      stop(mymsg)
    }
    
    #create the bounding box for plot extent
    bb <- st_bbox(rbind(
      st_as_sf(report$coord_start_andes[i,]),
      st_as_sf(report$coord_end_andes[i,]),
      st_as_sf(report$coord_start_ship[i,]),
      st_as_sf(report$coord_end_ship[i,])
    ))
    
    # Andes line
    andes_line <- st_sfc(
      st_linestring(rbind(
        st_coordinates(report$coord_start_andes[i,]),
        st_coordinates(report$coord_end_andes[i,])
      )),
      crs = st_crs(report$coord_start_andes)
    )
    
    # Ship line
    ship_line <- st_sfc(
      st_linestring(rbind(
        st_coordinates(report$coord_start_ship[i,]),
        st_coordinates(report$coord_end_ship[i,])
      )),
      crs = st_crs(report$coord_start_ship)
    )
    
    descbox <- sprintf("dist andes = %.2fm\ndist navire=%.2fm", 
                       report$dist$dist_andes[i],
                       report$dist$dist_ship[i])
      
  ptitle <- sprintf("station=%s trait=%s", sta_i, set_i)
    
  # plot the suff
  plot(st_geometry(report$coord_start_andes)[i],
       xlim=c(bb["xmin"], bb["xmax"]),
       ylim=c(bb["ymin"], bb["ymax"]),
       pch=16,
       col="orange",
       main=ptitle)
  plot(st_geometry(report$coord_end_andes)[i], 
       add=T,
       pch=16,
       col="orange")
  plot(st_geometry(report$coord_start_ship)[i], 
       add=T,
       pch=16,
       col="green")
  plot(st_geometry(report$coord_end_ship)[i], 
       add=T,
       pch=16,
       col="green")
  #plot the theorical position
  plot(st_geometry(sta_coord), 
       add=T,
       pch=16,
       col="black")
  # Plot lines
  plot(andes_line, 
       add = TRUE,
       col = "orange",
       lwd = 2)
  plot(ship_line, 
       add = TRUE,
       col = "green",
       lwd = 2)
  
  label_position <- st_coordinates(sta_coord)
  text(label_position[,1], label_position[,2],
       labels = sta_coord$name,
       pos = 4)
  
  legend("topright",
         legend = c("Andes GPS", "Ship GPS", "Station"),
         col = c("orange", "green", "black"),
         pch = c(16, 16, 16),
         lty = c(1, 1, NA),
         lwd = c(2, 2, NA),
         bty = "n")
  
  # add text in the plot, based on the station position
  text(x = label_position[,1],
       y = label_position[,2],
       labels = descbox,
       pos=1,
       offset=3
       )
  }
}

# visual representation of the new suggested coords
plot_missingxy_replacement <- function(report, sta_data){
  
  n_station <- dim(report$coord_start_ship)[1]
  
  # pop the navigational entries out of the data frame
  sta_mask <- sta_data$desc == "Navigational"
  sta_data <- sta_data[!sta_mask,]
  sta_name_numeric <- as.numeric(sta_data$name)
  
  #plot them all one by one to really see the 
  for(i in 1:n_station){
    
    sta_i <- report$coord_start_ship$sta_num[i]
    set_i <- report$coord_start_ship$set_num[i]
    
    # find the theorical position of this station
    sta_mask <- sta_name_numeric == sta_i
    sta_coord <- sta_data[sta_mask,]
  
    # error flag
    if(sum(sta_mask) != 1){
      mymsg <- sprintf("expecting only one match for station %s", sta_i)
      stop(mymsg)
    }
  
    #create the bounding box for plot extent
    bb <- st_bbox(rbind(
      st_as_sf(report$coord_start_ship[i,]),
      st_as_sf(report$coord_end_ship[i,])
    ))
  
    # Ship line
    ship_line <- st_sfc(
      st_linestring(rbind(
        st_coordinates(report$coord_start_ship[i,]),
        st_coordinates(report$coord_end_ship[i,])
      )),
      crs = st_crs(report$coord_start_ship)
    )
  
    descbox <- sprintf("dist navire=%.2fm", 
                       report$dist$dist_ship[i])
    
    # figure title
    ptitle <- sprintf("station=%s trait=%s", sta_i, set_i)
    
    plot(st_geometry(report$coord_start_ship)[i],
         xlim=c(bb["xmin"], bb["xmax"]),
         ylim=c(bb["ymin"], bb["ymax"]),
         pch=16,
         col="green",
         main=ptitle)
    plot(st_geometry(report$coord_end_ship)[i], 
         add=T,
         pch=16,
         col="green")
    #plot the theorical position
    plot(st_geometry(sta_coord), 
         add=T,
         pch=16,
         col="black")
    # plot line
    plot(ship_line, 
         add = TRUE,
         col = "green",
         lwd = 2)
    
    label_position <- st_coordinates(sta_coord)
    text(label_position[,1], label_position[,2],
         labels = sta_coord$name,
         pos = 4)
    
    legend("topright",
           legend = c("Ship GPS", "Station"),
           col = c("green", "black"),
           pch = c(16, 16),
           lty = c(1, NA),
           lwd = c(2, NA),
           bty = "n")
    
    text(x = label_position[,1],
         y = label_position[,2],
         labels = descbox,
         pos=1,
         offset=3
    )
  }
}

# subset wrapper to extract true depth from sensor dataset
get_true_depth <- function(sens_data){
  mask <- sens_data$type == "true_depth"
  depth_data <- sens_data[mask,]
}
