# Libraries & datasets ####
# libraries
library(tidyverse)
library(geosphere)
# datasets - GTFS
mcdot_routes <- read_delim("mcdot_routes_429.txt")
mcdot_trips <- read_delim("mcdot_trips_429.txt")
mcdot_stops <- read_delim("mcdot_stops_429.txt")
mcdot_stop_times <- read_delim("mcdot_stop_times_429.txt")
wmata_routes <- read_delim("jun_wmata_routes.txt")
wmata_trips <- read_delim("jun_wmata_trips.txt")
wmata_stops <- read_delim("jun_wmata_stops.txt")
wmata_stop_times <- read_delim("jun_wmata_stop_times.txt")
# to filter to stations
wmata_weekday_stations_arrivals <- read_csv("wmata_weekday_stations_arrivals.csv")
mcdot_weekday_stations_arrivals <- read_csv("mcdot_weekday_stations_arrivals.csv")

# prepare the station names for filtering 
# make a dataset that contains all of the distinct stop_ids (and stop_names) that correspond to the selected stations
wmata_stations <- wmata_weekday_stations_arrivals %>% 
  select(stop_id, stop_name, station_name, agency)
wmata_stations_distinct <- wmata_stations %>% 
  distinct(.keep_all = TRUE) %>% 
  filter(!is.na(station_name))
# mcdot
mcdot_stations <- mcdot_weekday_stations_arrivals %>% 
  select(stop_id, stop_name, station_name, agency)
mcdot_stations_distinct <- mcdot_stations %>% 
  distinct(.keep_all = TRUE) %>% 
  filter(!is.na(station_name))


# WMATA Metrobus ####

## Join datasets for coordinates ####
wmata_stop_time_coords <- left_join(wmata_stop_times %>% 
                                      select(trip_id, stop_id, stop_sequence, arrival_time),
                                    wmata_stops %>% 
                                      select(-stop_desc, -zone_id, -stop_url))
wmata_route_trips <- left_join(wmata_routes %>% 
                                 select(route_id, route_short_name),
                               wmata_trips %>% 
                                 select(route_id, service_id, trip_id, direction_id))
# create dataset with the coordinates of every single bus arrival at every stop (including the name of the route)
wmata_stop_and_route_details <- left_join(wmata_stop_time_coords, wmata_route_trips)
# filter to only station stops
wmata_station_info <- wmata_stop_and_route_details %>% 
  filter(stop_id %in% wmata_stations_distinct$stop_id) 
# attach to this dataset the station names in the format I created 
wmata_station_info <- left_join(wmata_station_info, wmata_stations_distinct)


# MCDOT Ride On ####

## Join datasets for coordinates ####
mcdot_stop_time_coords <- left_join(mcdot_stop_times %>% 
                                      select(trip_id, stop_id, stop_sequence, arrival_time),
                                    mcdot_stops %>% 
                                      select(-stop_desc, -zone_id, -stop_url, -location_type, -parent_station, -stop_timezone, -wheelchair_boarding))
mcdot_route_trips <- left_join(mcdot_routes %>% 
                                 select(route_id, route_short_name),
                               mcdot_trips %>% 
                                 select(route_id, service_id, trip_id, direction_id))
mcdot_stop_and_route_details <- left_join(mcdot_stop_time_coords, mcdot_route_trips)

# filter to only station stops 
mcdot_station_info <- mcdot_stop_and_route_details %>% 
  filter(stop_id %in% mcdot_stations_distinct$stop_id) 
# append to this dataset the station names in the format I created 
mcdot_station_info <- left_join(mcdot_station_info, mcdot_stations_distinct)


# Join MCDOT and WMATA ####

# make datasets joinable
mcdot_station_info$route_id <- as.character(mcdot_station_info$route_id)
# join datasets (bind rows)
station_info <- bind_rows(mcdot_station_info, wmata_station_info)
# filter to stations that mcdot serves
station_info <- station_info %>% 
  filter(station_name %in% mcdot_stations_distinct$station_name)

#### Station summary table ####
# of coordinates, unique routes, name 

# summarise data to see total unique routes
station_route_summary <- station_info %>% 
  group_by(station_name) %>% 
  summarise(
    total_routes = n_distinct(route_short_name),
    lat = max(stop_lat), # there's multiple different latitudes and longitudes listed for the specific bus bays. this is just a way to select only one latitude.
    lon = max(stop_lon)
  )


# Geographical distances ####

# create a table of the distances between each station to each other

# make an empty table (not the cleanest way to do it, but it works)
cat(paste0(as.vector(station_route_summary$station_name), collapse = "' = rep(NA, endlength), '"), "= rep(NA, endlength)")
endlength <- length(station_route_summary$station_name)
empty_station <- tibble('BETHESDA STATION' = rep(NA, endlength), 'FOREST GLEN STATION' = rep(NA, endlength), 'GERMANTOWN TRANSIT CENTER' = rep(NA, endlength), 'GLENMONT STATION' = rep(NA, endlength), 'GROSVENOR STATION' = rep(NA, endlength), 'LAKEFOREST TRANSIT CENTER' = rep(NA, endlength), 'MEDICAL CENTER STATION' = rep(NA, endlength), 'MONTGOMERY MALL TRANSIT CENTER' = rep(NA, endlength), 'NORTH BETHESDA STATION' = rep(NA, endlength), 'ROCKVILLE STATION' = rep(NA, endlength), 'SHADY GROVE STATION' = rep(NA, endlength), 'SILVER SPRING STATION' = rep(NA, endlength), 'TRAVILLE TRANSIT CENTER' = rep(NA, endlength), 'TWINBROOK STATION' = rep(NA, endlength), 'WHEATON STATION' = rep(NA, endlength)) 
empty_station <- empty_station %>% 
  mutate(
    "station" = names(empty_station)
  ) %>% 
  relocate(
    "station", .before = "BETHESDA STATION"
  )

distances <- empty_station

for (column_index in 1:nrow(empty_station)) {
  # first take the column (one by one)
  non_station_column_index <- column_index+1 # to avoid referring to the "station" column
  column_name <- distances$station[column_index]
  
  # next take every row within that column
  for (row_index in 1:nrow(empty_station)) {
    row_name <- distances$station[row_index]
    
    lon_1 <- as.numeric(station_route_summary$lon[which(station_route_summary$station_name == column_name)]) # get latitude of the "column" in coordinates table
    lat_1 <- as.numeric(station_route_summary$lat[which(station_route_summary$station_name == column_name)]) # get longitude of the "column" in coordinates table
    lon_2 <- as.numeric(station_route_summary$lon[which(station_route_summary$station_name == row_name)]) # get longitude of the "row" in coordinates table
    lat_2 <- as.numeric(station_route_summary$lat[which(station_route_summary$station_name == row_name)]) # get latitude of the "row" in coordinates table
    
    
    distance_miles <- distm(
      c(lon_1, lat_1), c(lon_2, lat_2), fun = distHaversine
    )/1609.344  # this calculates meters, so have to use meters to miles conversion formula. source for using this function and library: Claude Haiku 4.5
    
    # insert miles into table
    distances[row_index, non_station_column_index] <- distance_miles
    
  }
  
}


# distances2 <- distances %>% 
with_routes <- left_join(distances, station_route_summary %>% 
                           mutate(station = station_name) %>% 
                           select(station, total_routes))


desired_stations <- tibble()

#### Select desirable destination stations ####

for (column_index in 1:nrow(distances)) {
  non_station_column_index <- column_index+1 # to avoid referring to the "station" column
  column_name <- distances$station[column_index]
  
  closest_stations <- with_routes %>% 
    arrange(with_routes[non_station_column_index]) %>% 
    slice(2:6)  # top 5 closest stations. not 1:5, since that will include the station itself (the station itself has the shortest distance, 0 miles)
  closest_stations <- closest_stations %>% 
    arrange(desc(closest_stations$total_routes)) %>% 
    slice_head()
  
  best_stations <- tibble(
    station_name = column_name,
    best_station = closest_stations$station[1],
    distance = as.numeric(closest_stations[column_name])
  )
  
  desired_stations <- bind_rows(desired_stations, best_stations)
  
}

desired_stations_info <- left_join(desired_stations, station_route_summary %>% 
                                     select(station_name, total_routes) %>% 
                                     rename_at("total_routes",~"total_routes_station"))


# write_csv(with_routes, "distance_between_each_station.csv")
# write_csv(desired_stations_info, "desired_station_directions.csv")