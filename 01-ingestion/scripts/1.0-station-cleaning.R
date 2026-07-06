# Libraries & datasets ####
library(tidyverse)
library(lubridate)
# ride on
mcdot_calendar <- read_delim("mcdot_calendar_429.txt")
mcdot_routes <- read_delim("mcdot_routes_429.txt")
mcdot_stop_times <- read_delim("mcdot_stop_times_429.txt")
mcdot_stops <- read_delim("mcdot_stops_429.txt")
mcdot_trips <- read_delim("mcdot_trips_429.txt")
# metrobus
wmata_trips <- read_delim("jun_wmata_trips.txt")
wmata_routes <- read_delim("jun_wmata_routes.txt")
wmata_stop_times <- read_delim("jun_wmata_stop_times.txt")
wmata_stops <- read_delim("jun_wmata_stops.txt")
wmata_calendar <- read_delim("jun_wmata_calendar.txt")


# Part 1: Initial GTFS cleaning ####

wmata_stop_time_trips <- left_join(wmata_stop_times %>% 
                                     select(arrival_time, trip_id, stop_id, stop_sequence, timepoint), wmata_trips %>% 
                                     select(route_id, service_id, trip_id, trip_headsign, direction_id))
wmata_stop_time_trips_routes <- left_join(wmata_stop_time_trips, wmata_routes %>% 
                                            select(route_id, route_short_name))


mcdot_stop_time_trips <- left_join(mcdot_stop_times %>% 
                                     select(arrival_time, trip_id, stop_id, stop_sequence, timepoint), mcdot_trips %>% 
                                     select(route_id, service_id, trip_id, trip_headsign, direction_id))
mcdot_stop_time_trips_routes <- left_join(mcdot_stop_time_trips, mcdot_routes %>% 
                                            select(route_id, route_short_name))


# check calendar dates
mcdot_calendar
# service_id 1 == monday-friday
# 2 == saturday
# 3 == sunday
identical(mcdot_stop_times$arrival_time, mcdot_stop_times$departure_time)
wmata_calendar
# service_id 11 == monday-thursday
# 14 == saturday
# 13 == sunday
identical(wmata_stop_times$arrival_time, wmata_stop_times$departure_time)



## MCDOT Ride On ####
mcdot_all_stop_time_names <- left_join(mcdot_stop_times, mcdot_stops %>%
                                         select(stop_id, stop_name))
mcdot_all_stop_time_names <- mcdot_all_stop_time_names %>%
  relocate(stop_name, .after = stop_id)




##### Weekdays only ####

mcdot_weekday_trips <- mcdot_trips %>% 
  filter(service_id == 1)

mcdot_wkday_stop_time_names <- merge(mcdot_weekday_trips %>% 
                                       select(route_id, service_id, trip_id, trip_headsign, direction_id), 
                                     mcdot_all_stop_time_names)

mcdot_wkday_stop_time_names <- left_join(mcdot_wkday_stop_time_names, mcdot_routes %>% 
                                           select(route_id, route_short_name))

identical(mcdot_wkday_stop_time_names$arrival_time, mcdot_wkday_stop_time_names$departure_time)

mcdot_wkday_stop_time_names <- mcdot_wkday_stop_time_names %>%
  relocate(route_short_name, .after = route_id) %>% 
  select(-trip_headsign, # this column makes searching in the dataframe viewer difficult
         -departure_time, -stop_headsign, -pickup_type, -drop_off_type) 



##### Saturdays only ####

mcdot_saturday_trips <- mcdot_trips %>% 
  filter(service_id == 2)

mcdot_sat_stop_time_names <- merge(mcdot_saturday_trips %>% 
                                     select(route_id, service_id, trip_id, trip_headsign, direction_id), mcdot_all_stop_time_names)

mcdot_sat_stop_time_names <- left_join(mcdot_sat_stop_time_names, mcdot_routes %>% 
                                         select(route_id, route_short_name))

identical(mcdot_sat_stop_time_names$arrival_time, mcdot_sat_stop_time_names$departure_time)

mcdot_sat_stop_time_names <- mcdot_sat_stop_time_names %>%
  relocate(route_short_name, .after = route_id) %>% 
  select(-trip_headsign,
         -departure_time, -stop_headsign, -pickup_type, -drop_off_type) 



##### Sundays only ####

mcdot_sunday_trips <- mcdot_trips %>% 
  filter(service_id == 3)

mcdot_sun_stop_time_names <- merge(mcdot_sunday_trips %>% 
                                     select(route_id, service_id, trip_id, trip_headsign, direction_id), mcdot_all_stop_time_names)

mcdot_sun_stop_time_names <- left_join(mcdot_sun_stop_time_names, mcdot_routes %>% 
                                         select(route_id, route_short_name))

identical(mcdot_sun_stop_time_names$arrival_time, mcdot_sun_stop_time_names$departure_time)

mcdot_sun_stop_time_names <- mcdot_sun_stop_time_names %>%
  relocate(route_short_name, .after = route_id) %>% 
  select(-trip_headsign,
         -departure_time, -stop_headsign, -pickup_type, -drop_off_type)



## WMATA Metrobus ####

wmata_all_stop_time_names <- left_join(wmata_stop_times, wmata_stops %>%
                                         select(stop_id, stop_name))
wmata_all_stop_time_names <- wmata_all_stop_time_names %>%
  relocate(stop_name, .after = stop_id)


##### Weekdays only ####

wmata_weekday_trips <- wmata_trips %>% 
  filter(service_id == 11)

wmata_wkday_stop_time_names <- merge(wmata_weekday_trips %>% 
                                       select(route_id, service_id, trip_id, trip_headsign, direction_id), 
                                     wmata_all_stop_time_names)

wmata_wkday_stop_time_names <- left_join(wmata_wkday_stop_time_names, wmata_routes %>% 
                                           select(route_id, route_short_name))

identical(wmata_wkday_stop_time_names$arrival_time, wmata_wkday_stop_time_names$departure_time)

wmata_wkday_stop_time_names <- wmata_wkday_stop_time_names %>%
  relocate(route_short_name, .after = route_id) %>% 
  select(-trip_headsign, -departure_time, -stop_headsign, -pickup_type, -drop_off_type) 


##### Saturdays only ####

wmata_saturday_trips <- wmata_trips %>% 
  filter(service_id == 14)

wmata_sat_stop_time_names <- merge(wmata_saturday_trips %>% 
                                     select(route_id, service_id, trip_id, trip_headsign, direction_id), wmata_all_stop_time_names)

wmata_sat_stop_time_names <- left_join(wmata_sat_stop_time_names, wmata_routes %>% 
                                         select(route_id, route_short_name))

identical(wmata_sat_stop_time_names$arrival_time, wmata_sat_stop_time_names$departure_time)

wmata_sat_stop_time_names <- wmata_sat_stop_time_names %>%
  relocate(route_short_name, .after = route_id) %>% 
  select(-trip_headsign, -departure_time, -stop_headsign, -pickup_type, -drop_off_type) 


##### Sundays only ####

wmata_sunday_trips <- wmata_trips %>% 
  filter(service_id == 13)

wmata_sun_stop_time_names <- merge(wmata_sunday_trips %>% 
                                     select(route_id, service_id, trip_id, trip_headsign, direction_id), wmata_all_stop_time_names)

wmata_sun_stop_time_names <- left_join(wmata_sun_stop_time_names, wmata_routes %>% 
                                         select(route_id, route_short_name))

identical(wmata_sun_stop_time_names$arrival_time, wmata_sun_stop_time_names$departure_time)

wmata_sun_stop_time_names <- wmata_sun_stop_time_names %>%
  relocate(route_short_name, .after = route_id) %>% 
  select(-trip_headsign, -departure_time, -stop_headsign, -pickup_type, -drop_off_type)



# Part 2: Station info ####


## MCDOT Ride On ####
# starting with ride on to filter to only stations in montgomery county
# after filtering to stations with ride on, will match those stations in the metrobus data

# subset to only stations/transfer centers, by selecting stops that have "BAY ", "STATION", or "TRANSIT CENTER" in them
# first, creating a list of all the "stations" I will be using:
mcdot_stops_weekday_stations <- subset(mcdot_wkday_stop_time_names, !grepl("CINNAMON DR & BAY LEAF WAY", mcdot_wkday_stop_time_names$stop_name))
mcdot_stops_weekday_stations <- subset(mcdot_stops_weekday_stations, grepl("& BAY |TRANSIT CENTER", mcdot_stops_weekday_stations$stop_name))

mcdot_weekday_stations_list <- mcdot_stops_weekday_stations %>% 
  # first: remove 3 stations not in Montgomery County
  filter(!grepl("LANGLEY|FRIENDSHIP HEIGHTS|TAKOMA STATION", stop_name)) %>% 
  mutate(
    station_name = str_extract(stop_name, "[^&]*&"),
    station_name = gsub(" &", "", station_name),
    station_name = gsub("-", "", station_name),
    agency = "Montgomery County MD Ride On"
  ) %>% 
  distinct(station_name, .keep_all = TRUE)
mcdot_stations <- mcdot_weekday_stations_list$station_name
# create a list of transit centers (will be used with metrobus data)
mcdot_transit_centers <- mcdot_weekday_stations_list %>% 
  filter(grepl("TRANSIT", station_name)) %>% 
  mutate(
    simpler_name = gsub("TRANSIT CENTER", "", station_name)
  )
mcdot_transit_centers <- mcdot_transit_centers$simpler_name


# Weekday
# there are several issues with "CINNAMON DR & BAY LEAF WAY", "WARING STATION RD", and "STATION 40 DR" ending up in the results despite not being stations, but no other issues, so these are being removed individually
mcdot_stops_weekday_stations <- subset(mcdot_wkday_stop_time_names, !grepl("WARING STATION RD|CINNAMON DR & BAY LEAF WAY|STATION 40 DR", mcdot_wkday_stop_time_names$stop_name))
mcdot_stops_weekday_stations <- subset(mcdot_stops_weekday_stations, grepl("STATION|& BAY |TRANSIT CENTER", mcdot_stops_weekday_stations$stop_name))
# Saturday
mcdot_stops_saturday_stations <- subset(mcdot_sat_stop_time_names, !grepl("WARING STATION RD|CINNAMON DR & BAY LEAF WAY|STATION 40 DR", mcdot_sat_stop_time_names$stop_name))
mcdot_stops_saturday_stations <- subset(mcdot_stops_saturday_stations, grepl("STATION|& BAY |TRANSIT CENTER", mcdot_stops_saturday_stations$stop_name))
# Sunday
mcdot_stops_sunday_stations <- subset(mcdot_sun_stop_time_names, !grepl("WARING STATION RD|CINNAMON DR & BAY LEAF WAY|STATION 40 DR", mcdot_sun_stop_time_names$stop_name))
mcdot_stops_sunday_stations <- subset(mcdot_stops_sunday_stations, grepl("STATION|& BAY |TRANSIT CENTER", mcdot_stops_sunday_stations$stop_name))

# Weekday
mcdot_weekday_stations_arrivals <- mcdot_stops_weekday_stations %>% 
  mutate(stop_name = gsub("-", "", stop_name)) %>%
  mutate(station_name = str_extract(stop_name, paste(mcdot_stations, collapse = "|")), 
         agency = "Montgomery County MD Ride On"
  ) %>% 
  relocate(station_name, .after = stop_name)

# Saturday
mcdot_saturday_stations_arrivals <- mcdot_stops_saturday_stations %>% 
  mutate(stop_name = gsub("-", "", stop_name)) %>%
  mutate(station_name = str_extract(stop_name, paste(mcdot_stations, collapse = "|")), 
         agency = "Montgomery County MD Ride On"
  ) %>% 
  relocate(station_name, .after = stop_name)

# Sunday
mcdot_sunday_stations_arrivals <- mcdot_stops_sunday_stations %>% 
  mutate(stop_name = gsub("-", "", stop_name)) %>%
  mutate(station_name = str_extract(stop_name, paste(mcdot_stations, collapse = "|")), 
         agency = "Montgomery County MD Ride On"
  ) %>% 
  relocate(station_name, .after = stop_name)

## WMATA Metrobus ####

# capitalize the stop names to match ride on formatting
wmata_wkday_stop_time_names <- wmata_wkday_stop_time_names %>%
  mutate(
    upper_stop = toupper(stop_name)
  )
wmata_sat_stop_time_names <- wmata_sat_stop_time_names %>% 
  mutate(
    upper_stop = toupper(stop_name)
  )
wmata_sun_stop_time_names <- wmata_sun_stop_time_names %>% 
  mutate(
    upper_stop = toupper(stop_name)
  )

# Weekday
wmata_stops_weekday_stations <- subset(wmata_wkday_stop_time_names, grepl("+BAY |STATION|TRANSIT CENTER", wmata_wkday_stop_time_names$upper_stop)) # filter to just "stations"

wmata_weekday_stations_arrivals <- wmata_stops_weekday_stations %>% 
  mutate(
    upper_stop = gsub("-", "", upper_stop), # remove any - 
    station_name = str_extract(upper_stop, "[^+]*+"), # remove any + and anything after the + (eg, GROSVENOR STATION + BAY)
    station_name = ifelse(!grepl("STATION", station_name), paste(station_name, "STATION"), station_name), # add "station" to any rows that don't have "station" in the name
    station_name = ifelse( # any stations that are referred to as "transit centers" in the mcdot station list, must be renamed to match the mcdot list format
      gsub("STATION", "", station_name) %in% mcdot_transit_centers, # if, when removing "STATION", the name of the stop appears in the mcdot transit centers vector,
      gsub("STATION", "TRANSIT CENTER", station_name), station_name), # replace "STATION" in "TRANSIT CENTER". otherwise, leave as is.
    agency = "WMATA") %>% 
  # remove any stations that are not included in the montgomery county station list
  filter(
    grepl(paste0(mcdot_stations, collapse = "|"), station_name)
  )

# Saturday
wmata_stops_sat_stations <- subset(wmata_sat_stop_time_names, grepl("+BAY |STATION|TRANSIT CENTER", wmata_sat_stop_time_names$upper_stop))
# 
wmata_saturday_stations_arrivals <- wmata_stops_sat_stations %>% 
  mutate(
    upper_stop = gsub("-", "", upper_stop),
    station_name = str_extract(upper_stop, "[^+]*+"),
    station_name = ifelse(!grepl("STATION", station_name), paste(station_name, "STATION"), station_name),
    station_name = ifelse(
      gsub("STATION", "", station_name) %in% mcdot_transit_centers, gsub("STATION", "TRANSIT CENTER", station_name), station_name),
    agency = "WMATA") %>% 
  filter(
    grepl(paste0(mcdot_stations, collapse = "|"), station_name)
  )


# Sunday
wmata_stops_sun_stations <- subset(wmata_sun_stop_time_names, grepl("+BAY |STATION|TRANSIT CENTER", wmata_sun_stop_time_names$upper_stop))
wmata_sunday_stations_arrivals <- wmata_stops_sun_stations %>% 
  mutate(
    upper_stop = gsub("-", "", upper_stop),
    station_name = str_extract(upper_stop, "[^+]*+"),
    station_name = ifelse(!grepl("STATION", station_name), paste(station_name, "STATION"), station_name),
    station_name = ifelse(
      gsub("STATION", "", station_name) %in% mcdot_transit_centers, gsub("STATION", "TRANSIT CENTER", station_name), station_name),
    agency = "WMATA") %>% 
  filter(
    grepl(paste0(mcdot_stations, collapse = "|"), station_name)
  )



## Join Ride On and Metrobus ####

# first, convert "route_id" to character for ride on to make joinable
mcdot_weekday_stations_arrivals$route_id <- as.character(mcdot_weekday_stations_arrivals$route_id)
mcdot_saturday_stations_arrivals$route_id <- as.character(mcdot_saturday_stations_arrivals$route_id)
mcdot_sunday_stations_arrivals$route_id <- as.character(mcdot_sunday_stations_arrivals$route_id)

# weekday
weekday_stations_arrivals <- bind_rows(mcdot_weekday_stations_arrivals, wmata_weekday_stations_arrivals)
# saturday
saturday_stations_arrivals <- bind_rows(mcdot_saturday_stations_arrivals, wmata_saturday_stations_arrivals)
# sunday
sunday_stations_arrivals <- bind_rows(mcdot_sunday_stations_arrivals, wmata_sunday_stations_arrivals)



# Part 3: Station times ####

# first: make list of all of the stations with the stop_ids
stations_ids <- weekday_stations_arrivals %>%
  select(stop_id, stop_name, station_name, agency)
stations_ids_distinct <- stations_ids %>% 
  distinct(.keep_all = TRUE) %>% 
  filter(!is.na(station_name))
mcdot_stations <- mcdot_weekday_stations_arrivals %>% 
  distinct(station_name, .keep_all = TRUE) %>% 
  filter(!is.na(station_name)) # "ROCKVILLE PIKE" is listed here as a stop, so it has an NA for station
mcdot_stations <- mcdot_stations$station_name



## MCDOT Ride On ####

mcdot_stop_time_trips <- left_join(mcdot_stop_times, mcdot_trips %>% 
                                     select(trip_id, route_id, direction_id, trip_headsign))
mcdot_stop_time_routes <- left_join(mcdot_stop_time_trips, mcdot_routes %>% 
                                      select(route_id, route_short_name)) %>% 
  relocate(c(trip_headsign, route_short_name, direction_id), .after = stop_id)

mcdot_stop_time_routes_names <- left_join(mcdot_stop_time_routes, mcdot_stops %>% 
                                            select(stop_id, stop_name))

# service_id 1 == monday-friday
# 2 == saturday
# 3 == sunday

mcdot_WKDAY_trips <- mcdot_trips %>% 
  filter(service_id == 1)
mcdot_WKDAY_trips <- mcdot_WKDAY_trips$trip_id
#
mcdot_SATURDAY_trips <- mcdot_trips %>% 
  filter(service_id == 2)
mcdot_SATURDAY_trips <- mcdot_SATURDAY_trips$trip_id
#
mcdot_SUNDAY_trips <- mcdot_trips %>% 
  filter(service_id == 3)
mcdot_SUNDAY_trips <- mcdot_SUNDAY_trips$trip_id


### Filter to station names ####
mcdot_stations_stop_times <- mcdot_stop_time_routes_names %>% 
  filter(trip_id %in% mcdot_WKDAY_trips)


### Filter & clean ####
# subset to the stations only
mcdot_stations_stop_times <- mcdot_stop_time_routes_names %>% 
  filter(stop_id %in% stations_ids_distinct$stop_id)
mcdot_stations_times <- left_join(mcdot_stations_stop_times, stations_ids_distinct)
mcdot_stations_times <- mcdot_stations_times %>% 
  filter(station_name %in% mcdot_stations)

# filter subset to weekdays
mcdot_WKDAYS_stations_times <- mcdot_stations_times %>% 
  filter(trip_id %in% mcdot_WKDAY_trips)
# saturdays
mcdot_SATURDAYS_stations_times <- mcdot_stations_times %>% 
  filter(trip_id %in% mcdot_SATURDAY_trips)
# sundays
mcdot_SUNDAYS_stations_times <- mcdot_stations_times %>% 
  filter(trip_id %in% mcdot_SUNDAY_trips)



## WMATA Metrobus ####

wmata_stop_time_trips <- left_join(wmata_stop_times, wmata_trips %>% 
                                     select(trip_id, route_id, direction_id, trip_headsign))
wmata_stop_time_routes <- left_join(wmata_stop_time_trips, wmata_routes %>% 
                                      select(route_id, route_short_name)) %>% 
  relocate(c(trip_headsign, route_short_name, direction_id), .after = stop_id)
wmata_stop_time_routes_names <- left_join(wmata_stop_time_routes, wmata_stops %>% 
                                            select(stop_id, stop_name))

# service_id 11 == monday-thursday
# 14 == saturday
# 13 == sunday

wmata_WKDAY_trips <- wmata_trips %>% 
  filter(service_id == 11)
wmata_WKDAY_trips <- wmata_WKDAY_trips$trip_id
#
wmata_SATURDAY_trips <- wmata_trips %>% 
  filter(service_id == 14)
wmata_SATURDAY_trips <- wmata_SATURDAY_trips$trip_id
#
wmata_SUNDAY_trips <- wmata_trips %>% 
  filter(service_id == 13)
wmata_SUNDAY_trips <- wmata_SUNDAY_trips$trip_id


### Filter & clean ####
# subset to the stations only
wmata_stations_stop_times <- wmata_stop_time_routes_names %>% 
  filter(stop_id %in% stations_ids_distinct$stop_id)
wmata_stations_times <- left_join(wmata_stations_stop_times, stations_ids_distinct)
wmata_stations_times <- wmata_stations_times %>% 
  filter(station_name %in% mcdot_stations)

# filter subset to weekdays
wmata_WKDAYS_stations_times <- wmata_stations_times %>% 
  filter(trip_id %in% wmata_WKDAY_trips)
# saturdays
wmata_SATURDAYS_stations_times <- wmata_stations_times %>% 
  filter(trip_id %in% wmata_SATURDAY_trips)
# sundays
wmata_SUNDAYS_stations_times <- wmata_stations_times %>% 
  filter(trip_id %in% wmata_SUNDAY_trips)


## Combine WMATA & MCDOT ####
mcdot_stations_times$route_id <- as.character(mcdot_stations_times$route_id)
#
mcdot_WKDAYS_stations_times$route_id <- as.character(mcdot_WKDAYS_stations_times$route_id)
mcdot_SATURDAYS_stations_times$route_id <- as.character(mcdot_SATURDAYS_stations_times$route_id)
mcdot_SUNDAYS_stations_times$route_id <- as.character(mcdot_SUNDAYS_stations_times$route_id)
##
station_times <- bind_rows(mcdot_stations_times, wmata_stations_times)
#
weekday_station_times <- bind_rows(mcdot_WKDAYS_stations_times, wmata_WKDAYS_stations_times)
saturday_station_times <- bind_rows(mcdot_SATURDAYS_stations_times, wmata_SATURDAYS_stations_times)
sunday_station_times <- bind_rows(mcdot_SUNDAYS_stations_times, wmata_SUNDAYS_stations_times)

# Download ####

# from Part 2
# write_csv(mcdot_weekday_stations_arrivals, "mcdot_weekday_stations_arrivals.csv")
# write_csv(wmata_weekday_stations_arrivals, "wmata_weekday_stations_arrivals.csv")

# from Part 3
# write_csv(weekday_station_times, "weekday_station_times.csv")
# write_csv(saturday_station_times, "saturday_station_times.csv")
# write_csv(sunday_station_times, "sunday_station_times.csv")