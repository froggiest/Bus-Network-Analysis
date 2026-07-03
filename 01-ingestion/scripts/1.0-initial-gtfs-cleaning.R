# in this script I am getting the stop names for specific day schedules
# libraries & datasets
library(tidyverse)
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

# check some things

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
  select(-trip_headsign, # this would confuse searches for stations etc
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
  select(-trip_headsign, # this would confuse searches for stations etc
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
  select(-trip_headsign, # this would confuse searches for stations etc
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
  select(-trip_headsign, # this would confuse searches for stations etc
         -departure_time, -stop_headsign, -pickup_type, -drop_off_type) 


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
  select(-trip_headsign, # this would confuse searches for stations etc
         -departure_time, -stop_headsign, -pickup_type, -drop_off_type) 


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
  select(-trip_headsign, # this would confuse searches for stations etc
         -departure_time, -stop_headsign, -pickup_type, -drop_off_type)



## Download ####

# write_csv(mcdot_sun_stop_time_names, "mcdot_sun_stop_time_names.csv")
# write_csv(mcdot_sat_stop_time_names, "mcdot_sat_stop_time_names.csv")
# write_csv(mcdot_wkday_stop_time_names, "mcdot_wkday_stop_time_names.csv")
# write_csv(wmata_sun_stop_time_names, "wmata_sun_stop_time_names.csv")
# write_csv(wmata_sat_stop_time_names, "wmata_sat_stop_time_names.csv")
# write_csv(wmata_wkday_stop_time_names, "wmata_wkday_stop_time_names.csv")