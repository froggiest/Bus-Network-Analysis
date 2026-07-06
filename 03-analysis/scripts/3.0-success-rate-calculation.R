# libraries
library(tidyverse)
library(lubridate)
library(hms)
# datasets
weekday_station_times <- read_csv("weekday_station_times.csv")
saturday_station_times <- read_csv("saturday_station_times.csv")
sunday_station_times <- read_csv("sunday_station_times.csv")
desired_stations <- read_csv("desired_station_directions.csv")
# add "desired stations"
weekday_station_times <- left_join(weekday_station_times, desired_stations)
saturday_station_times <- left_join(saturday_station_times, desired_stations)
sunday_station_times <- left_join(sunday_station_times, desired_stations)


### Fix time issue ####
# times past midnight on a single trip are listed with hours above 23:00:00, which needs to be reformatted.
# this is an inefficient fix, but it works!

weekday_time_fix <- weekday_station_times %>% 
  mutate(
    hours = ifelse(hour(arrival_time) < 10,
                   paste0(0, hour(arrival_time)), 
                   hour(arrival_time)),
    minutes = ifelse(minute(arrival_time) < 10, 
                     paste0(0, minute(arrival_time)),
                     minute(arrival_time)),
    seconds = ifelse(second(arrival_time) < 10, 
                     paste0(0, second(arrival_time)),
                     second(arrival_time))
    ) %>% 
  mutate(
    clean_arrival_time = hms::as_hms(paste0(hours, ":", minutes, ":", seconds)) # source for hms::as_hms -- Claude Haiku 4.5
    ) %>% 
  relocate(clean_arrival_time, .after = arrival_time)

class(weekday_time_fix$arrival_time)
class(weekday_time_fix$clean_arrival_time)

weekday_time_check <- weekday_time_fix %>%
  filter(hours > 2)

identical(weekday_time_check$clean_arrival_time, weekday_time_check$arrival_time)

weekday_station_times <- weekday_time_fix %>% 
  select(-departure_time, -hours,-minutes, -seconds)

# saturday
saturday_time_fix <- saturday_station_times %>% 
  mutate(
    hours = ifelse(hour(arrival_time) < 10,paste0(0, hour(arrival_time)),hour(arrival_time)),
    minutes = ifelse(minute(arrival_time) < 10, paste0(0, minute(arrival_time)),minute(arrival_time)),
    seconds = ifelse(second(arrival_time) < 10, paste0(0, second(arrival_time)),second(arrival_time))) %>% 
  mutate(clean_arrival_time = hms::as_hms(paste0(hours, ":", minutes, ":", seconds))) %>% 
  relocate(clean_arrival_time, .after = arrival_time)
#
saturday_time_check <- saturday_time_fix %>%
  filter(hours > 2)
identical(saturday_time_check$clean_arrival_time, saturday_time_check$arrival_time)
saturday_station_times <- saturday_time_fix %>% 
  select(-departure_time, -hours,-minutes, -seconds)

# sunday
sunday_time_fix <- sunday_station_times %>% 
  mutate(
    hours = ifelse(hour(arrival_time) < 10,paste0(0, hour(arrival_time)),hour(arrival_time)),
    minutes = ifelse(minute(arrival_time) < 10, paste0(0, minute(arrival_time)),minute(arrival_time)),
    seconds = ifelse(second(arrival_time) < 10, paste0(0, second(arrival_time)),second(arrival_time))) %>% 
  mutate(clean_arrival_time = hms::as_hms(paste0(hours, ":", minutes, ":", seconds))) %>% 
  relocate(clean_arrival_time, .after = arrival_time)
#
sunday_time_check <- sunday_time_fix %>%
  filter(hours > 2)
identical(sunday_time_check$clean_arrival_time, sunday_time_check$arrival_time)
sunday_station_times <- sunday_time_fix %>% 
  select(-departure_time, -hours,-minutes, -seconds)


## Define time spans ####
# weekday am peak
am_peak <- c(lubridate::hms("05:30:00"), lubridate::hms("09:30:00")) # lubridate::hms result of debugging a line with Claude Haiku 4.5
am_offpeak <- c(lubridate::hms("09:31:00"), lubridate::hms("15:29:00"))
pm_peak <- c(lubridate::hms("15:30:00"), lubridate::hms("20:22:00"))
pm_offpeak <- c(lubridate::hms("20:23:00"), lubridate::hms("23:59:00"))
overnight_hours <- c(lubridate::hms("00:00:00"), lubridate::hms("05:29:00")) 
day_hours <- c(lubridate::hms("07:00:00"), lubridate::hms("19:00:00"))

## Function ####

weekdayBusSuccessRate <- function(station, timespan) {
  
  destination <- weekday_station_times$best_station[weekday_station_times$station_name == station][1]
  
  trials <- 0
  success <- 0
  
  if (hour(timespan[2]) > hour(timespan[1])) {
    start_to_end_seconds <- as.numeric(timespan[2] - timespan[1]) # some debugging/troubleshooting with Claude Haiku 4.5 
    start_to_end_minutes <- start_to_end_seconds/60
  }
  if  (hour(timespan[2]) < hour(timespan[1])) {
    start_to_end_seconds <- as.numeric(timespan[1] - timespan[2])
    start_to_end_minutes <- start_to_end_seconds/60
  }
  

  for (increment_min in 1:start_to_end_minutes) {
    
    if (increment_min == 1) {
      current_time = timespan[1]
    } else if (increment_min > 1) {
      current_time = timespan[1] + minutes(increment_min)
    }
    
    
    # search for arrivals/departures
    
    # 1) subset to all buses departing from this station in the next 10 minutes
    tripsDeparting <- weekday_station_times %>% 
      filter(
        station_name == station
      ) %>% 
      filter(clean_arrival_time == current_time + minutes(1) | clean_arrival_time == current_time + minutes(2) | clean_arrival_time == current_time + minutes(3) | clean_arrival_time == current_time + minutes(4) | clean_arrival_time == current_time + minutes(5) | clean_arrival_time == current_time + minutes(6) | clean_arrival_time == current_time + minutes(7) | clean_arrival_time == current_time + minutes(8) | clean_arrival_time == current_time + minutes(9) | clean_arrival_time == current_time + minutes(10)) # really need to replace this line with something less ridiculous
    
    # 2) filter subset to only the trips that arrive at the desirable destination
    tripsArrivingAtDesired <- weekday_station_times %>% 
      filter(trip_id %in% tripsDeparting$trip_id) %>% # bus trips that arrived at the current station within 10 minutes, as defined above
      group_by(trip_id) %>% # necessary
      filter(station_name == destination # check these trips for stops at the desirable destination
             & # and make sure 
               stop_sequence > stop_sequence[which(station_name == station)] # that these stops come AFTER the stop at the origin station (aka, that the buses are going in the right direction)
      )
    
    # 3) filter original subset to only the buses that are departing to the destination (not really necessary, but it's neat)
    tripsDepartingToDesired <- tripsDeparting %>% 
      filter(trip_id %in% tripsArrivingAtDesired$trip_id)
    
    
    # check for success
    if (nrow(tripsDepartingToDesired) >= 1) {
      success <- success+1
      trials <- trials+1
    } else if (nrow(tripsDepartingToDesired) == 0) {
      trials <- trials+1
    }
  }
  
  success_rate <- success/trials
  success_percentage <- paste0(round(success_rate*100,3), "%")
  
  message(paste0("Chance of catching the bus to ", destination, " within 10 minutes of arrival at ", station, " between ", timespan[1], " and ", timespan[2], ": ", success_percentage))
  message(success)
  message("—")
  message(trials)
  
  return(success_rate)
  
  
  
}

saturdayBusSuccessRate <- function(station, timespan) {
  
  destination <- saturday_station_times$best_station[saturday_station_times$station_name == station][1]
  
  trials <- 0
  success <- 0
  
  if (hour(timespan[2]) > hour(timespan[1])) {
    start_to_end_seconds <- as.numeric(timespan[2] - timespan[1]) 
    start_to_end_minutes <- start_to_end_seconds/60
  }
  if  (hour(timespan[2]) < hour(timespan[1])) {
    start_to_end_seconds <- as.numeric(timespan[1] - timespan[2])
    start_to_end_minutes <- start_to_end_seconds/60
  }
  

  for (increment_min in 1:start_to_end_minutes) {
    
    if (increment_min == 1) {
      current_time = timespan[1]
    } else if (increment_min > 1) {
      current_time = timespan[1] + minutes(increment_min)
    }
    
    

    # 1) subset to all buses departing from this station in this time span
    tripsDeparting <- saturday_station_times %>% 
      filter(
        station_name == station
      ) %>% 
      filter(clean_arrival_time == current_time + minutes(1) | clean_arrival_time == current_time + minutes(2) | clean_arrival_time == current_time + minutes(3) | clean_arrival_time == current_time + minutes(4) | clean_arrival_time == current_time + minutes(5) | clean_arrival_time == current_time + minutes(6) | clean_arrival_time == current_time + minutes(7) | clean_arrival_time == current_time + minutes(8) | clean_arrival_time == current_time + minutes(9) | clean_arrival_time == current_time + minutes(10))
    
    # 2) filter subset to only the trips that arrive at the desirable destination
    tripsArrivingAtDesired <- saturday_station_times %>% 
      filter(trip_id %in% tripsDeparting$trip_id) %>% # bus trips that arrived at the current station within 10 minutes, as defined above
      group_by(trip_id) %>%
      filter(station_name == destination # check these trips for stops at the desired destination
             &
               stop_sequence > stop_sequence[which(station_name == station)] # ensure these stops come AFTER the stop at the origin station (meaning that the buses are going in the right direction)
      )
    
    # 3) filter original subset to only the buses that are departing to the destination
    tripsDepartingToDesired <- tripsDeparting %>% 
      filter(trip_id %in% tripsArrivingAtDesired$trip_id)
    

    # check success
    if (nrow(tripsDepartingToDesired) >= 1) {
      success <- success+1
      trials <- trials+1
    } else if (nrow(tripsDepartingToDesired) == 0) {
      trials <- trials+1
    }
  }
  
  success_rate <- success/trials
  success_percentage <- paste0(round(success_rate*100,3), "%")
  
  message(paste0("Chance of catching the bus to ", destination, " within 10 minutes of arrival at ", station, " between ", timespan[1], " and ", timespan[2], " on a Saturday: ", success_percentage))
  message(success)
  message("—")
  message(trials)
  
  return(success_rate)
  
  
  
}

sundayBusSuccessRate <- function(station, timespan) {
  
  destination <- sunday_station_times$best_station[sunday_station_times$station_name == station][1]
  
  trials <- 0
  success <- 0
  
  if (hour(timespan[2]) > hour(timespan[1])) {
    start_to_end_seconds <- as.numeric(timespan[2] - timespan[1])
    start_to_end_minutes <- start_to_end_seconds/60
  }
  if  (hour(timespan[2]) < hour(timespan[1])) {
    start_to_end_seconds <- as.numeric(timespan[1] - timespan[2])
    start_to_end_minutes <- start_to_end_seconds/60
  }
  
  
  for (increment_min in 1:start_to_end_minutes) {
    
    if (increment_min == 1) {
      current_time = timespan[1]
    } else if (increment_min > 1) {
      current_time = timespan[1] + minutes(increment_min)
    }
    
    

    # 1) subset to all buses departing from this station in this time span
    tripsDeparting <- sunday_station_times %>% 
      filter(
        station_name == station
      ) %>% 
      filter(clean_arrival_time == current_time + minutes(1) | clean_arrival_time == current_time + minutes(2) | clean_arrival_time == current_time + minutes(3) | clean_arrival_time == current_time + minutes(4) | clean_arrival_time == current_time + minutes(5) | clean_arrival_time == current_time + minutes(6) | clean_arrival_time == current_time + minutes(7) | clean_arrival_time == current_time + minutes(8) | clean_arrival_time == current_time + minutes(9) | clean_arrival_time == current_time + minutes(10))
    
    # 2) filter subset to only the trips that arrive at the desirable destination
    tripsArrivingAtDesired <- sunday_station_times %>% 
      filter(trip_id %in% tripsDeparting$trip_id) %>% 
      group_by(trip_id) %>%
      filter(station_name == destination 
             &
               stop_sequence > stop_sequence[which(station_name == station)]
      )
    
    # 3) filter original subset to only the buses that are departing to the destination
    tripsDepartingToDesired <- tripsDeparting %>% 
      filter(trip_id %in% tripsArrivingAtDesired$trip_id)
    

    # check success
    if (nrow(tripsDepartingToDesired) >= 1) {
      success <- success+1
      trials <- trials+1
    } else if (nrow(tripsDepartingToDesired) == 0) {
      trials <- trials+1
    }
  }
  
  success_rate <- success/trials
  success_percentage <- paste0(round(success_rate*100,3), "%")
  
  message(paste0("Chance of catching the bus to ", destination, " within 10 minutes of arrival at ", station, " between ", timespan[1], " and ", timespan[2], " on a Sunday: ", success_percentage))
  message(success)
  message("—")
  message(trials)
  
  return(success_rate)
  
  
  
}


## Create success rate table ####

## Empty success rate table ####
empty_rate_table <- tibble(
  station_name = desired_stations$station_name,
  weekday_am_peak = rep(NA, length(station_name)),
  weekday_am_offpeak = rep(NA, length(station_name)),
  weekday_pm_peak = rep(NA, length(station_name)),
  weekday_pm_offpeak = rep(NA, length(station_name)),
  weekday_day_hours = rep(NA, length(station_name)),
  weekday_overnight_hours = rep(NA, length(station_name)),
  saturday_am_peak = rep(NA, length(station_name)),
  saturday_am_offpeak = rep(NA, length(station_name)),
  saturday_pm_peak = rep(NA, length(station_name)),
  saturday_pm_offpeak = rep(NA, length(station_name)),
  saturday_day_hours = rep(NA, length(station_name)),
  saturday_overnight_hours = rep(NA, length(station_name)),
  sunday_am_peak = rep(NA, length(station_name)),
  sunday_am_offpeak = rep(NA, length(station_name)),
  sunday_pm_peak = rep(NA, length(station_name)),
  sunday_pm_offpeak = rep(NA, length(station_name)),
  sunday_day_hours = rep(NA, length(station_name)),
  sunday_overnight_hours = rep(NA, length(station_name))
)

## Loop success rate ####

for (given_station in 1:nrow(empty_rate_table)) {
  # select the current station
  current_station <- empty_rate_table$station_name[given_station]
  
  # weekday am peak
  empty_rate_table$weekday_am_peak[given_station] <- weekdayBusSuccessRate(current_station, am_peak)
  print(paste("completed part 1 of loop", given_station))
  
  # weekday am offpeak
  empty_rate_table$weekday_am_offpeak[given_station] <- weekdayBusSuccessRate(current_station, am_offpeak)
  print(paste("completed part 2 of loop", given_station))
  
  # weekday pm peak
  empty_rate_table$weekday_pm_peak[given_station] <- weekdayBusSuccessRate(current_station, pm_peak)
  print(paste("completed part 3 of loop", given_station))
  
  # weekday pm offpeak
  empty_rate_table$weekday_pm_offpeak[given_station] <- weekdayBusSuccessRate(current_station, pm_offpeak)
  print(paste("completed part 4 of loop", given_station))
  
  # weekday day hours
  empty_rate_table$weekday_day_hours[given_station] <- weekdayBusSuccessRate(current_station, day_hours)
  print(paste("completed part 5 of loop", given_station))
  
  # weekday overnight hours
  empty_rate_table$weekday_overnight_hours[given_station] <- weekdayBusSuccessRate(current_station, overnight_hours)
  print(paste("completed part 6 of loop", given_station))
  
  #### SATURDAY ####
  
  # saturday am peak
  empty_rate_table$saturday_am_peak[given_station] <- saturdayBusSuccessRate(current_station, am_peak)
  print(paste("completed part 7 of loop", given_station))
  
  # saturday am offpeak
  empty_rate_table$saturday_am_offpeak[given_station] <- saturdayBusSuccessRate(current_station, am_offpeak)
  print(paste("completed part 8 of loop", given_station))
  
  # saturday pm peak
  empty_rate_table$saturday_pm_peak[given_station] <- saturdayBusSuccessRate(current_station, pm_peak)
  print(paste("completed part 9 of loop", given_station))
  
  # saturday pm offpeak
  empty_rate_table$saturday_pm_offpeak[given_station] <- saturdayBusSuccessRate(current_station, pm_offpeak)
  print(paste("completed part 10 of loop", given_station))
  
  # saturday day hours
  empty_rate_table$saturday_day_hours[given_station] <- saturdayBusSuccessRate(current_station, day_hours)
  print(paste("completed part 11 of loop", given_station))
  
  # saturday overnight hours
  empty_rate_table$saturday_overnight_hours[given_station] <- saturdayBusSuccessRate(current_station, overnight_hours)
  print(paste("completed part 12 of loop", given_station))
  
  
  #### SUNDAY ####
  
  # sunday am peak
  empty_rate_table$sunday_am_peak[given_station] <- sundayBusSuccessRate(current_station, am_peak)
  print(paste("completed part 13 of loop", given_station))
  
  # sunday am offpeak
  empty_rate_table$sunday_am_offpeak[given_station] <- sundayBusSuccessRate(current_station, am_offpeak)
  print(paste("completed part 14 of loop", given_station))
  
  # sunday pm peak
  empty_rate_table$sunday_pm_peak[given_station] <- sundayBusSuccessRate(current_station, pm_peak)
  print(paste("completed part 15 of loop", given_station))
  
  # sunday pm offpeak
  empty_rate_table$sunday_pm_offpeak[given_station] <- sundayBusSuccessRate(current_station, pm_offpeak)
  print(paste("completed part 16 of loop", given_station))
  
  # sunday day hours
  empty_rate_table$sunday_day_hours[given_station] <- sundayBusSuccessRate(current_station, day_hours)
  print(paste("completed part 17 of loop", given_station))
  
  # sunday overnight hours
  empty_rate_table$sunday_overnight_hours[given_station] <- sundayBusSuccessRate(current_station, overnight_hours)
  print(paste("completed part 18 of loop", given_station))
  
}


rate_table <- left_join(empty_rate_table, desired_stations)
completed_table_table <- rate_table %>% 
  mutate(
    difference_am_peak_to_off = weekday_am_peak-weekday_am_offpeak
  ) %>% 
  rename_at("best_station",~"destination_station") %>% 
  rename_at("station_name",~"origin_station") %>% 
  rename_at("total_routes_station",~"total_routes_origin") %>% 
  relocate(difference_am_peak_to_off, .after = weekday_am_peak)

# write_csv(rate_table, "bus_success_rates_by_station.csv")