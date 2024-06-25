source("(2) load_libraries.R")
source("(4) population_data.R")
source("(3) paths.R")
load_libraries()

# Cleans and aggregates temperature data used at various points in our analysis

write_cleaned_temp_data <- function() {
  max_temperature_data <- read.csv(file.path(TEMPERATURE_DATA_DIR, "max_temperature_per_county_per_day_20002022.csv"))
  min_temperature_data <- read.csv(file.path(TEMPERATURE_DATA_DIR, "min_temperature_per_county_per_day_20002022.csv"))
  mean_temperature_data <- read.csv(file.path(TEMPERATURE_DATA_DIR, "mean_temperature_per_county_per_day_20002022.csv"))
  vpdmax_data <- read.csv(file.path(TEMPERATURE_DATA_DIR, "vpdmax_per_county_per_day_20002022.csv"))

  vpdmin_data <- vpdmax_data

  reshape_data <- function(data) {
    data %>%
      gather(Date, Temperature, starts_with("X"), -GEOID, -county) %>%
      rename(county = "county")
  }

  max_temperature_data_long <- reshape_data(max_temperature_data)
  min_temperature_data_long <- reshape_data(min_temperature_data)
  mean_temperature_data_long <- reshape_data(mean_temperature_data)
  vpdmin_data_long <- vpdmin_data %>%
    gather(Date, MinVPDeficit, starts_with("X"), -GEOID, -county) %>%
    rename(county = "county")

  convert_date <- function(date) {
    as.Date(sub("X", "", date), format = "%Y.%m.%d")
  }

  max_temperature_data_long$Date <- convert_date(max_temperature_data_long$Date)
  min_temperature_data_long$Date <- convert_date(min_temperature_data_long$Date)
  mean_temperature_data_long$Date <- convert_date(mean_temperature_data_long$Date)
  vpdmin_data_long$Date <- convert_date(vpdmin_data_long$Date)

  max_temperature_data_long$year <- year(max_temperature_data_long$Date)
  min_temperature_data_long$year <- year(min_temperature_data_long$Date)
  mean_temperature_data_long$year <- year(mean_temperature_data_long$Date)
  vpdmin_data_long$year <- year(vpdmin_data_long$Date)

  renamed_county_pop <- county_pop %>%
    rename(GEOID = fips)

  join_with_county <- function(data) {
    data %>%
      left_join(renamed_county_pop, by = c("year", "county", "GEOID"))
  }

  max_temperature_data_long <- join_with_county(max_temperature_data_long)
  min_temperature_data_long <- join_with_county(min_temperature_data_long)
  mean_temperature_data_long <- join_with_county(mean_temperature_data_long)
  vpdmin_data_long <- join_with_county(vpdmin_data_long)



  max_temperature_data_long <- max_temperature_data_long %>%
    rename(MaxTemperature = Temperature)

  min_temperature_data_long <- min_temperature_data_long %>%
    rename(MinTemperature = Temperature)

  mean_temperature_data_long <- mean_temperature_data_long %>%
    rename(MeanTemperature = Temperature)


  max_temperature_data_long <- max_temperature_data_long %>%
    mutate(
      SatVapPres = ifelse(MaxTemperature > 0,
        exp(34.494 - (4924.99 / (MaxTemperature + 237.1))) / ((MaxTemperature + 105)^1.57),
        exp(43.494 - (6545.8 / (MaxTemperature + 278))) / ((MaxTemperature + 868)^2)
      )
    )



  combined_temp_data <- max_temperature_data_long
  combined_temp_data <- left_join(combined_temp_data, min_temperature_data_long %>% dplyr::select(county, Date, GEOID, MinTemperature), by = c("county", "Date", "GEOID"))
  combined_temp_data <- left_join(combined_temp_data, mean_temperature_data_long %>% dplyr::select(county, Date, GEOID, MeanTemperature), by = c("county", "Date", "GEOID"))
  combined_temp_data <- left_join(combined_temp_data, vpdmin_data_long %>% dplyr::select(county, Date, GEOID, MinVPDeficit), by = c("county", "Date", "GEOID"))


  combined_temp_data$MinVPDeficit <- combined_temp_data$MinVPDeficit * 100
  combined_temp_data$VaporPressure <- combined_temp_data$SatVapPres - combined_temp_data$MinVPDeficit
  combined_temp_data$RelHum <- ((combined_temp_data$VaporPressure) / combined_temp_data$SatVapPres) * 100

  combined_temp_data$MaxTemperatureF <- (combined_temp_data$MaxTemperature * (9 / 5)) + 32

  combined_temp_data$HeatIndex <- heat.index(
    t = combined_temp_data$MaxTemperatureF,
    rh = combined_temp_data$RelHum,
    temperature.metric = "fahrenheit"
  )

  season_vector <- character(nrow(combined_temp_data))

  # Assign seasons based on the criteria
  season_vector[months(combined_temp_data$Date) %in% c("May", "June", "July", "August", "September", "October")] <- "warm"
  season_vector[months(combined_temp_data$Date) %in% c("December", "January", "February", "March")] <- "cool"
  season_vector[months(combined_temp_data$Date) %in% c("April", "November")] <- "shoulder"

  # Add the season column to the dataframe
  combined_temp_data$season <- season_vector

  warm_season_start <- as.Date("05-01", format = "%m-%d")
  warm_season_end <- as.Date("10-14", format = "%m-%d")

  # Mutate block column
  combined_temp_data <- combined_temp_data %>%
    mutate(
      block = cut(
        Date,
        breaks = seq(warm_season_start, warm_season_end, by = "21 days"),
        labels = FALSE,
        right = FALSE
      )
    )

  combined_temp_data <- combined_temp_data %>%
    mutate(
      month_day = format(Date, "%m-%d"),
      block = case_when(
        month_day %in% c("12-01", "12-02", "12-03", "12-04", "12-05", "12-06", "12-07", "12-08", "12-09", "12-10", "12-11", "12-12", "12-13", "12-14", "12-15", "12-16", "12-17", "12-18", "12-19", "12-20", "12-21") ~ 1,
        month_day %in% c("12-22", "12-23", "12-24", "12-25", "12-26", "12-27", "12-28", "12-29", "12-30", "12-31", "01-01", "01-02", "01-03", "01-04", "01-05", "01-06", "01-07", "01-08", "01-09", "01-10", "01-11") ~ 2,
        month_day %in% c("01-12", "01-13", "01-14", "01-15", "01-16", "01-17", "01-18", "01-19", "01-20", "01-21", "01-22", "01-23", "01-24", "01-25", "01-26", "01-27", "01-28", "01-29", "01-30", "01-31", "02-01") ~ 3,
        month_day %in% c("02-02", "02-03", "02-04", "02-05", "02-06", "02-07", "02-08", "02-09", "02-10", "02-11", "02-12", "02-13", "02-14", "02-15", "02-16", "02-17", "02-18", "02-19", "02-20", "02-21") ~ 4,
        month_day %in% c("02-22", "02-23", "02-24", "02-25", "02-26", "02-27", "02-28", "02-29", "03-01", "03-02", "03-03", "03-04", "03-05", "03-06", "03-07", "03-08", "03-09", "03-10", "03-11", "03-12", "03-13", "03-14") ~ 5,
        TRUE ~ 6 # Default case, assign a block for any remaining dates
        # Add more cases if needed
      )
    )

  write.csv(combined_temp_data, file.path(TEMPERATURE_DATA_DIR, "combined_temp_data.csv"), row.names = FALSE)

  warm_data <- combined_temp_data %>%
    filter(season == "warm")

  cool_data <- combined_temp_data %>%
    filter(season == "cool")

  write.csv(warm_data, file.path(TEMPERATURE_DATA_DIR, "data_warm_season.csv"), row.names = FALSE)
  write.csv(cool_data, file.path(TEMPERATURE_DATA_DIR, "data_cool_season.csv"), row.names = FALSE)
}

write_cleaned_temp_data()
