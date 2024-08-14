source("(2) load_libraries.R")
source("(3) paths.R")

# To run this file:
# in terminal, run "Rscript '(10) dlnm.R'

load_libraries()

temp_metric_to_color <- list("min" = "blue", "heat_index" = "purple", "mean" = "green", "max" = "red")

get_temp_metric_col <- function(df, temp_metric) {
  if (temp_metric == "max") {
    return(df$MaxTemperature)
  } else if (temp_metric == "min") {
    return(df$MinTemperature)
  } else if (temp_metric == "heat_index") {
    return(df$HeatIndex)
  }
}

get_centering_temp <- function(df, temp_metric) {
  death_df <- df %>%
    filter(binary == 1)

  if (temp_metric == "max") {
    return(mean(death_df$MaxTemperature))
  } else if (temp_metric == "min") {
    return(mean(death_df$MinTemperature))
  } else if (temp_metric == "heat_index") {
    return(mean(death_df$HeatIndex))
  }
}

get_range <- function(df, temp_metric) {
  death_df <- df %>%
    filter(binary == 1)
  
  if (temp_metric == "max") {
    return(round(range(death_df$MaxTemperature), 0))
  } else if (temp_metric == "min") {
    return(round(range(death_df$MinTemperature), 0))
  } else if (temp_metric == "heat_index") {
    return(round(range(death_df$HeatIndex), 0))
  }

}

make_lag_col <- function(df, col_name, N, temp_metric) {
  if (temp_metric == "max") {
    return(
      df %>%
      group_by(county) %>%
      mutate(
        PrevDayIndex = match(Date - N, Date),  # Find index of previous day's date
        {{ col_name }} := ifelse(is.na(PrevDayIndex), NA, MaxTemperature[PrevDayIndex])  # Get previous day's temperature
      ) %>%
      select(-PrevDayIndex) %>%
      ungroup()
    )
  } else if (temp_metric == "min") {
    return(
      df %>%
        group_by(county) %>%
        mutate(
          PrevDayIndex = match(Date - N, Date),  # Find index of previous day's date
          {{ col_name }} := ifelse(is.na(PrevDayIndex), NA, MinTemperature[PrevDayIndex])  # Get previous day's temperature
        ) %>%
        select(-PrevDayIndex) %>%
        ungroup()
    )
  } else if (temp_metric == "heat_index") {
    return(
      df %>%
        group_by(county) %>%
        mutate(
          PrevDayIndex = match(Date - N, Date),  # Find index of previous day's date
          {{ col_name }} := ifelse(is.na(PrevDayIndex), NA, HeatIndex[PrevDayIndex])  # Get previous day's temperature
        ) %>%
        select(-PrevDayIndex) %>%
        ungroup()
    )
  }
  
  df %>%
    group_by(county) %>%
    mutate(
      PrevDayIndex = match(Date - N, Date),  # Find index of previous day's date
      {{ col_name }} := ifelse(is.na(PrevDayIndex), NA, MaxTemperature[PrevDayIndex])  # Get previous day's temperature
    ) %>%
    select(-PrevDayIndex) %>%
    ungroup()
}

# data: a dataframe containing the following columns:
#  - "Date"
#  - "county"
#  - "GEOID"
#  - "MinTemperature"
#  - "MaxTemperature"
#  - "MeanTemperature"
#  - "HeatIndex"
#  - "uniqueID"
#  - "case_control"
#  - "opioid_contributing"
#  - "cocaine_contributing"
#  - "other_stimi_contributing"
#  - "year"
#  - "total_population"
#  - "simple_race"
#  - "sex"
#  - "ethnicity"
#  - "simple_age"
#  - "block"
#  - "lag0"
#  - "lag1"
#  - "lag2"
#  - "lag3"
#  - "lag4"
#  - "lag5"
#  - "lag6"
#
# The rows in this dataframe represent either a death or control date.
dlnm <- function(data, temp_metric) {
  
  data <- data %>%
    mutate(Year = year(Date)) %>%
    filter(year(Date) >= 2000 & Year <= 2022) %>%
    dplyr::select(-Year)
  
  data$lag0 <- data$MaxTemperature
  data <- make_lag_col(data, lag1, 1, temp_metric)
  data <- make_lag_col(data, lag2, 2, temp_metric)
  data <- make_lag_col(data, lag3, 3, temp_metric)
  data <- make_lag_col(data, lag4, 4, temp_metric)
  data <- make_lag_col(data, lag5, 5, temp_metric)
  data <- make_lag_col(data, lag6, 6, temp_metric)
  
  # creating the exposure histories martrix
  Qdata <- data %>%
    filter(!is.na(uniqueID)) %>%
    dplyr::select(lag0:lag6)
  
  data <- data %>%
    filter(!is.na(uniqueID)) 
  
  varfun = "bs"
  varper <- c(.5)
  lag = 6
  lagnk <- 2
  
  temp_metric_col = get_temp_metric_col(data, temp_metric)
  
  argvar <- list(fun=varfun, knots=quantile(temp_metric_col, c(.5), na.rm = T))
  cb <- crossbasis(Qdata,lag=lag,argvar=argvar,
                   arglag=list(knots=logknots(lag,lagnk)))
  cbnest = cb
  
  # # regression model
  mnest <- clogit(binary ~ cbnest + strata(uniqueID), data, method = "exact")
  
  # # predicting specific effect summaries
  cenvalue = get_centering_temp(data, temp_metric)
  range = get_range(data, temp_metric)
  range = seq(range[1], range[2])
  pnest <- crosspred(cbnest, mnest, cen=cenvalue, at=range, cumul=TRUE)
  pnest
}

# matched_data_file is a dataframe containing temperature data for each death date (cases) and controls.
# This function concatenates the death data with temperature data for all dates to create
# a dataframe on which we can invoke dlnm().
dlnm_season <- function(matched_data_file, start_year, end_year, temp_metric) {
  matched_data_blocks <- read.csv(matched_data_file)
  matched_data_blocks <- matched_data_blocks %>%
    mutate(binary = ifelse(case_control == "case", 1, 0))

  combined_temp_data <- read.csv(file.path(TEMPERATURE_DATA_DIR, "combined_temp_data.csv"))
  combined_temp_data$Date <- as.Date(combined_temp_data$Date)

  matched_data_blocks_edited <- matched_data_blocks %>%
    filter(block %in% c(1, 2, 3, 4, 5, 6, 7))

  matched_data_blocks_edited <- matched_data_blocks_edited %>%
    filter(opioid_contributing == "yes")

  matched_data_blocks_edited$Date <- as.Date(matched_data_blocks_edited$Date)

  final_dataframe <- bind_rows(matched_data_blocks_edited, combined_temp_data %>%
    select(-SatVapPres, -MinVPDeficit, -VaporPressure, -RelHum, -MaxTemperatureF))


  final_dataframe <- final_dataframe %>%
    mutate(Year = year(Date)) %>%
    filter(Year >= start_year & Year <= end_year) %>%
    dplyr::select(-Year)

  dlnm(final_dataframe, temp_metric = temp_metric)
}


# Work in progress
plot_dlnm <- function(pnest, filename_prefix, temp_metric) {
  if (is.null(temp_metric_to_color[temp_metric])) {
    stop(paste0(temp_metric, " is not a valid temperature metric"))
  }

  temp_values <- data.frame(pnest$predvar)
  matfit_df <- data.frame(pnest$matRRfit)
  matlow_df <- data.frame(pnest$matRRlow)
  mathigh_df <- data.frame(pnest$matRRhigh)

  plot_color <- as.character(temp_metric_to_color[temp_metric])

  for (i in 0:6) {
    # Create plot data for the current column index
    plot_data <- data.frame(
      temp = temp_values,
      fit = matfit_df[, paste0("lag", i)], # Values from the current row of matRRfit
      low = matlow_df[, paste0("lag", i)], # Values from the current row of matRRlow
      high = mathigh_df[, paste0("lag", i)] # Values from the current row of matRRhigh
    )

    # Create the plot for the current column index
    ggplot(plot_data, aes(x = pnest.predvar)) +
      geom_line(aes(y = fit), color = plot_color) +
      geom_ribbon(aes(ymin = low, ymax = high), fill = plot_color, alpha = 0.3) +
      labs(x = NULL, y = NULL) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "black") + # Add horizontal line
      scale_x_continuous(breaks = seq(-20, 20, by = 5)) +
      coord_cartesian(ylim = c(0.5, 1.75)) + # Set y-axis limits
      theme_minimal()

    filename <- paste0(filename_prefix, "_", temp_metric, "_", i, ".png")
    ggsave(filename = file.path(PLOT_DIR, filename))
  }
}

temp_metric = "min"
warm_season_dlnm_overall <- dlnm_season(file.path(MISC_DATA_DIR, "cool_season_matched_data_blocks_cocaine.csv"), 2000, 2022, temp_metric = temp_metric)
plot_dlnm(warm_season_dlnm_overall, filename_prefix = "cocaine_overall", temp_metric = temp_metric)

