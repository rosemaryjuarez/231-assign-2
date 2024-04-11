# Set your working directory to your home path first
setwd("/Users/shane")

# Now you can read the table with a relative path from your home directory
clim_data <- read.table("Downloads/clim.txt", header = TRUE)

# Display the first few rows of the dataframe
head(clim_data)

calculate_climate_statistics <- function(climate_data) {
  # Check if the required columns exist in the dataframe
  required_columns <- c('year', 'month', 'tmin_c', 'tmax_c', 'precip')
  if (!all(required_columns %in% names(climate_data))) {
    stop("The dataframe does not have the required columns: ", paste(required_columns, collapse=", "), ".")
  }
  
  # Calculate the minimum and maximum temperature, and total precipitation
  # for each month of each year
  stats <- list(
    MinTemp = aggregate(tmin_c ~ year + month, data = climate_data, FUN = min, na.rm = TRUE),
    MaxTemp = aggregate(tmax_c ~ year + month, data = climate_data, FUN = max, na.rm = TRUE),
    TotalPrecip = aggregate(precip ~ year + month, data = climate_data, FUN = sum, na.rm = TRUE)
  )
  
  # Merge the individual statistics dataframes into one
  climate_stats_df <- Reduce(function(...) merge(..., by = c("year", "month"), all = TRUE), stats)
  
  # Sort the data frame by year and month
  climate_stats_df <- climate_stats_df[order(climate_stats_df$year, climate_stats_df$month), ]
  
  return(climate_stats_df)
}

# Usage:
# Assuming 'clim_data' is your loaded dataframe with the appropriate columns
climate_stats_df <- calculate_climate_statistics(clim_data)
print(climate_stats_df)

calculate_almond_yield_anomaly <- function(climate_data) {
  # Ensure the dataframe has the necessary columns
  required_columns <- c('year', 'month', 'tmin_c', 'precip')
  missing_columns <- setdiff(required_columns, names(climate_data))
  if (length(missing_columns) > 0) {
    stop("The dataframe does not have the required columns: ", paste(missing_columns, collapse=", "), ".")
  }
  
  # Calculate the min temp for February and total precip for January for each year
  feb_min_temp <- aggregate(tmin_c ~ year, data = climate_data[climate_data$month == 2,], FUN = min)
  jan_precip <- aggregate(precip ~ year, data = climate_data[climate_data$month == 1,], FUN = sum)
  
  # Merge the February minimum temperature and January precipitation data
  yield_data <- merge(feb_min_temp, jan_precip, by = "year")
  
  # Calculate the almond yield anomaly for each year using the formula
  yield_data$almond_yield_anomaly <- with(yield_data, -0.0157 * tmin_c - 0.00467 * tmin_c^2 - 
                                            0.07 * precip + 0.0043 * precip^2 + 0.28)
  
  # Select the year and the calculated almond yield anomaly
  yield_anomaly_by_year <- yield_data[, c('year', 'almond_yield_anomaly')]
  
  return(yield_anomaly_by_year)
}

# Assuming 'clim_data' is your loaded dataframe with the appropriate columns
# Call the function and print the result
clim_data <- read.csv("/Downloads/clim.txt", header = TRUE) # Replace with your actual file path
almond_yield_anomaly_df <- calculate_almond_yield_anomaly(clim_data)
print(almond_yield_anomaly_df)

# Assuming almond_yield_anomaly_df is your dataframe with the almond yield anomalies
mean_almond_yield_anomaly <- mean(almond_yield_anomaly_df$almond_yield_anomaly, na.rm = TRUE)

# Print the mean almond yield anomaly
print(mean_almond_yield_anomaly)
