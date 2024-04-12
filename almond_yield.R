
#' Calculating almond yield by year
#'
#' @param data 
#'
#' @return yield_anomaly_by_year - this gives us the calculation needed to calculate 
#'        the yearly almost yield using variables such as precipitation and temperature
#'        to create a statistical model.
#'        we use this equation as our model: 
#'        Y = -0.0157T~n,2~ - 0.0046(T~n,2~)\^2 - 0.07P~1~ + 0.0043(P~1~)\^2 + 0.28
#'        where:
#'        Y = yield (tons of almonds per acre)
#'        T = temperatures,where subscript numbers denote month of climate variable(Celsius)
#'        P = average daily precipitation (mm)
#' 
#' @examples calculate_almond_yield(clim_data)
#' 
calculate_almond_yield <- function(data) {
  # Ensure the dataframe has the necessary columns
  required_columns <- c('year', 'month', 'tmin_c', 'precip')
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0) {
    stop("The dataframe does not have the required columns: ", paste(missing_columns, collapse=", "), ".")
  }
  
  # Calculate the min temp for February and total precip for January for each year
  feb_min_temp <- aggregate(tmin_c ~ year, data = data[data$month == 2,], FUN = min)
  jan_precip <- aggregate(precip ~ year, data = data[data$month == 1,], FUN = sum)
  
  # Merge the February minimum temperature and January precipitation data
  yield_data <- merge(feb_min_temp, jan_precip, by = "year")
  
  # Calculate the almond yield anomaly for each year using the formula
  yield_data$almond_yield_anomaly <- with(yield_data, -0.0157 * tmin_c - 0.00467 * tmin_c^2 - 
                                            0.07 * precip + 0.0043 * precip^2 + 0.28)
  
  # Select the year and the calculated almond yield anomaly
  yield_anomaly_by_year <- yield_data[, c('year', 'almond_yield_anomaly')]
  
  return(yield_anomaly_by_year)
}
