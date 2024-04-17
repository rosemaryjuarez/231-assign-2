#' Calculating almond yield and profit by year
#'
#' @param data 
#' @param almond_price Average price of almonds per ton ($)
#' @param cost_per_acre Total cost per acre ($)
#' @param acres Total number of acres
#'
#' @return yield_and_profit_by_year - a dataframe containing the year, almond yield, and profit
#' 
#' @examples calculate_almond_yield_and_profit(clim_data, almond_price = 5000, cost_per_acre = 3000, acres = 100)
#' 
almond_yield_and_profit <- function(data, almond_price, cost_per_acre, acres) {
  
  # Calculate the almond yield anomaly for each year using the existing model
  yield_anomaly_by_year <- calculate_almond_yield(data)
  
  # Calculate the almond yield in tons per acre
  yield_anomaly_by_year$almond_yield_tons_per_acre <- exp(yield_anomaly_by_year$almond_yield_anomaly)
  
  # Calculate the revenue from almond sales
  yield_anomaly_by_year$revenue <- yield_anomaly_by_year$almond_yield_tons_per_acre * almond_price * acres
  
  # Calculate the total cost
  yield_anomaly_by_year$total_cost <- cost_per_acre * acres
  
  # Calculate the profit
  yield_anomaly_by_year$profit <- yield_anomaly_by_year$revenue - yield_anomaly_by_year$total_cost
  
  return(yield_anomaly_by_year)
}
