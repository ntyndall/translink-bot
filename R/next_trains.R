#' @title Next Trains
#' 
#' @export


next_trains <- function(startStation, stopStation) {
  
  # Get station information from starting point
  station.info <- startStation %>% 
    translink.bot::query_live()
  
  # Make sure stopping station exists in the information
  
  
  
}