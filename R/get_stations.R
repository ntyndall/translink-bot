#' @title Get Stations
#' 
#' @export


get_stations <- function() {
  return(
    system.file("extdata", "stationlist.json", package = "translink.bot") %>% 
      jsonlite::fromJSON() %>%
      `[[`("stations")
  )
}
