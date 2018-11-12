#' @title Set up
#'
#' @export


api <- function() {

  # Source the API
  pl <- plumber::plumb(system.file("extdata", "endpoints.R", package = "translink.bot"))

  # Load API details
  details <- system.file("extdata", "params.yaml", package = "translink.bot") %>%
    yaml::yaml.load_file()

  # Load station list
  system.file("extdata", "trainlines.json", package = "translink.bot") %>% 
    jsonlite::fromJSON() %>% 
    translink.bot::station_lines()  
  
  # Run the API
  pl$run(
    port = details$port, 
    host = details$host
  )
}
