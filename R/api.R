#' @title Set up
#'
#' @export


api <- function() {

  # Source the API
  pl <- plumber::plumb(system.file("extdata", "endpoints.R", package = "translink.bot"))

  # Load API details
  details <- system.file("extdata", "params.yaml", package = "translink.bot") %>%
    yaml::yaml.load_file()

  # Run the API
  pl$run(
    port = details$port, 
    host = details$host
  )
}
