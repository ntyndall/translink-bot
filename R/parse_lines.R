#' @title Parse Lines
#' 
#' @export


parse_lines <- function(dbr) {

  # Parse the lines
  results <- "stationlines" %>% 
    dbr$HGETALL()

  # Get actual lines as a character vector
  mylines <- results %>% 
    `[`(c(F, T)) %>%
    purrr::flatten_chr()

  # Return the lines back
  return(
    list(
      attachments = data.frame(
        pretext = c("Listing available train lines + colours;", NA %>% rep(mylines %>% length)),
        fallback = c(NA, paste0(" > ",  mylines)),
        text = c(NA, mylines),
        color = c(NA, results %>% `[`(c(T, F)) %>% purrr::flatten_chr())
      )
    ) %>% 
      jsonlite::toJSON()
  )
}