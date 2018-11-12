#' @title Parse Lines
#' 
#' @export


parse_lines <- function(dbr) {
  
  # Get the actual message
  myMessage <- event$text
  
  # Parse the lines
  results <- "stationlines" %>% 
    dbr$HGETALL()
  
  results %>% `[`(c(T, F)) %>% purrr::flatten_chr()
  mylines <- results %>% `[`(c(F, T)) %>% purrr::flatten_chr()

  # Return the lines back
  return(
    list(
      attachments = data.frame(
        pretext = "Listing available train lines + colours;",
        fallback = paste0(" - ",  mylines),
        text = mylines,
        color = results %>% `[`(c(T, F)) %>% purrr::flatten_chr()
      )
    ) %>% 
      jsonlite::toJSON()
  )
}