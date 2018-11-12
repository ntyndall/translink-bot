#' @title Station Lines
#' 
#' @export


station_lines <- function(stationlines) {
  
  # Define a colour set 
  colSet <- RColorBrewer::brewer.pal(
    n = 9,
    name = "Set1"
  )
  
  # Set up a temporary redis connection
  rcon <- redux::hiredis()
  
  # Map and flatten function
  mf <- function(x, i) x %>% purrr::map(i)%>% purrr::flatten_chr() 
  
  # Remove the current station list hash just for safety
  "stationcolors" %>% rcon$DEL()
  "stationlines" %>% rcon$DEL()

  # Define the line codes data frame
  linecodes <- stationlines$lines
  
  # Split up the codes
  splitends <- linecodes$code %>% 
    strsplit(split = "[:]")
  
  # Double length vector 
  newcodes <- linecodes$code %>% 
    c(splitends %>% mf(2) %>% paste0(":", splitends %>% mf(1)))
  
  # Also for line number
  mycolors <- colSet %>% 
    `[`(linecodes$line %>% rep(2))
  
  # Set the colours in a redis hash
  "stationcolors" %>% 
    rcon$HMSET(
      field = newcodes, 
      value = mycolors
    )

  # Now insert the station lines
  "stationlines" %>%
    rcon$HMSET(
      field = colSet %>% `[`(stationlines$order$line),
      value = stationlines$order$stations
    )

  # Remove the temporary connection
  rm(rcon)
}