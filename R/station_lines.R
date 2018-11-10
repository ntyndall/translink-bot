#' @title Station Lines
#' 
#' @export


station_lines <- function(stationlines) {
  # Set up a temporary redis connection
  rcon <- redux::hiredis()
  
  # Map and flatten function
  mf <- function(x, i) x %>% purrr::map(i)%>% purrr::flatten_chr() 
  
  # Remove the current station list hash just for safety
  "stationlist" %>% rcon$DEL()
  
  # Split up the codes
  splitends <- stationlines$code %>% 
    strsplit(split = "[:]")
  
  # Double length vector 
  newcodes <- stationlines$code %>% 
    c(splitends %>% mf(2) %>% paste0(":", splitends %>% mf(1)))
  
  # Also for line number
  newlines <- stationlines$line %>% 
    rep(2)
  
  # Subset some colors (Shouldn't be more than 12 lines...)
  mycolors <- RColorBrewer::brewer.pal(n = 12, name = "Set3") %>% 
    `[`(newlines)
  
  # Set the colours in a redis hash
  "stationlist" %>% 
    rcon$HMSET(
      field = newcodes, 
      value = mycolors
    )
  
  # Remove the temporary connection
  rm(rcon)
}