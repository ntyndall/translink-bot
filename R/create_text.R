#' @title Create Text
#' 
#' @export


create_text <- function(allresults, dbr, startStation, stopStation) {
  
  # Get ETA's
  etas <- allresults$callingpoints %>% 
    lapply(
      FUN = function(x) {
        x %>% 
          `[[`("etarr") %>%
          `[`(x %>% `[[`("Name") %>% as.character %>% `==`(stopStation) %>% which) %>% 
          as.character
      }
    ) %>% 
    purrr::flatten_chr()
  
  incoming <- allresults$myresults
  incoming %<>% lapply(as.character)
  
  # Convert various timestamps to hh:mm
  etas %<>% translink.bot::conv_time()
  incoming$time %<>% translink.bot::conv_time()
   
  # Header text
  headTxt <- paste0("Trains from *", startStation, "* to * ", stopStation, "* (", incoming$originname[1], "/", incoming$name[1], " line)")
  
  # Start to accumulate information
  infoTxt <- paste0("Depart : *", incoming$time, "* // Arrive : *", etas, "*")
  
  # Chceck if any trains are delayed
  delayedTr <- incoming$Status %>% 
    `==`("On time") %>% 
    `!`()
  
  # Convert for printing
  incoming$Status <- paste0(" [ ", incoming$Status, " ]")
  if (delayedTr %>% any) {
    incoming$Status[delayedTr] <- paste0(" [ Delayed by ", incoming$Minutes[delayedTr], " minutes ]")
  }
  
  # Get origin destination combo
  originDest <- incoming$origintiploc %>% 
    paste0(":", incoming$tiploc)
  
  lineColors <- c()
  for (i in 1:(infoTxt %>% length)) {
    res <- "stationcolors" %>% 
      dbr$HMGET(field = originDest[i]) %>% 
      `[[`(1)
    lineColors %<>% c(if (res %>% is.null) "#000000" else res)
  }
  
  # Prepare the data structure
  actualTimes <- paste0(infoTxt, incoming$Status)

  # Data frame for slack message
  return(
    data.frame(
      pretext = c(headTxt, NA %>% rep(actualTimes %>% length)),
      fallback = c(headTxt, paste0(" - ", actualTimes)),
      text = c(NA, actualTimes),
      color = c(NA, lineColors)
    )
  )
}
