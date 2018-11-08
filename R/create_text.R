#' @title Create Text
#' 
#' @export


create_text <- function(allresults, startStation, stopStation) {
  
  # Get ETA's
  etas <- allresults$callingpoints %>% 
    lapply(
      FUN = function(x) x %>% `[[`("etarr") %>% `[`(x %>% nrow) %>% as.character
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
  infoTxt <- paste0(" - Depart : *", incoming$time, "* // Arrive : *", etas, "*")
    
  # Chceck if any trains are delayed
  delayedTr <- incoming$Status %>% 
    `==`("On time") %>% 
    `!`()
  
  # Convert for printing
  incoming$Status <- paste0(" [ ", incoming$Status, " ]")
  if (delayedTr %>% any) {
    incoming$Status[delayedTr] <- paste0(" [ Delayed by ", incoming$Minutes[delayedTr], " minutes ]")
  }

  # Return slack message
  return(paste(c(headTxt, paste0(infoTxt, incoming$Status)), collapse = "\n"))
}
