#' @title Create Text
#' 
#' @export


create_text <- function(incoming, startStation, stopStation) {
  
  #incoming <- allresults$myresults
  incoming %<>% lapply(as.character)
  
  # Convert to time hh:mm
  incoming$time %<>% 
    strsplit(split = "") %>%
    purrr::map(function(x) paste0(paste(x[1:2], collapse = ""), ":", paste(x[3:4], collapse = ""))) %>%
    purrr::flatten_chr()
   
  # Header text
  headTxt <- paste0("Trains from *", startStation, "* to * ", stopStation, "* (", incoming$originname[1], "/", incoming$name[1], " line)")
  
  # Start to accumulate information
  infoTxt <- paste0(" - Next train at : ", incoming$time)
    
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
