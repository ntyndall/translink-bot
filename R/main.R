


main <- function(startStation, stopStation) {
  
  # Load up station list
  station.list <- translink.bot::get_stations()
  
  # Lower case everything
  startStation %<>% Hmisc::upFirst()
  stopStation %<>% Hmisc::upFirst()
  
  # Get the start code
  startCode <- station.list$code %>% 
    `[`(startStation %>% 
          stringdist::stringdist(station.list$name, method = 'jw') %>%
          which.min
       )
  
  # Need to make sure that the start and stop stations are actually close
  # to something at all...
  
  
  # Check the calling points, i.e. get ids in the right direction
  correctWay <- lapply(
    X = callingpoints,
    FUN = function(x) if (stopStation %in% (x %>% `[[`("Name") %>% as.character)) T else F 
  ) %>% 
    purrr::flatten_lgl()
  
  # Subset the right way details
  callingpoints %<>% `[`(correctWay)
  myresults %<>% subset(correctWay)
  
  # Need to do some slack stuff here

}
