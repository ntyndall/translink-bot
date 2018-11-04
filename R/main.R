#' @title Main
#' 
#' @export


main <- function(allInfo) {
  
  # Parse the input & get start and stop stations
  myStations <- allInfo$event$text %>% translink.bot::parse_message()
  startStation <- myStations$startStation
  stopStation <- myStations$stopStation
  
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
  
  # Get calling information
  allresults <- startCode %>% 
    translink.bot::query_live()
  
  # Need to make sure that the start and stop stations are actually close
  # to something at all...
  
  
  # Check the calling points, i.e. get ids in the right direction
  correctWay <- lapply(
    X = allresults$callingpoints,
    FUN = function(x) if (stopStation %in% (x %>% `[[`("Name") %>% as.character)) T else F 
  ) %>% 
    purrr::flatten_lgl()
  
  # Subset the right way details
  allresults$callingpoints %<>% `[`(correctWay)
  allresults$myresults %<>% subset(correctWay)
  
  # Need to do some slack stuff here (i.e. just post result!)
  mybody <- list(
    token = Sys.getenv("SLACK_TOKEN"), 
    text = allresults$myresults$time[1],
    channel = "CDV2M38KG"
  )
  
  # Simple post
  res <- httr::POST(
    url = "https://slack.com/api/chat.postMessage", 
    body = mybody
  )
}
