#' @title Parse Get Favourites
#' 
#' @export


parse_get <- function(dbr, event) {
  
  # Get the actual message
  myMessage <- event$text[1]

  # Look up ALL favourites
  allFavs <- paste0(event$team_id, ":", event$user_id, "*") %>% 
    dbr$KEYS()
  
  # Flatten list of favourites
  if (allFavs %>% length %>% `>`(0)) {
    allFavs %<>% purrr::flatten_chr()
    
    # Get all keywords
    allkwords <- allFavs %>% 
      strsplit(split = ":") %>% 
      purrr::map(3) %>% 
      purrr::flatten_chr()
    
    # Now get the info
    if (myMessage %in% allkwords) {
      myroute <- allFavs[myMessage == allkwords] %>% dbr$GET()
      
      myroute %<>% strsplit(split = "[:]") %>% purrr::flatten_chr()
      myroute <- c(
        myroute[1] %>% strsplit(split = "[-]") %>% purrr::flatten_chr(),
        "to",
        myroute[2] %>% strsplit(split = "[-]") %>% purrr::flatten_chr()
      )

      # Query train timetable
      slackTxt <- dbr %>%
        translink.bot::parse_trains(
          event = list(text = myroute)
        )
    } else {
      slackTxt <- paste0("This route is not stored yet!")
    }
  } else {
    slackTxt <- "You have no routes saved!"
  }

  # Return text for slack
  return(slackTxt)
}