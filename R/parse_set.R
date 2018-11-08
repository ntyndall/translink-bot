#' @title Parse Set Favourites
#' 
#' @export


parse_set <- function(dbr, event) {
  
  # Get the actual message
  myMessage <- event$text
  
  # First should always be keyword!
  kword <- myMessage[1]
  
  # Make sure to statement exists
  if ("to" %in% myMessage) {
    
    # Get to index
    myInd <- "to" %>% 
      `==`(myMessage) %>%
      which
    
    # Parse out stations
    startSt <- myMessage[2:(myInd %>% `-`(1))]
    stopSt <- myMessage[(myInd %>% `+`(1)):(myMessage %>% length)]
    
    # Make sure keyword is allowed
    slackTxt <- if (kword %in% c("all", "info", "delete", "all")) {
      paste0("Cannot use *", kword, "* as a keyword")
    } else {
      paste0("Adding, *", kword, "* to list of routes")
    }

    # Update redis key
    paste0(event$team_id, ":", event$user_id, ":", kword) %>%
      dbr$SET(
        value = paste0(startSt %>% paste(collapse = "-"), ":", stopSt %>% paste(collapse = "-"))
      )
  } else {
    slackTxt <- "Could not find `to` tag..."
  }
  
  # Return text for slack
  return(slackTxt)
}