#' @title Parse Message
#' 
#' @export


parse_message <- function(event, dbr) {
  
  # Get the actual message
  myMessage <- event$text
  
  # Split by space
  myMessage %<>% 
    strsplit(split = " ") %>%
    purrr::flatten_chr() %>%
    tolower
  
  # Find `set` tag
  setTag <- myMessage %>% 
    grepl(pattern = "set", fixed = TRUE)
  
  # Check existence
  if (setTag %>% any) {
    tagKey <- myMessage %>% `[`(setTag %>% which + 1)
    tagKey <- paste0(event$team, ":", event$user, ":", tagKey)
  }
  
  # Parse out mention of userID <@...>
  # myMessage %>% gsub(pattern = "<@\\w+> ", replacement = "")
  
  # Find `to` tag or else look up favourites
  toTag <- myMessage %>% 
    grepl(pattern = "to", fixed = TRUE)
  
  if (toTag %>% any) {
    startSt <- myMessage[toTag %>% which %>% `-`(1)]
    stopSt <- myMessage[toTag %>% which %>% `+`(1)]
    
    if (setTag %>% any) {
      # Update key in redis
      dbr$SET(
        key = tagKey,
        value = paste0(startSt, ":", stopSt)
      )
    } 
  } else {
    # Look up favourites
    allFavs <- paste0(event$team, ":", event$user, "*") %>% 
      dbr$KEYS()
    
    if (allFavs %>% length %>% `>`(0)) {
      allFavs %<>% purrr::flatten_chr()
      
      # Favourite names
      favNames <- allFavs %>% strsplit(":") %>% purrr::flatten_chr()
      
      keyWord <- myMessage[2]
      
      splitUp <- keyWord %>% 
        strsplit("") %>%
        purrr::flatten_chr()
      
      # Check to see if message is inound or outbound 
      lastChr <- splitUp %>% 
        utils::tail(1)
      
      if (lastChr %>% `==`("'")) {
        keyWord <- splitUp %>% 
          utils::head(-1) %>% 
          paste(collapse = "")
        swapUp <- TRUE
      } else {
        swapUp <- FALSE
      }
      
      if (keyWord %in% favNames) {
        specialRoute <- dbr$GET(
          key = allFavs[keyWord %>% `==`(favNames) %>% which]
        )
        
        specialRoute %<>% strsplit(split = ":") %>% purrr::flatten_chr()
        
        if (swapUp) specialRoute %<>% rev
        startSt <- specialRoute[1]
        stopST <- specialRoute[2]
      }
    }
  }
  
  # Return station list back
  return(
    list(
      startStation = startSt,
      stopStation = stopSt
    )
  )
}
