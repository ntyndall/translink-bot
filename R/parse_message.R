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
    favouriteName <- myMessage %>% `[`(setTag %>% which + 1)
    if (favouriteName %in% c("all", "info", "delete")) stop("just exit from here")
    tagKey <- paste0(event$team, ":", event$user, ":", favouriteName)
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
    
    # Create function for matching the input
    which_tag <- function(mess, tag) mess[tag %>% `==`(mess) %>% which %>% `+`(1)]
    
    # Look up ALL favourites
    allFavs <- paste0(event$team, ":", event$user, "*") %>% 
      dbr$KEYS()
    
    # Check for an action
    if ("delete" %in% myMessage) {
      redpipe <- redux::redis
      
      kword <- which_tag(myMessage, "delete")
      if (kword == "all") {
        results <- dbr$pipeline(
          .commands = lapply(
            X = allFavs,
            FUN = function(x) x %>% redpipe$DEL()
          )
        )
      } else {
        results <- dbr$DEL()
      }
      additionalMsg <- "Deleting info"
      
    } else if ("info" %in% myMessage) {
      kword <- which_tag(myMessage, "info")
      if (kword == "all") {
        results <- dbr$pipeline(
          .commands = lapply(
            X = allFavs,
            FUN = function(x) x %>% redpipe$GET()
          )
        )
      } else {
        results <-  dbr$GET() 
      }
      
      additionalMsg <- "Getting info"
    } else {
      # Check for user-created tags
      if (allFavs %>% length %>% `>`(0)) {
        allFavs %<>% purrr::flatten_chr()
        
        # Favourite names
        favNames <- allFavs %>% 
          strsplit(":") %>%
          purrr::map(3) %>% 
          purrr::flatten_chr()
        
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
          stopSt <- specialRoute[2]
        }
        additionalMsg <- "Setting favourite info"
      }
      additionalMsg <- NULL
    }
  }
  
  # Return station list back
  return(
    list(
      startStation = startSt,
      stopStation = stopSt,
      additionalMsg = additionalMsg
    )
  )
}
