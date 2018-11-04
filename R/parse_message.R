#' @title Parse Message
#' 
#' @export


parse_message <- function(myMessage) {
  
  # Split by space
  myMessage %<>% 
    strsplit(split = " ") %>%
    purrr::flatten_chr() %>%
    tolower
  
  # Parse out mention of userID <@...>
  # myMessage %>% gsub(pattern = "<@\\w+> ", replacement = "")
  
  # Find `to` tag
  myind <- myMessage %>% 
    grepl(pattern = "to", fixed = TRUE) %>% 
    which
  
  return(
    list(
      startStation = myMessage[myind - 1],
      stopStation = myMessage[myind + 1]
    )
  )
}
