#' @title Parse Request
#'
#' @export


parse_req <- function(bodyTxt) {
  
  # Prepend for httr usage
  bodyTxt <- paste0("?", bodyTxt)
  
  # Parse the query
  bodyTxt %<>% 
    httr::parse_url() %>%
    `[[`("query")

  # Update the incoming message
  bodyTxt$text %<>%
    strsplit(split = "[+]") %>% 
    purrr::flatten_chr() %>%
    tolower
  
  # Return updated body back
  return(bodyTxt)
}
