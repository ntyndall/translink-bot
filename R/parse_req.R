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

  # Log to the API
  cat(
    bodyTxt$token, ":",
    bodyTxt$team_domain, "~", bodyTxt$team_id, "|",
    bodyTxt$channel_name, "~", bodyTxt$channel_id, "|",
    bodyTxt$user_name, "~", bodyTxt$user_id, "\n",
    bodyTxt$text, "\n"
  )

  # Update the incoming message
  bodyTxt$text %<>%
    strsplit(split = "[+]") %>% 
    purrr::flatten_chr() %>%
    tolower
  
  # Return updated body back
  return(bodyTxt)
}
