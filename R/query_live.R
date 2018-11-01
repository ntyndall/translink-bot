#' @title Query Live Data
#' 
#' @export


query_live <- function(stationCode) {
  details <- paste0("https://apis.opendatani.gov.uk/translink/", stationCode, ".xml") %>%
    httr::GET() %>%
    `[[`("content") %>%
    rawToChar()
  
  # Convert to xml and get all services first
  services <- details %>%
    xml2::read_xml() %>%
    xml2::xml_find_all(".//Service")
  
  # Get calling points as a list of data frames
  callingpoints <- lapply(
    X = services, 
    FUN = function(x) x %>% xml2::xml_find_all(".//CallingPoint") %>% xml2::as_list() %>% lapply(attributes)
  ) %>% lapply(
    FUN = function(x) {
      if (x %>% length %>% `!=`(0)) {
        x %>% lapply(as.data.frame) %>% purrr::reduce(rbind) 
      } else {
        NULL
      }
    }
  )
  
  # Get the remainder of tags
  rest <- lapply(
    X = ".//" %>% paste0(translink.bot::allowed_tags()),
    FUN = function(y) services %>% xml2::xml_find_first(y) %>% xml2::as_list() %>% lapply(attributes)
  )
  
  # Now convert to a data frame
  myresults <- lapply(
    X = rest,
    FUN = function(x) x %>% lapply(as.data.frame) %>% purrr::reduce(rbind)
  )
  
  # Manually update these for now
  names(myresults[[2]]) <- c("arrivetime", "arrived", "arrivedtstamp")
  names(myresults[[7]]) <- c("originname", "origintiploc", "origincrs")
  
  # Create data frame
  myresults %<>% purrr::reduce(cbind)
  
  return(
    list(
      myresults = myresults,
      callingpoints = callingpoints
    )
  )
}