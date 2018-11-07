#' @title Parse Info
#'
#' @export


parse_info <- function(dbr, allFavs, myMessage) {

  # Set up pipeline
  redpipe <- redux::redis
  
  # Get all keywords
  allkwords <- allFavs %>% 
    strsplit(split = ":") %>% 
    purrr::map(3) %>% 
    purrr::flatten_chr()
  
  kword <- translink.bot::which_tag(myMessage, "info")
  
  if (kword == "all") {
    results <- dbr$pipeline(
      .commands = lapply(
        X = allFavs,
        FUN = function(x) x %>% redpipe$GET()
      )
    ) %>%
      purrr::flatten_chr()
    
    favnames <- allkwords
  } else if (kword %in% allkwords) {
    # Which one?
    results <- allFavs[kword %>% `==`(allkwords) %>% which] %>% dbr$GET() 
    favnames <- kword
  } else {
    return(
      list(
        startSt = NULL,
        stopSt = NULL,
        updateAction = paste0("No favourites matching : ", kword)
      )
    )
  }
  
  forslack <- results %>% 
    strsplit(split = ":") %>% 
    purrr::map(function(x) x %>% paste(collapse = " to ")) %>% 
    purrr::flatten_chr()
  
  myres <- paste(paste0(favnames, " : ", forslack), collapse = " \n")
  
  return(
    list(
      startSt = NULL,
      stopSt = NULL,
      updateAction = paste0("Favourite information ... \n", myres)
    )
  )
}
