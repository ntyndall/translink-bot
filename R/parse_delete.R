#' @title Parse Info
#'
#' @export


parse_delete <- function(dbr, allFavs, myMessage) {
  
  # Set up pipeline
  redpipe <- redux::redis
  
  # Get all keywords
  allkwords <- allFavs %>% 
    strsplit(split = ":") %>% 
    purrr::map(3) %>% 
    purrr::flatten_chr()
  
  kword <- translink.bot::which_tag(myMessage, "delete")
  
  if (kword == "all") {
    results <- dbr$pipeline(
      .commands = lapply(
        X = allFavs,
        FUN = function(x) x %>% redpipe$DEL()
      )
    )
    favnames <- allkwords
  } else if (kword %in% allkwords) {
    # Which one?
    results <- allFavs[kword %>% `==`(allkwords) %>% which] %>% dbr$DEL() 
    favnames <- kword
  } else {
    return(
      list(
        startSt = NULL,
        stopSt = NULL,
        updateAction = paste0("Nothing to delete for : ", kword)
      )
    )
  }
  
  return(
    list(
      startSt = NULL,
      stopSt = NULL,
      updateAction = paste0("Deleting information ... \n- ", favnames %>% paste(collapse = "\n- "))
    )
  )
}
