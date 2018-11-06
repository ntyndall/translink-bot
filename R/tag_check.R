#' @title Tag Check
#' 
#' @export


tag_check <- function(allFavs) {
  
  # Favourite names
  favNames <- allFavs %>% 
    strsplit(":") %>%
    purrr::map(3) %>% 
    purrr::flatten_chr()
  
  return(favNames)
}