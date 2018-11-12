#' @title Convert Time
#'
#' @export


conv_time <- function(z) {

  results <- z %>% strsplit(split = "") %>%
    purrr::map(function(x) paste0(paste(x[1:2], collapse = ""), ":", paste(x[3:4], collapse = ""))) %>%
    purrr::flatten_chr()
  
  if ("" %in% z) results["" %>% `==`(z) %>% which] <- "Unknown"

  return(results)
}


#' @title Split By Space
#' 
#' @export

split_space <- function(x) x %>% strsplit(split = " ") %>% purrr::flatten_chr()


#' @title Format Station Name
#' 
#' @export

format_name <- function(x) x %>% tolower %>% Hmisc::upFirst() %>% paste(collapse = " ")
