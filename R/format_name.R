#' @title Format Station Name
#' 
#' @export

format_name <- function(x) x %>% tolower %>% Hmisc::upFirst() %>% paste(collapse = " ")