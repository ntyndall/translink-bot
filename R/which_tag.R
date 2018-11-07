#' @title Which Tag
#' 
#' @export


which_tag <- function(mess, tag) mess[tag %>% `==`(mess) %>% which %>% `+`(1)]