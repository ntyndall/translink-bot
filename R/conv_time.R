#' @title Convert Time
#'
#' @export


conv_time <- function(z) {
  return(
    z %>% strsplit(split = "") %>%
      purrr::map(function(x) paste0(paste(x[1:2], collapse = ""), ":", paste(x[3:4], collapse = ""))) %>%
      purrr::flatten_chr()
  )
}