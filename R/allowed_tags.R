#' @title Allowed XML tags
#' 
#' @export


allowed_tags <- function() {
  return(
    c(
      "ServiceType",
      "ArriveTime",
      "DepartTime",
      "Platform",
      "ServiceStatus",
      "Delay",
      "Origin1",
      "Destination1"
    )
  )
}
