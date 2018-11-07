#' @title Accumulator
#'
#' @export


StationInfo <- R6::R6Class(
  classname = "StationInfo", 
  public = list(
    startSt = NULL,
    stopSt = NULL,
    updateAction = NULL
  )
)
