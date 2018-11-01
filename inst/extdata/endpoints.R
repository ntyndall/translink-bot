#* Log some information about the incoming request
#*


function(req) {
  
  # Log the request
  cat(
    as.character(Sys.time()), "-",
    req$REQUEST_METHOD, req$PATH_INFO, "-",
    req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n"
  )
  
  # Forward on
  plumber::forward()
}


#* @get /test


print_hello <- function(var = "hello") {
  return(var)
}

#* @get /


function() {
  return("this is root /")
}

#* @post /mypost

function() {
  return(1 + 1)
}
