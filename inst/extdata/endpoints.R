#* Log some information about the incoming request
#*
#* @filter logger


function(req) {
  
  # Create redis connection for query
  req$dbr <- redux::hiredis()
  
  # Log the request
  cat(
    as.character(Sys.time()), "-",
    req$REQUEST_METHOD, req$PATH_INFO, "-",
    req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n"
  )
  
  # Forward on
  plumber::forward()
}

#* @get /


function() return("/")

#* @post /mybot


function(req, res) {  
  
  res$body <- req$postBody
  
  # Store the post body
  myinfo <- jsonlite::fromJSON(req$postBody)
  
  # Do I need to verify?
  if ("challenge" %in% names(myinfo)) {
    res$body <- myinfo$challenge
  } else {
    # Log the next 2 lines on the slack info
    cat(
      myinfo$event_time, "-",
      myinfo$token, "/", myinfo$team_id, "/", 
      myinfo$event$user, "/", myinfo$event$channel, "\n",
      myinfo$event$text, "\n"
    )
    
    # Call to main to handle the request
    # req %>% translink.bot::main(logger = F)
  }
  
  # Return the response back
  res
}

#* @post /tinfo

function(req, res) {
  # Return the response back
  res$body <- req$dbr %>% 
    translink.bot::parse_info(
      event = req$postBody %>% translink.bot::parse_req()
    )
  
  res
}

#* @post /ttrains
#*
#* @serializer html

function(req, res) {
  # Return the response back
  res$body <- req$dbr %>% 
    translink.bot::parse_trains(
      event = req$postBody %>% translink.bot::parse_req()
    )
  
  # Update headers
  res$headers <- list("Content-Type" = "application/json")
  
  res
}

#* @post /tset

function(req, res) {
  # Return the response back
  res$body <- req$dbr %>%
    translink.bot::parse_set(
      event = req$postBody %>% translink.bot::parse_req()
    )
  
  res
}

#* @post /tget
#*
#* @serializer html

function(req, res) {
  # Return the response back
  mytext <- req$dbr %>%
    translink.bot::parse_get(
      event = req$postBody %>% translink.bot::parse_req()
    )
  
  res$body <- mytext
  res$headers <- list("Content-Type" = "application/json")
  res
}

#* @post /tdelete

function(req, res) {
  # Return the response back
  res$body <- req$dbr %>%
    translink.bot::parse_delete(
      event = req$postBody %>% translink.bot::parse_req()
    )
  
  res
}

#* @post /tlines
#*
#* @serializer html

function(req, res) {
  # Return the response back
  res$body <- req$dbr %>%
    translink.bot::parse_lines()
  
  res$headers <- list("Content-Type" = "application/json")
  res
}


#* @get /authorize
#*
#* @serializer html

function(req, res) {
  # Get OAUTH access
  myres <- httr::POST(
    url = "https://slack.com/api/oauth.access", 
    body = list(
      client_id = Sys.getenv("SLACK_ID"), 
      client_secret = Sys.getenv("SLACK_SECRET") , 
      code = req$QUERY_STRING %>% 
        httr::parse_url() %>% 
        `[[`("query") %>%
        `[[`("code")
    )
  )
  
  # Make sure the request was authorized
  actionTaken <- myres$content %>%
    rawToChar() %>% 
    jsonlite::fromJSON()
  
  # html message
  htmlMesssage <- if (actionTaken$ok) {
    "<h1>Success!</h1>"
  } else {
    paste0("<h1>Authorization Failed!<h1><h2>- ", actionTaken$error, "</h2>")
  }
  
  # Return response back  
  paste0("<html><body>", htmlMessage, "</body></html>")
}
