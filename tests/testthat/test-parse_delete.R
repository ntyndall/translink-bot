context("test-parse_get.R")

# Set up default team and user info
event <- list(
  team_id = "teamA",
  user_id = "userA"
)

# Set up REDIS connection
dbr <- redux::hiredis()

temp_fun <- function(i, dbr, event) {
  texts <- c("test a to b", "another c to d")

  event$text <- texts[i] %>% translink.bot::split_space()
  results <- dbr %>% 
    translink.bot::parse_set(
      event = event
    )
}

# Begin tests
test_that("Check `delete` statement (/tdelete).", {
  
  # Add text, we need to set some key first
  temp_fun(
    i = 1,
    dbr = dbr, 
    event = event
  )
  
  # Make sure key was created
  expect_equal( dbr$KEYS("*") %>% length, 1 )
  
  # Delete a single argument
  event$text <- "test"
  results <- dbr %>% 
    translink.bot::parse_delete(
      event = event
    )
  
  # Make sure test was deleted
  expect_equal( dbr$KEYS("*") %>% length, 0 )
  expect_equal( results %>% strsplit(split = "\n") %>% purrr::flatten_chr() %>% length, 2 )
  
  # Reset DB
  dbr$FLUSHDB()
  
  # Add two now, delete via all and named
  for (j in c("all", "test another")) {
    for (k in 1:2) {
      temp_fun(
        i = k,
        dbr = dbr, 
        event = event
      )
    }
    
    # Make sure keys were created
    expect_equal( dbr$KEYS("*") %>% length, 2 )
    
    # Delete both argument
    event$text <- j %>% translink.bot::split_space()
    results <- dbr %>% 
      translink.bot::parse_delete(
        event = event
      )
    
    # Make sure test was deleted
    expect_equal( dbr$KEYS("*") %>% length, 0 )
    expect_equal( results %>% strsplit(split = "\n") %>% purrr::flatten_chr() %>% length, 3 )
  }

  # Reset DB
  dbr$FLUSHDB()
})
