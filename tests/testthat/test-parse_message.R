context("test-parse_message.R")

# Set up default team and user info
event <- list(
  team = "teamA",
  user = "userA"
)

# Set up REDIS connection
dbr <- redux::hiredis()


test_that("Check `to` statement.", {

  # Add text
  event$text <- "<> a to b"
  results <- event %>% 
    translink.bot::parse_message(
      dbr = dbr
    )

  expect_equal( results$startSt, "a" )
  expect_equal( results$stopSt, "b" )
  expect_null( results$updateAction )

  # Reset DB
  dbr$FLUSHDB()
  
})


test_that("Check `set to` statement plus retrieval.", {
  
  # Add text
  event$text <- "<> set home a to b"
  results <- event %>% 
    translink.bot::parse_message(
      dbr = dbr
    )
  
  expect_null( results$startSt, "a" )
  expect_null( results$stopSt, "b" )
  expect_gt( results$updateAction %>% nchar, 0 )
  expect_equal( paste0(event$team, ":", event$user, ":home") %>% dbr$GET(), "a:b" )
  
  # Retrieve favourite
  event$text <- "<> home"
  results <- event %>% 
    translink.bot::parse_message(
      dbr = dbr
    )
  
  expect_equal( results$startSt, "a" )
  expect_equal( results$stopSt, "b" )
  expect_null( results$updateAction )

  # Retrieve favourite (Reversed)
  event$text <- "<> home'"
  results <- event %>% 
    translink.bot::parse_message(
      dbr = dbr
    )
  
  expect_equal( results$startSt, "b" )
  expect_equal( results$stopSt, "a" )
  expect_null( results$updateAction )

  # Reset database
  dbr$FLUSHDB()

})


test_that("Check `info` and `all` statements.", {
  
  # Set a favourite first
  event$text <- "<> set home a to b"
  results <- event %>% 
    translink.bot::parse_message(
      dbr = dbr
    )

  # Get information
  for (i in c("all", "home")) {
    event$text <- paste0("<> info ", i)
    results <- event %>% 
      translink.bot::parse_message(
        dbr = dbr
      )
    
    # Information has been appended
    expect_null( results$startSt )
    expect_null( results$stopSt )
    expect_gt( results$updateAction %>% nchar, 0 )
  }

  # Flush DB
  dbr$FLUSHDB()
  
})


test_that("Check `delete` and `all` statements.", {
  
  # Get information
  for (i in c("all", "home")) {
    # Set a favourite first
    event$text <- "<> set home a to b"
    results <- event %>% 
      translink.bot::parse_message(
        dbr = dbr
      )

    event$text <- paste0("<> delete ", i)
    results <- event %>% 
      translink.bot::parse_message(
        dbr = dbr
      )
    
    # Information has been appended
    expect_null( results$startSt )
    expect_null( results$stopSt )
    expect_gt( results$updateAction %>% nchar, 0 )
  }
  
  # Flush DB
  dbr$FLUSHDB()
  
})
