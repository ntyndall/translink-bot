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

  expect_equal( results$startStation, "a" )
  expect_equal( results$stopStation, "b" )
  expect_null( results$updateAction )

})

test_that("Check `set to` statement plus retrieval.", {
  
  # Add text
  event$text <- "<> set home a to b"
  results <- event %>% 
    translink.bot::parse_message(
      dbr = dbr
    )
  
  expect_equal( results$startStation, "a" )
  expect_equal( results$stopStation, "b" )
  expect_null( results$updateAction )
  expect_equal( paste0(event$team, ":", event$user, ":home") %>% dbr$GET(), "a:b" )
  
  # Retrieve favourite
  event$text <- "<> home"
  results <- event %>% 
    translink.bot::parse_message(
      dbr = dbr
    )
  
  expect_equal( results$startStation, "a" )
  expect_equal( results$stopStation, "b" )
  expect_null( results$updateAction )

  # Retrieve favourite (Reversed)
  event$text <- "<> home'"
  results <- event %>% 
    translink.bot::parse_message(
      dbr = dbr
    )
  
  expect_equal( results$startStation, "b" )
  expect_equal( results$stopStation, "a" )
  expect_null( results$updateAction )

  # Reset database
  dbr$FLUSHDB()

})
