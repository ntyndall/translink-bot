context("test-parse_message.R")

# Set up default team and user info
event <- list(
  team = "teamA",
  user = "userA"
)

# Set up REDIS connection
dbr <- redux::hiredis()

test_that("Return a simple `to` statement.", {

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

test_that("Make sure favourites can be set.", {
  
  # Add text
  event$text <- "<> set home a to b"
  results <- event %>% 
    translink.bot::parse_message(
      dbr = dbr
    )
  
  expect_equal( results$startStation, "a" )
  expect_equal( results$stopStation, "b" )
  expect_null( results$updateAction )
  
  # Check keyword has been set in redis
  expect_equal( paste0(event$team, ":", event$user, ":home") %>% dbr$GET(), "a:b" )
  
  dbr$FLUSHDB()

})