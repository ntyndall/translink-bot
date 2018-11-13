context("test-parse_lines.R")

# Set up REDIS connection
dbr <- redux::hiredis()


# Begin tests
test_that("Make sure that the station information can be returned back", {

  # Insert the station list first
  system.file("extdata", "trainlines.json", package = "translink.bot") %>% 
    jsonlite::fromJSON() %>% 
    translink.bot::station_lines()  
  
  # Get results, and parse to data frame
  results <- dbr %>%
    translink.bot::parse_lines() %>% 
    jsonlite::fromJSON() %>%
    `[[`("attachments")
  
  # Check tests
  expect_is( results, "data.frame" )
  expect_gt( results %>% nrow, 0 )
  expect_gt( results %>% ncol, 0 )

  # Reset DB
  dbr$FLUSHDB()
})