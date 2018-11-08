context("test-parse_set.R")

# Set up default team and user info
event <- list(
  team_id = "teamA",
  user_id = "userA"
)

# Set up REDIS connection
dbr <- redux::hiredis()

# Begin tests
test_that("Check `set` statement (/tset).", {

  # Add text
  event$text <- "test a to b" %>% translink.bot::split_space()
  results <- dbr %>% 
    translink.bot::parse_set(
      event = event
    )

  # Check tests
  expect_equal( dbr$KEYS("*") %>% purrr::flatten_chr(), event %>% purrr::map(1) %>% paste(collapse = ":") )
  expect_true( "*test*" %in% (results %>% translink.bot::split_space()) )

  # Reset DB
  dbr$FLUSHDB()
  
})
