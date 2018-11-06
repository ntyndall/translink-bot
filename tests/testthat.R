library(testthat)
library(covr)
library(purrr)
library(magrittr)
library(organisR)

# Run the tests
results <- testthat::test_dir(
  path = "tests/testthat",
  reporter = "summary"
)

# Return response code
quit(
  save = 'no',
  status = results %>% organisR::test_output(),
  runLast = FALSE
)
