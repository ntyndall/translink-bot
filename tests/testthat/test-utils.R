context("test-utils.R")


# Begin tests
test_that("Check convert time works as expected for single + multi args", {
  
  expect_equal( "" %>% translink.bot::conv_time(), "Unknown" )
  expect_equal( "1023" %>% translink.bot::conv_time(), "10:23" )
  expect_equal( c("", "1023") %>% translink.bot::conv_time(), c("Unknown", "10:23") )

})

test_that("Character vector can be split", {

  expect_equal( "1" %>% translink.bot::split_space(), "1" )
  expect_equal( c("1 2") %>% translink.bot::split_space(), c("1", "2") )

})

test_that("Format text appropriately", {

  expect_equal( "1" %>% translink.bot::format_name(), "1" )
  expect_equal( "a test" %>% translink.bot::format_name(), "A Test" )
  expect_equal( "A TEST" %>% translink.bot::format_name(), "A Test" )

})