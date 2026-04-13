test_that("hello() works", {
  expect_message(hello(), "Hello, World!")
  expect_message(hello("You"), "Hello, You!")
})
