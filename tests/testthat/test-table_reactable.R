test_that("aw_reactable returns a reactable htmlwidget", {
  skip_if_not_installed("reactable")
  data <- data.frame(state = c("AL", "AK"), value = c(1, 2))
  tbl <- aw_reactable(data)
  expect_s3_class(tbl, "reactable")
  expect_s3_class(tbl, "htmlwidget")
})

test_that("aw_reactable honours overridden defaults", {
  skip_if_not_installed("reactable")
  data <- data.frame(state = c("AL", "AK"), value = c(1, 2))
  tbl <- aw_reactable(data, defaultPageSize = 25, searchable = FALSE)
  expect_s3_class(tbl, "reactable")
})
