test_that("add_line_chart returns a highchart in all branches", {
  df <- data.frame(
    date     = c("2020", "2021", "2022"),
    value    = c(10, 20, 15),
    category = c("A", "B", "A"),
    high     = c(12, 22, 17),
    low      = c(8, 18, 13)
  )

  base <- create_base_chart("line")

  # Single line, no range (the previously broken path)
  hc1 <- add_line_chart(base, data = df, x_col = "date", y_col = "value")
  expect_s3_class(hc1, "highchart")
  expect_false(is.null(hc1))

  # Grouped
  hc2 <- add_line_chart(base, data = df, x_col = "date", y_col = "value",
                        group_col = "category")
  expect_s3_class(hc2, "highchart")

  # Range
  hc3 <- add_line_chart(base, data = df, x_col = "date", y_col = "value",
                        high_col = "high", low_col = "low")
  expect_s3_class(hc3, "highchart")

  # Grouped + range
  hc4 <- add_line_chart(base, data = df, x_col = "date", y_col = "value",
                        group_col = "category",
                        high_col = "high", low_col = "low")
  expect_s3_class(hc4, "highchart")
})
