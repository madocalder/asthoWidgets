test_that("add_dumbbell_chart returns a highchart with one series and 3 points", {
  df <- data.frame(
    category = c("A", "B", "C"),
    low      = c(10, 20, 30),
    high     = c(15, 25, 50),
    stringsAsFactors = FALSE
  )
  hc <- create_base_chart(type = "dumbbell") |>
    add_dumbbell_chart(
      data     = df,
      x_col    = "category",
      low_col  = "low",
      high_col = "high",
      title_options = list(title = "Test")
    )
  expect_s3_class(hc, "highchart")
  series <- hc$x$hc_opts$series
  expect_length(series, 1)
  expect_length(series[[1]]$data, 3)
})

test_that("add_dumbbell_chart with group_col emits one series per group", {
  df <- data.frame(
    category = rep(c("A", "B"), times = 2),
    low      = c(10, 20, 12, 22),
    high     = c(15, 25, 18, 28),
    series   = rep(c("2019", "2021"), each = 2),
    stringsAsFactors = FALSE
  )
  hc <- create_base_chart(type = "dumbbell") |>
    add_dumbbell_chart(
      data      = df,
      x_col     = "category",
      low_col   = "low",
      high_col  = "high",
      group_col = "series",
      title_options = list(title = "Test")
    )
  expect_s3_class(hc, "highchart")
  expect_length(hc$x$hc_opts$series, 2)
})
