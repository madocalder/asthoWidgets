test_that("add_dumbbell_chart returns a highchart", {
  data <- data.frame(
    category = c("A", "B", "C"),
    low = c(10, 20, 30),
    high = c(40, 50, 60),
    stringsAsFactors = FALSE
  )
  hc <- create_base_chart(type = "dumbbell") |>
    add_dumbbell_chart(
      data = data,
      x_col = "category",
      low_col = "low",
      high_col = "high",
      title_options = list(title = "Test")
    )
  expect_s3_class(hc, "highchart")
})

test_that("add_dumbbell_chart raises on missing columns", {
  data <- data.frame(category = "A", low = 1, high = 2)
  expect_error(
    create_base_chart(type = "dumbbell") |>
      add_dumbbell_chart(
        data = data,
        x_col = "category",
        low_col = "low",
        high_col = "missing_col"
      ),
    "missing_col"
  )
})

test_that("add_dumbbell_chart handles grouped data", {
  data <- data.frame(
    category = rep(c("A", "B"), 2),
    low = c(10, 20, 15, 25),
    high = c(40, 50, 45, 55),
    grp = rep(c("g1", "g2"), each = 2),
    stringsAsFactors = FALSE
  )
  hc <- create_base_chart(type = "dumbbell") |>
    add_dumbbell_chart(
      data = data,
      x_col = "category",
      low_col = "low",
      high_col = "high",
      group_col = "grp"
    )
  expect_s3_class(hc, "highchart")
})
