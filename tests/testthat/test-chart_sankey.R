test_that("add_sankey_chart returns a highchart", {
  data <- data.frame(
    from = c("State", "State", "Federal"),
    to = c("Programs", "Admin", "Programs"),
    weight = c(100, 50, 200),
    stringsAsFactors = FALSE
  )
  hc <- create_base_chart(type = "sankey") |>
    add_sankey_chart(
      data = data,
      from_col = "from",
      to_col = "to",
      weight_col = "weight",
      title_options = list(title = "Flows")
    )
  expect_s3_class(hc, "highchart")
})

test_that("add_sankey_chart raises on missing columns", {
  data <- data.frame(from = "a", to = "b", weight = 1)
  expect_error(
    create_base_chart(type = "sankey") |>
      add_sankey_chart(
        data = data,
        from_col = "from",
        to_col = "to",
        weight_col = "absent"
      ),
    "absent"
  )
})
