test_that("aw_kpi_card returns a shiny.tag", {
  card <- aw_kpi_card(value = 1234, label = "States", icon = "users")
  expect_s3_class(card, "shiny.tag")
})

test_that("aw_kpi_card formats numeric values with thousands separators", {
  card <- aw_kpi_card(value = 1234567, label = "Total")
  rendered <- as.character(card)
  expect_match(rendered, "1,234,567")
})

test_that("aw_kpi_card accepts pre-formatted string values", {
  card <- aw_kpi_card(value = "$1.2M", label = "Expenditures")
  rendered <- as.character(card)
  expect_match(rendered, "\\$1\\.2M")
})

test_that("aw_kpi_grid wraps cards in responsive columns", {
  grid <- aw_kpi_grid(
    aw_kpi_card(value = 1, label = "A"),
    aw_kpi_card(value = 2, label = "B"),
    aw_kpi_card(value = 3, label = "C"),
    col_widths = 3
  )
  expect_s3_class(grid, "shiny.tag")
  expect_match(as.character(grid), "col-md-4")
})

test_that("aw_kpi_grid with no cards renders an empty div", {
  grid <- aw_kpi_grid()
  expect_s3_class(grid, "shiny.tag")
})
