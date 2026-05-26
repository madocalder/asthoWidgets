test_that("aw_definition_popover renders the label and a popover trigger", {
  tag <- aw_definition_popover(
    label = "Governance",
    definition = "How the agency relates to the state government."
  )
  rendered <- as.character(tag)
  expect_match(rendered, "Governance")
  expect_match(rendered, "data-bs-toggle=\"popover\"")
  expect_match(rendered, "How the agency relates")
})

test_that("aw_definition_popover uses the custom title when provided", {
  tag <- aw_definition_popover(
    label = "FTE",
    definition = "Full-time equivalent",
    title = "Workforce metric"
  )
  expect_match(as.character(tag), "Workforce metric")
})

test_that("aw_definition_popover_dependencies returns a script tag", {
  dep <- aw_definition_popover_dependencies()
  expect_s3_class(dep, "shiny.tag")
  expect_equal(dep$name, "script")
})
