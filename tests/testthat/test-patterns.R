test_that("pattern_fill returns a pattern object over the background colour", {
  p <- pattern_fill("chainlink", "#005182")
  expect_named(p, "pattern")
  expect_equal(p$pattern$backgroundColor, "#005182")
  expect_equal(p$pattern$patternUnits, "userSpaceOnUse")
  expect_true(nzchar(p$pattern$path$d))
})

test_that("fill-style shapes draw with a fill, stroke-style with an outline", {
  filled <- pattern_fill("triangles", "#DD5D29")
  expect_equal(filled$pattern$path$stroke, "none")
  expect_true(nzchar(filled$pattern$path$fill))

  outlined <- pattern_fill("parquet", "#46B981")
  expect_equal(outlined$pattern$path$fill, "none")
  expect_true(nzchar(outlined$pattern$path$stroke))
  expect_gt(outlined$pattern$path$strokeWidth, 0)
})

test_that("foreground defaults darken the background, wave lightens it", {
  darkened <- pattern_fill("chainlink", "#70C4E8")$pattern$path$fill
  lightened <- pattern_fill("wave", "#70C4E8")$pattern$path$fill
  brightness <- function(hex) sum(grDevices::col2rgb(hex))
  expect_lt(brightness(darkened), brightness("#70C4E8"))
  expect_gt(brightness(lightened), brightness("#70C4E8"))
})

test_that("explicit foreground overrides the derived shade", {
  p <- pattern_fill("triangles", "#DD5D29", foreground = "#b94b24")
  expect_equal(p$pattern$path$fill, "#b94b24")
})

test_that("unknown shapes error with the available names", {
  expect_error(pattern_fill("zigzag", "#005182"), "unknown shape")
})

test_that("pattern_fill_shapes lists the category cycle without the na motif", {
  shapes <- pattern_fill_shapes()
  expect_false("na" %in% shapes)
  expect_true(all(shapes %in% names(pattern_shape_defs())))
  expect_gte(length(shapes), 5)
})

test_that("the pattern hook wraps colours for fill charts and skips line charts", {
  wrap_all <- function(colors) {
    lapply(colors, function(v) {
      if (is.character(v)) pattern_fill("chainlink", v) else v
    })
  }
  old <- options(asthoWidgets.pattern_colors = wrap_all)
  on.exit(options(old))

  pie <- create_base_chart(type = "pie")
  pie_colors <- pie$x$theme$colors
  expect_true(all(vapply(pie_colors, function(v) "pattern" %in% names(v), logical(1))))

  line <- create_base_chart(type = "line")
  expect_type(line$x$theme$colors, "character")

  # other fill-drawn types are wrapped too (#69 follow-up)
  for (ty in c("bubble", "packedbubble", "sankey")) {
    cols <- create_base_chart(type = ty)$x$theme$colors
    expect_true(
      all(vapply(cols, function(v) "pattern" %in% names(v), logical(1))),
      info = ty
    )
  }

  donut <- create_base_chart(type = "pie") |>
    add_pie_chart(
      data.frame(name = c("a", "b"), y = c(1, 2)),
      x_col = "name", y_col = "y",
      pie_options = list(colors = c("#005182", "#70C4E8"))
    )
  slice_colors <- lapply(donut$x$hc_opts$series[[1]]$data, `[[`, "color")
  expect_true(all(vapply(slice_colors, function(v) "pattern" %in% names(v), logical(1))))

  map <- create_base_map(hc_thm = get_astho_hc_theme()) |>
    add_data_layers_to_map(
      map_data = data.frame(id = "x", name = "X", path = "M 0 0", value = "v"),
      color_options = list(
        dataClasses = list(list(name = "v", from = "v", to = "v")),
        colors = "#005182"
      ),
      title_options = list(title = "t"),
      tooltip_options = list(pointFormat = "p")
    )
  expect_true("pattern" %in% names(map$x$hc_opts$colors[[1]]))
})

test_that("without a hook registered, colours pass through untouched", {
  old <- options(asthoWidgets.pattern_colors = NULL)
  on.exit(options(old))
  pie <- create_base_chart(type = "pie")
  expect_type(pie$x$theme$colors, "character")
})

test_that("base builders load the pattern-fill module", {
  map <- create_base_map(hc_thm = get_astho_hc_theme())
  chart <- create_base_chart(type = "pie")
  has_module <- function(hc) {
    any(vapply(
      hc$dependencies %||% list(),
      function(d) any(grepl("pattern-fill", unlist(d$script))),
      logical(1)
    ))
  }
  expect_true(has_module(map))
  expect_true(has_module(chart))
})
