#' Create an empty themed Highchart into which a single chart series can be added
#'
#' Mirrors \code{create_base_map()} but for non-map chart types (column, bar,
#' pie, line, etc.). Use the returned object as the first argument to one of
#' the \code{add_*_chart()} helpers.
#'
#' @param   type   Highcharts chart type, e.g. \code{"pie"}, \code{"column"},
#'   \code{"bar"}, \code{"line"}, \code{"bubble"}, \code{"wordcloud"}.
#' @param   hc_thm   Highchart theme object. Defaults to
#'   \code{get_astho_hc_theme()} so charts pick up the ASTHO brand styling
#'   without the caller having to wire it up.
#' @param   export_options   List. Options forwarded to \code{viz_export()}
#'   for the chart's PNG / SVG / CSV / XLS download menu.
#' @return   A \code{highchart} htmlwidget.
#' @export
create_base_chart <- function(type,
                              hc_thm = get_astho_hc_theme(),
                              export_options = list()) {
  highcharter::highchart() |>
    highcharter::hc_chart(type = type) |>
    highcharter::hc_add_theme(hc_thm = hc_thm) |>
    viz_export(export_options)
}

#' Add a pie or donut series to a base chart
#'
#' Each entry in the option lists falls back to a sensible default if absent,
#' so the simplest call only needs \code{data}, \code{x_col} and \code{y_col}.
#' Set \code{pie_options$innerSize} (e.g. \code{"70\%"}) to render a donut.
#'
#' @param   hc   A \code{highchart} object, typically from
#'   \code{create_base_chart(type = "pie")}.
#' @param   data   A data.frame. Must contain the columns named in \code{x_col}
#'   and \code{y_col}; any extra columns remain accessible in tooltips via the
#'   \code{point.<column>} reference.
#' @param   x_col   Column name (string) holding slice labels.
#' @param   y_col   Column name (string) holding slice values.
#' @param   z_col   Optional column name for the third axis (commonly used as
#'   a tooltip-only quantity such as a raw count).
#' @param   pie_options   Optional. Per-slice plot options. Recognised entries:
#'   \code{innerSize} (default \code{"70\%"}), \code{allowPointSelect}
#'   (\code{TRUE}), \code{cursor} (\code{"pointer"}), \code{showInLegend}
#'   (\code{TRUE}), \code{dataLabels} (the full Highcharts dataLabels list).
#' @param   title_options   Essential: \code{title}. Optional: \code{margin},
#'   \code{widthAdjust}.
#' @param   subtitle_options   Optional: \code{subtitle}, \code{useHTML},
#'   \code{verticalAlign}, \code{x}, \code{y}.
#' @param   tooltip_options   Optional: \code{pointFormat}, \code{headerFormat},
#'   \code{useHTML}.
#' @param   legend_options   Optional: \code{enabled}, \code{layout},
#'   \code{align}, \code{verticalAlign}, \code{titleText}.
#' @param   caption_options   Optional: \code{text}, \code{useHTML},
#'   \code{margin}.
#' @return   A \code{highchart} object.
#' @export
add_pie_chart <- function(hc,
                          data,
                          x_col,
                          y_col,
                          z_col = NULL,
                          pie_options = list(),
                          title_options = list(),
                          subtitle_options = list(),
                          tooltip_options = list(),
                          legend_options = list(),
                          caption_options = list()) {
  series_data <- pie_series_data(data, x_col, y_col, z_col)

  hc |>
    highcharter::hc_chart(crop = FALSE) |>
    highcharter::hc_title(
      text = title_options$title,
      margin = title_options$margin %||% 10,
      widthAdjust = title_options$widthAdjust %||% -60
    ) |>
    highcharter::hc_subtitle(
      text = subtitle_options$subtitle,
      useHTML = subtitle_options$useHTML %||% TRUE,
      verticalAlign = subtitle_options$verticalAlign %||% "bottom",
      y = subtitle_options$y %||% 30,
      x = subtitle_options$x %||% 0
    ) |>
    highcharter::hc_caption(
      text = caption_options$text,
      useHTML = caption_options$useHTML %||% TRUE,
      margin = caption_options$margin %||% 20
    ) |>
    highcharter::hc_plotOptions(
      pie = list(
        innerSize = pie_options$innerSize %||% "70%",
        allowPointSelect = pie_options$allowPointSelect %||% TRUE,
        cursor = pie_options$cursor %||% "pointer",
        showInLegend = pie_options$showInLegend %||% TRUE,
        dataLabels = pie_options$dataLabels %||% list(
          enabled = TRUE,
          format = "{point.percentage:.0f}%",
          distance = "20%",
          style = list(fontFamily = "Jost", fontSize = "14px"),
          connectorWidth = 3
        )
      )
    ) |>
    highcharter::hc_tooltip(
      pointFormat = tooltip_options$pointFormat %||%
        "<b>{point.percentage:.0f}%</b> &mdash; {point.name}",
      headerFormat = tooltip_options$headerFormat %||% "",
      useHTML = tooltip_options$useHTML %||% TRUE
    ) |>
    highcharter::hc_legend(
      enabled = legend_options$enabled %||% TRUE,
      layout = legend_options$layout %||% "horizontal",
      align = legend_options$align %||% "center",
      verticalAlign = legend_options$verticalAlign %||% "bottom",
      title = list(text = legend_options$titleText %||% NULL)
    ) |>
    highcharter::hc_add_series(
      type = "pie",
      data = series_data
    )
}

#' Build the per-point data list a Highcharts pie series expects
#'
#' Renames the user-selected x/y/z columns to Highcharts' canonical \code{name},
#' \code{y}, \code{z} names while keeping all other columns intact so tooltip
#' templates can still reference them via \code{{point.<column>}}.
#' @noRd
pie_series_data <- function(data, x_col, y_col, z_col = NULL) {
  if (!x_col %in% names(data)) {
    stop("add_pie_chart(): x_col '", x_col, "' not found in data.")
  }
  if (!y_col %in% names(data)) {
    stop("add_pie_chart(): y_col '", y_col, "' not found in data.")
  }
  data$name <- data[[x_col]]
  data$y    <- data[[y_col]]
  if (!is.null(z_col)) {
    if (!z_col %in% names(data)) {
      stop("add_pie_chart(): z_col '", z_col, "' not found in data.")
    }
    data$z <- data[[z_col]]
  }
  highcharter::list_parse(data)
}

#' Add a column / bar series to a base chart
#'
#' Handles single-series, multi-series grouped, stacked, and horizontal
#' (\code{column_options$inverted = TRUE}, which is what Highcharts calls a
#' bar chart) variants. When \code{group_col} is supplied, one series is
#' rendered per unique value, coloured from the chart's theme palette.
#'
#' @param   hc   A \code{highchart} object, typically from
#'   \code{create_base_chart(type = "column")}.
#' @param   data   A data.frame with at least \code{x_col} and \code{y_col};
#'   include \code{group_col} for grouped/stacked layouts. Extra columns
#'   stay accessible to tooltip templates as \code{point.<column>}.
#' @param   x_col   Column name (string) for the x-axis category.
#' @param   y_col   Column name (string) for the y-axis value.
#' @param   group_col   Optional column name. When supplied, data is split
#'   into one Highcharts series per unique value.
#' @param   column_options   Optional. Recognised entries: \code{inverted}
#'   (default \code{FALSE}; renders horizontal bars), \code{stacking}
#'   (\code{"normal"}, \code{"percent"}, or \code{NULL}), \code{pointWidth},
#'   \code{dataLabels} (full Highcharts dataLabels list).
#' @param   xAxis_options   Optional: \code{title}, \code{categories}.
#'   \code{categories} defaults to \code{unique(data[[x_col]])}.
#' @param   yAxis_options   Optional: \code{title}, \code{min} (default
#'   \code{0}), \code{max}, \code{labels} (full Highcharts labels list).
#' @param   title_options   Essential: \code{title}. Optional: \code{margin},
#'   \code{widthAdjust}.
#' @param   subtitle_options   Optional: \code{subtitle}, \code{useHTML},
#'   \code{verticalAlign}, \code{x}, \code{y}.
#' @param   tooltip_options   Optional: \code{pointFormat},
#'   \code{headerFormat}, \code{useHTML}, \code{shared}.
#' @param   legend_options   Optional: \code{enabled} (default \code{TRUE}
#'   when \code{group_col} is set, otherwise \code{FALSE}), \code{layout},
#'   \code{align}, \code{verticalAlign}, \code{titleText}.
#' @param   caption_options   Optional: \code{text}, \code{useHTML},
#'   \code{margin}.
#' @return   A \code{highchart} object.
#' @export
add_column_chart <- function(hc,
                             data,
                             x_col,
                             y_col,
                             group_col = NULL,
                             column_options = list(),
                             xAxis_options = list(),
                             yAxis_options = list(),
                             title_options = list(),
                             subtitle_options = list(),
                             tooltip_options = list(),
                             legend_options = list(),
                             caption_options = list()) {
  validate_columns(data, c(x_col, y_col, group_col), "add_column_chart")
  has_groups <- !is.null(group_col)

  hc <- hc |>
    highcharter::hc_chart(
      inverted = column_options$inverted %||% FALSE
    ) |>
    highcharter::hc_title(
      text = title_options$title,
      margin = title_options$margin %||% 10,
      widthAdjust = title_options$widthAdjust %||% -60
    ) |>
    highcharter::hc_subtitle(
      text = subtitle_options$subtitle,
      useHTML = subtitle_options$useHTML %||% TRUE,
      verticalAlign = subtitle_options$verticalAlign %||% "bottom",
      y = subtitle_options$y %||% 30,
      x = subtitle_options$x %||% 0
    ) |>
    highcharter::hc_caption(
      text = caption_options$text,
      useHTML = caption_options$useHTML %||% TRUE,
      margin = caption_options$margin %||% 20
    ) |>
    highcharter::hc_xAxis(
      type = "category",
      categories = xAxis_options$categories %||% unique(as.character(data[[x_col]])),
      title = list(text = xAxis_options$title %||% NULL),
      labels = labels_with_style(xAxis_options$labels)
    ) |>
    highcharter::hc_yAxis(
      title = list(text = yAxis_options$title %||% NULL),
      min = yAxis_options$min %||% 0,
      max = yAxis_options$max,
      labels = labels_with_style(yAxis_options$labels)
    ) |>
    highcharter::hc_plotOptions(
      column = list(
        stacking = column_options$stacking,
        pointWidth = column_options$pointWidth,
        dataLabels = column_options$dataLabels %||% list(
          enabled = TRUE,
          format = "{point.y}",
          style = list(fontFamily = "Jost", fontSize = "13px")
        )
      ),
      series = list(
        animation = list(duration = 800)
      )
    ) |>
    highcharter::hc_tooltip(
      pointFormat = tooltip_options$pointFormat %||%
        "<b>{point.name}</b>: {point.y}",
      headerFormat = tooltip_options$headerFormat %||% "",
      useHTML = tooltip_options$useHTML %||% TRUE,
      shared = tooltip_options$shared %||% FALSE
    ) |>
    highcharter::hc_legend(
      enabled = legend_options$enabled %||% has_groups,
      layout = legend_options$layout %||% "horizontal",
      align = legend_options$align %||% "center",
      verticalAlign = legend_options$verticalAlign %||% "bottom",
      title = list(text = legend_options$titleText %||% NULL)
    )

  if (has_groups) {
    series_list <- build_grouped_series(data, x_col, y_col, group_col, "column")
    hc |> highcharter::hc_add_series_list(series_list)
  } else {
    hc |> highcharter::hc_add_series(
      type = "column",
      name = legend_options$titleText %||% y_col,
      data = build_point_data(data, x_col, y_col)
    )
  }
}

#' Add a line series to a base chart
#'
#' Handles single-series and multi-series trend lines. When \code{group_col}
#' is supplied, one line is rendered per unique value.
#'
#' @param   hc   A \code{highchart} object, typically from
#'   \code{create_base_chart(type = "line")}.
#' @param   data   A data.frame.
#' @param   x_col,y_col,group_col   Column names (strings). \code{group_col}
#'   is optional.
#' @param   line_options   Optional: \code{lineWidth} (default \code{4}),
#'   \code{markerEnabled} (default \code{FALSE}), \code{dataLabels}.
#' @param   xAxis_options,yAxis_options,title_options,subtitle_options,
#'   tooltip_options,legend_options,caption_options   Same shape as
#'   \code{add_column_chart()}.
#' @return   A \code{highchart} object.
#' @export
add_line_chart <- function(hc,
                           data,
                           x_col,
                           y_col,
                           group_col = NULL,
                           line_options = list(),
                           xAxis_options = list(),
                           yAxis_options = list(),
                           title_options = list(),
                           subtitle_options = list(),
                           tooltip_options = list(),
                           legend_options = list(),
                           caption_options = list()) {
  validate_columns(data, c(x_col, y_col, group_col), "add_line_chart")
  has_groups <- !is.null(group_col)

  hc <- hc |>
    highcharter::hc_title(
      text = title_options$title,
      margin = title_options$margin %||% 10,
      widthAdjust = title_options$widthAdjust %||% -60
    ) |>
    highcharter::hc_subtitle(
      text = subtitle_options$subtitle,
      useHTML = subtitle_options$useHTML %||% TRUE,
      verticalAlign = subtitle_options$verticalAlign %||% "bottom",
      y = subtitle_options$y %||% 30,
      x = subtitle_options$x %||% 0
    ) |>
    highcharter::hc_caption(
      text = caption_options$text,
      useHTML = caption_options$useHTML %||% TRUE,
      margin = caption_options$margin %||% 20
    ) |>
    highcharter::hc_xAxis(
      type = "category",
      categories = xAxis_options$categories %||% unique(as.character(data[[x_col]])),
      title = list(text = xAxis_options$title %||% NULL),
      labels = labels_with_style(xAxis_options$labels)
    ) |>
    highcharter::hc_yAxis(
      title = list(text = yAxis_options$title %||% NULL),
      min = yAxis_options$min %||% 0,
      max = yAxis_options$max,
      labels = labels_with_style(yAxis_options$labels)
    ) |>
    highcharter::hc_plotOptions(
      line = list(
        lineWidth = line_options$lineWidth %||% 4,
        marker = list(
          enabled = line_options$markerEnabled %||% FALSE
        ),
        dataLabels = line_options$dataLabels %||% list(enabled = FALSE)
      )
    ) |>
    highcharter::hc_tooltip(
      pointFormat = tooltip_options$pointFormat %||%
        "<b>{point.name}</b>: {point.y}",
      headerFormat = tooltip_options$headerFormat %||% "",
      useHTML = tooltip_options$useHTML %||% TRUE,
      shared = tooltip_options$shared %||% FALSE
    ) |>
    highcharter::hc_legend(
      enabled = legend_options$enabled %||% has_groups,
      layout = legend_options$layout %||% "horizontal",
      align = legend_options$align %||% "center",
      verticalAlign = legend_options$verticalAlign %||% "bottom",
      title = list(text = legend_options$titleText %||% NULL)
    )

  if (has_groups) {
    series_list <- build_grouped_series(data, x_col, y_col, group_col, "line")
    hc |> highcharter::hc_add_series_list(series_list)
  } else {
    hc |> highcharter::hc_add_series(
      type = "line",
      name = legend_options$titleText %||% y_col,
      data = build_point_data(data, x_col, y_col)
    )
  }
}

#' Add a word cloud series to a base chart
#'
#' Renders a Highcharts wordcloud where each word's font size is proportional
#' to its weight.
#'
#' @param   hc   A \code{highchart} object, typically from
#'   \code{create_base_chart(type = "wordcloud")}.
#' @param   data   A data.frame.
#' @param   word_col   Column name (string) for the word text.
#' @param   weight_col   Column name (string) for the word weight (frequency).
#' @param   wordcloud_options   Optional: \code{minFontSize},
#'   \code{maxFontSize}, \code{rotation} (a list controlling the word
#'   rotation distribution).
#' @param   title_options,subtitle_options,tooltip_options,caption_options
#'   Same shape as \code{add_column_chart()}.
#' @return   A \code{highchart} object.
#' @export
add_word_cloud <- function(hc,
                           data,
                           word_col,
                           weight_col,
                           wordcloud_options = list(),
                           title_options = list(),
                           subtitle_options = list(),
                           tooltip_options = list(),
                           caption_options = list()) {
  validate_columns(data, c(word_col, weight_col), "add_word_cloud")

  series_data <- lapply(seq_len(nrow(data)), function(i) {
    list(name = data[[word_col]][i], weight = data[[weight_col]][i])
  })

  hc |>
    highcharter::hc_title(
      text = title_options$title,
      margin = title_options$margin %||% 10,
      widthAdjust = title_options$widthAdjust %||% -60
    ) |>
    highcharter::hc_subtitle(
      text = subtitle_options$subtitle,
      useHTML = subtitle_options$useHTML %||% TRUE,
      verticalAlign = subtitle_options$verticalAlign %||% "bottom",
      y = subtitle_options$y %||% 30,
      x = subtitle_options$x %||% 0
    ) |>
    highcharter::hc_caption(
      text = caption_options$text,
      useHTML = caption_options$useHTML %||% TRUE,
      margin = caption_options$margin %||% 20
    ) |>
    highcharter::hc_tooltip(
      pointFormat = tooltip_options$pointFormat %||% "<b>{point.name}</b>: {point.weight}",
      headerFormat = tooltip_options$headerFormat %||% "",
      useHTML = tooltip_options$useHTML %||% TRUE
    ) |>
    highcharter::hc_add_series(
      type = "wordcloud",
      data = series_data,
      minFontSize = wordcloud_options$minFontSize %||% 10,
      maxFontSize = wordcloud_options$maxFontSize %||% 60,
      rotation = wordcloud_options$rotation %||% list(from = 0, to = 0)
    )
}

# Internal helpers ------------------------------------------------------------

#' Default font/colour applied to axis labels.
#'
#' Highcharts replaces (rather than merges) the theme's
#' \code{xAxis$labels$style} block when a chart specifies its own
#' \code{labels} object, even if that object only sets \code{format}. If
#' the resulting labels have no \code{style$fontSize} the client-side
#' \code{labelMetrics()} call crashes with
#' \code{Cannot read properties of undefined (reading 'fontSize')}. This
#' default mirrors the values set in \code{get_astho_hc_theme()} so axes
#' keep their theme styling whenever a caller overrides only the format.
#' @noRd
default_axis_label_style <- function() {
  list(
    fontFamily = "Jost",
    fontSize = "15px",
    fontWeight = "normal",
    color = "#666"
  )
}

#' Ensure axis label options always carry a \code{style} block.
#'
#' Pass a user-supplied \code{labels} list (or \code{NULL}); returns a
#' list with \code{style} set to \code{default_axis_label_style()} when
#' the caller hasn't supplied one of their own.
#' @noRd
labels_with_style <- function(opts) {
  if (is.null(opts)) opts <- list()
  if (is.null(opts$style)) opts$style <- default_axis_label_style()
  opts
}

#' Validate that all referenced columns exist in `data`.
#' @noRd
validate_columns <- function(data, cols, fn_name) {
  cols <- cols[!is.null(cols) & nzchar(cols)]
  missing <- setdiff(cols, names(data))
  if (length(missing) > 0) {
    stop(
      fn_name, "(): column(s) not found in data: ",
      paste(missing, collapse = ", "),
      ". Available: ", paste(names(data), collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Build per-point list for a single series.
#' Keeps every column in `data` so tooltip templates can reference them via
#' `{point.<col>}`.
#' @noRd
build_point_data <- function(data, x_col, y_col) {
  data$name <- data[[x_col]]
  data$y    <- data[[y_col]]
  highcharter::list_parse(data)
}

#' Split `data` by `group_col` and emit one series per unique value.
#' @noRd
build_grouped_series <- function(data, x_col, y_col, group_col, type) {
  groups <- unique(as.character(data[[group_col]]))
  lapply(groups, function(g) {
    sub <- data[as.character(data[[group_col]]) == g, , drop = FALSE]
    list(
      name = g,
      type = type,
      data = build_point_data(sub, x_col, y_col)
    )
  })
}

# Local infix helper duplicated per file so each source file is self-contained.
`%||%` <- function(a, b) if (is.null(a)) b else a
