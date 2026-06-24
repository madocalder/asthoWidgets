# nolint start: line_length_linter

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
#'   \code{markerEnabled} (default \code{TRUE}), \code{dataLabels}.
#' @param   xAxis_options,yAxis_options,title_options,subtitle_options,tooltip_options,legend_options,caption_options
#'   Same shape as \code{add_column_chart()}.
#' @return   A \code{highchart} object.
#' @export

# nolint end

add_line_chart <- function(hc,
                           data,
                           x_col,
                           y_col,
                           group_col = NULL,
                           line_options = list(),
                           xAxis_options = list(), # nolint: object_name_linter
                           yAxis_options = list(), # nolint: object_name_linter
                           title_options = list(),
                           subtitle_options = list(),
                           tooltip_options = list(),
                           legend_options = list(),
                           caption_options = list()) {
  validate_columns(data, c(x_col, y_col, group_col), "add_line_chart")
  has_groups <- !is.null(group_col)

  hc <- hc |>
    highcharter::hc_title(
      text = nz_or_null(title_options$title),
      margin = title_options$margin %||% 10,
      widthAdjust = title_options$widthAdjust %||% -60
    ) |>
    highcharter::hc_subtitle(
      text = nz_or_null(subtitle_options$subtitle),
      useHTML = subtitle_options$useHTML %||% TRUE,
      verticalAlign = subtitle_options$verticalAlign %||% "top",
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
      categories = as.list(xAxis_options$categories %||% unique(as.character(data[[x_col]]))),
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
          enabled = line_options$markerEnabled %||% TRUE,
          radius = line_options$markerRadius %||% 6
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

  hc <- add_grow_for_legend_hook(hc)
  # Highcharts reads the marker-symbol cycle from the root `symbols`, not chart.symbols
  hc$x$hc_opts$symbols <- line_marker_symbols()
  hc <- attach_marker_symbols(hc)

  hc <- if (has_groups) {
    hc |> highcharter::hc_add_series_list(
      build_grouped_series(data, x_col, y_col, group_col, "line")
    )
  } else {
    hc |> highcharter::hc_add_series(
      type = "line",
      name = legend_options$titleText %||% yAxis_options$title %||% y_col,
      data = build_point_data(data, x_col, y_col)
    )
  }
  apply_export_naming(hc, title_options$title, xAxis_options$title %||% x_col)
}
