# nolint start: line_length_linter

#' Add a line series to a base chart
#'
#' Handles single-series and multi-series trend lines. When \code{group_col}
#' is supplied, one line is rendered per unique value. Optionally supports
#' range visualization with high/low columns.
#'
#' @param hc A \code{highchart} object, typically from
#'   \code{create_base_chart(type = "line")}.
#' @param data A data.frame containing the data to plot.
#' @param x_col Column name (string) for the x-axis values.
#' @param y_col Column name (string) for the y-axis values.
#' @param group_col Optional column name (string) for grouping data into
#'   multiple series.
#' @param high_col Optional column name (string) for the high values in
#'   a range series.
#' @param low_col Optional column name (string) for the low values in
#'   a range series.
#' @param line_options List of line styling options. Supports \code{lineWidth}
#'   (default \code{4}), \code{markerEnabled} (default \code{TRUE}),
#'   \code{markerRadius} (default \code{6}), and \code{dataLabels}.
#' @param xAxis_options List of x-axis customization options. Supports
#'   \code{categories}, \code{title}, and \code{labels} (passed to
#'   \code{labels_with_style}).
#' @param yAxis_options List of y-axis customization options. Supports
#'   \code{title}, \code{min} (default \code{0}), \code{max}, and
#'   \code{labels} (passed to \code{labels_with_style}).
#' @param title_options List of title options. Supports \code{title}
#'   (main title text), \code{margin} (default \code{10}), and
#'   \code{widthAdjust} (default \code{-60}).
#' @param subtitle_options List of subtitle options. Passed to
#'   \code{add_astho_subtitle()}.
#' @param tooltip_options List of tooltip customization options. Supports
#'   \code{pointFormat} (default \code{"<b>{point.name}</b>: {point.y}"}),
#'   \code{headerFormat} (default \code{""}), \code{useHTML} (default
#'   \code{TRUE}), and \code{shared} (default \code{FALSE}).
#' @param legend_options List of legend customization options. Supports
#'   \code{enabled} (default \code{has_groups}), \code{layout} (default
#'   \code{"horizontal"}), \code{align} (default \code{"center"}),
#'   \code{verticalAlign} (default \code{"bottom"}), and \code{titleText}.
#' @param caption_options List of caption options. Supports \code{text},
#'   \code{useHTML} (default \code{TRUE}), and \code{margin} (default
#'   \code{20}).
#' @return A \code{highchart} object with the line series added.
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic line chart
#' hc <- create_base_chart("line")
#' add_line_chart(hc, data = df, x_col = "date", y_col = "value")
#'
#' # Grouped line chart
#' add_line_chart(hc, data = df, x_col = "date", y_col = "value",
#'                group_col = "category")
#'
#' # Line chart with range
#' add_line_chart(hc, data = df, x_col = "date", y_col = "value",
#'                high_col = "high", low_col = "low")
#' }

# nolint end

add_line_chart <- function(hc,
                           data,
                           x_col,
                           y_col,
                           group_col = NULL,
                           high_col = NULL,
                           low_col = NULL,
                           line_options = list(),
                           xAxis_options = list(), # nolint: object_name_linter
                           yAxis_options = list(), # nolint: object_name_linter
                           title_options = list(),
                           subtitle_options = list(),
                           tooltip_options = list(),
                           legend_options = list(),
                           caption_options = list(),
                           range_options = list()
                           ) {
  validate_columns(data, c(x_col, y_col, group_col), "add_line_chart") # nolint: object_usage_linter
  has_groups <- !is.null(group_col)
  has_range <- !is.null(high_col) & !is.null(low_col)

  hc <- hc |>
    highcharter::hc_title(
      text = nz_or_null(title_options$title), # nolint: object_usage_linter
      margin = title_options$margin %||% 10,
      widthAdjust = title_options$widthAdjust %||% -60
    ) |>
    add_astho_subtitle(subtitle_options) |> # nolint: object_usage_linter
    highcharter::hc_caption(
      text = caption_options$text,
      useHTML = caption_options$useHTML %||% TRUE,
      margin = caption_options$margin %||% 20
    ) |>
    highcharter::hc_xAxis(
      type = "category",
      categories = as.list(xAxis_options$categories %||% unique(as.character(data[[x_col]]))),
      title = list(text = xAxis_options$title %||% NULL),
      labels = labels_with_style(xAxis_options$labels) # nolint: object_usage_linter
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

  hc <- add_grow_for_legend_hook(hc) # nolint: object_usage_linter
  # Highcharts reads the marker-symbol cycle from the root `symbols`, not chart.symbols
  hc$x$hc_opts$symbols <- line_marker_symbols() # nolint: object_usage_linter
  hc <- attach_marker_symbols(hc) # nolint: object_usage_linter

  hc <- if (has_groups) {
    hc |> highcharter::hc_add_series_list(
      build_grouped_series(data, x_col, y_col, group_col, "line") # nolint: object_usage_linter
    )
  } else {



    hc <- hc |> highcharter::hc_add_series(
      type = "line",
      name = legend_options$titleText %||% yAxis_options$title %||% y_col,
      data = build_point_data(data, x_col, y_col) # nolint: object_usage_linter
    )

    hc <- if (has_range) {
      hc |> highcharter::hc_add_series(
        data = build_point_data(data, x_col, y_col, high_col, low_col),
        type = "arearange",
        zIndex = -3,
        fillOpacity = range_options$opacity  %||%  .3,
        lineWidth = range_options$lineWidth  %||% 0,
        linkedTo = ":previous"
      )
    }

  }



  apply_export_naming(hc, title_options$title, xAxis_options$title %||% x_col) # nolint: object_usage_linter
}
