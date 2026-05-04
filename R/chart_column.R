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
      text = nz_or_null(title_options$title),
      margin = title_options$margin %||% 10,
      widthAdjust = title_options$widthAdjust %||% -60
    ) |>
    highcharter::hc_subtitle(
      text = nz_or_null(subtitle_options$subtitle),
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
