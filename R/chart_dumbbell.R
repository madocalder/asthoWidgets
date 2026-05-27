#' Add a dumbbell series to a base chart
#'
#' Renders a horizontal dumbbell where each category has a connector
#' between a low and a high value. Categories appear on the y-axis,
#' values on the x-axis. The chart is automatically inverted so the
#' caller does not have to think about axis orientation.
#'
#' Each point exposes \code{point.low} and \code{point.high} for use in
#' tooltip templates, plus any other column from \code{data} via
#' \code{point.<column>}.
#'
#' @param   hc   A \code{highchart} object, typically from
#'   \code{create_base_chart(type = "dumbbell")}.
#' @param   data   A data.frame with at least \code{x_col}, \code{low_col},
#'   and \code{high_col}.
#' @param   x_col   Column name (string) for the category.
#' @param   low_col   Column name (string) for the low value of each dumbbell.
#' @param   high_col   Column name (string) for the high value of each dumbbell.
#' @param   group_col   Optional column name. When supplied, data is split
#'   into one Highcharts series per unique value.
#' @param   dumbbell_options   Optional. Recognised entries:
#'   \code{lowColor} (default \code{"#005182"} - ASTHO main blue),
#'   \code{color} (default \code{"#C65227"} - ASTHO orange; used for the
#'   connector line and the high marker), \code{connectorWidth} (default
#'   \code{2}), \code{markerRadius} (default \code{7}).
#' @param   xAxis_options   Optional: \code{title}, \code{labels}. Note
#'   that because the chart is inverted, this is the visible vertical
#'   axis (categories).
#' @param   yAxis_options   Optional: \code{title}, \code{min}, \code{max},
#'   \code{labels}. Visible horizontal axis (values).
#' @param   title_options   Essential: \code{title}.
#' @param   subtitle_options   Optional: \code{subtitle}.
#' @param   tooltip_options   Optional: \code{pointFormat} (default uses
#'   \code{{point.low}} and \code{{point.high}}), \code{headerFormat},
#'   \code{useHTML}, \code{shared}.
#' @param   legend_options   Optional: \code{enabled} (default depends on
#'   whether \code{group_col} is set).
#' @param   caption_options   Optional: \code{text}, \code{useHTML},
#'   \code{margin}.
#' @return   A \code{highchart} object.
#' @export
add_dumbbell_chart <- function(hc,
                               data,
                               x_col,
                               low_col,
                               high_col,
                               group_col = NULL,
                               dumbbell_options = list(),
                               xAxis_options = list(), # nolint: object_name_linter
                               yAxis_options = list(), # nolint: object_name_linter
                               title_options = list(),
                               subtitle_options = list(),
                               tooltip_options = list(),
                               legend_options = list(),
                               caption_options = list()) {
  validate_columns(data, c(x_col, low_col, high_col, group_col), "add_dumbbell_chart")
  has_groups <- !is.null(group_col)

  categories <- xAxis_options$categories %||% unique(as.character(data[[x_col]]))

  hc <- hc |>
    highcharter::hc_chart(
      type = "dumbbell",
      inverted = TRUE
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
      categories = categories,
      title = list(text = xAxis_options$title %||% NULL),
      labels = labels_with_style(xAxis_options$labels)
    ) |>
    highcharter::hc_yAxis(
      title = list(text = yAxis_options$title %||% NULL),
      min = yAxis_options$min,
      max = yAxis_options$max,
      labels = labels_with_style(yAxis_options$labels)
    ) |>
    highcharter::hc_plotOptions(
      dumbbell = list(
        lowColor = dumbbell_options$lowColor %||% "#005182",
        color = dumbbell_options$color %||% "#C65227",
        connectorWidth = dumbbell_options$connectorWidth %||% 2,
        marker = list(radius = dumbbell_options$markerRadius %||% 7)
      ),
      series = list(
        animation = list(duration = 800)
      )
    ) |>
    highcharter::hc_tooltip(
      pointFormat = tooltip_options$pointFormat %||%
        "<b>{point.name}</b><br>Low: {point.low}<br>High: {point.high}",
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
    series_list <- build_dumbbell_grouped_series(data, x_col, low_col, high_col, group_col)
    hc |> highcharter::hc_add_series_list(series_list)
  } else {
    hc |> highcharter::hc_add_series(
      type = "dumbbell",
      name = legend_options$titleText %||% paste(low_col, "to", high_col),
      data = build_dumbbell_point_data(data, x_col, low_col, high_col)
    )
  }
}

#' Build the per-point list a Highcharts dumbbell series expects.
#'
#' Each point needs \code{name}, \code{low}, and \code{high}. Any other
#' column in \code{data} is preserved so tooltips can reference it via
#' \code{point.<column>}.
#' @noRd
build_dumbbell_point_data <- function(data, x_col, low_col, high_col) {
  data$name <- data[[x_col]]
  data$low <- data[[low_col]]
  data$high <- data[[high_col]]
  highcharter::list_parse(data)
}

#' Split data by group and emit one dumbbell series per unique value.
#' @noRd
build_dumbbell_grouped_series <- function(data, x_col, low_col, high_col, group_col) {
  groups <- unique(as.character(data[[group_col]]))
  lapply(groups, function(g) {
    sub <- data[as.character(data[[group_col]]) == g, , drop = FALSE]
    list(
      name = g,
      type = "dumbbell",
      data = build_dumbbell_point_data(sub, x_col, low_col, high_col)
    )
  })
}
