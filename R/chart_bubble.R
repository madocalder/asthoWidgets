# nolint start: line_length_linter

#' Add a bubble series to a base chart
#'
#' Each row becomes one bubble. \code{x_col} and \code{y_col} position
#' the bubble; \code{z_col} sets the bubble size (defaults to
#' \code{y_col} if not supplied). Supplying \code{group_col} splits the
#' data into one series per unique value, so the colour palette can
#' encode a categorical variable.
#'
#' @param   hc   A \code{highchart} object, typically from
#'   \code{create_base_chart(type = "bubble")}.
#' @param   data   A data.frame with at least \code{x_col} and
#'   \code{y_col}.
#' @param   x_col,y_col   Column names (strings) for the bubble's
#'   horizontal and vertical position.
#' @param   z_col   Optional column name for the bubble size. Defaults
#'   to \code{y_col} so the largest values become the largest bubbles.
#' @param   group_col   Optional column name. When supplied, one series
#'   is rendered per unique value.
#' @param   bubble_options   Optional. Recognised entries: \code{minSize}
#'   (default \code{"15"}), \code{maxSize} (default \code{"60"}),
#'   \code{dataLabels} (full Highcharts dataLabels list).
#' @param   xAxis_options,yAxis_options,title_options,subtitle_options,tooltip_options,legend_options,caption_options
#'   Same shape as \code{add_column_chart()}.
#' @return   A \code{highchart} object.
#' @export

# nolint end

add_bubble_chart <- function(hc,
                             data,
                             x_col,
                             y_col,
                             z_col = NULL,
                             group_col = NULL,
                             bubble_options = list(),
                             xAxis_options = list(), # nolint: object_name_linter
                             yAxis_options = list(), # nolint: object_name_linter
                             title_options = list(),
                             subtitle_options = list(),
                             tooltip_options = list(),
                             legend_options = list(),
                             caption_options = list()) {
  z_col <- z_col %||% y_col
  validate_columns(data, c(x_col, y_col, z_col, group_col), "add_bubble_chart") # nolint: object_usage_linter
  has_groups <- !is.null(group_col)

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
      type = xAxis_options$type %||% "category",
      categories = xAxis_options$categories,
      title = list(text = xAxis_options$title %||% NULL),
      labels = labels_with_style(xAxis_options$labels) # nolint: object_usage_linter
    ) |>
    highcharter::hc_yAxis(
      title = list(text = yAxis_options$title %||% NULL),
      min = yAxis_options$min,
      max = yAxis_options$max,
      labels = labels_with_style(yAxis_options$labels)
    ) |>
    highcharter::hc_plotOptions(
      bubble = list(
        minSize = bubble_options$minSize %||% 15,
        maxSize = bubble_options$maxSize %||% 60,
        dataLabels = bubble_options$dataLabels %||% list(enabled = FALSE)
      )
    ) |>
    highcharter::hc_tooltip(
      pointFormat = tooltip_options$pointFormat %||%
        "<b>{point.name}</b><br>{point.y}",
      headerFormat = tooltip_options$headerFormat %||% "",
      useHTML = tooltip_options$useHTML %||% TRUE
    ) |>
    highcharter::hc_legend(
      enabled = legend_options$enabled %||% has_groups,
      layout = legend_options$layout %||% "horizontal",
      align = legend_options$align %||% "center",
      verticalAlign = legend_options$verticalAlign %||% "bottom",
      title = list(text = legend_options$titleText %||% NULL)
    )

  if (has_groups) {
    series <- lapply(unique(as.character(data[[group_col]])), function(g) {
      sub <- data[as.character(data[[group_col]]) == g, , drop = FALSE]
      list(
        name = g,
        type = "bubble",
        data = bubble_point_data(sub, x_col, y_col, z_col)
      )
    })
    hc |> highcharter::hc_add_series_list(series)
  } else {
    hc |> highcharter::hc_add_series(
      type = "bubble",
      name = legend_options$titleText %||% y_col,
      data = bubble_point_data(data, x_col, y_col, z_col)
    )
  }
}

#' Build per-point list for a bubble series.
#'
#' Renames \code{x_col}, \code{y_col}, and \code{z_col} to canonical
#' \code{name} (x), \code{y}, and \code{z} while keeping all other
#' columns for tooltip access.
#' @noRd
bubble_point_data <- function(data, x_col, y_col, z_col) {
  data$name <- data[[x_col]]
  data$y    <- data[[y_col]]
  data$z    <- data[[z_col]]
  highcharter::list_parse(data)
}
