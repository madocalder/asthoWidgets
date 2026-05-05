#' Add a dumbbell series to a base chart
#'
#' Each row of `data` becomes one dumbbell. `low_col` is the lower endpoint,
#' `high_col` the upper. Supplying `group_col` produces one Highcharts series
#' per unique value (each rendered with its own colour from the theme palette).
#'
#' @param   hc   A \code{highchart} object, typically from
#'   \code{create_base_chart(type = "dumbbell")}.
#' @param   data   A data.frame with at least \code{x_col}, \code{low_col},
#'   and \code{high_col}; include \code{group_col} for grouped layouts.
#' @param   x_col   Column name (string) for the dumbbell category.
#' @param   low_col   Column name for the low endpoint.
#' @param   high_col  Column name for the high endpoint.
#' @param   group_col   Optional column name. When supplied, data is split
#'   into one series per unique value.
#' @param   dumbbell_options   Optional. Recognised: \code{markerRadius}
#'   (default 6), \code{connectorWidth} (default 3).
#' @param   xAxis_options,yAxis_options,title_options,subtitle_options,tooltip_options,legend_options,caption_options
#'   Same shape as \code{add_column_chart()}.
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

  build_points <- function(df) {
    lapply(seq_len(nrow(df)), function(i) {
      list(
        name = as.character(df[[x_col]][i]),
        low  = df[[low_col]][i],
        high = df[[high_col]][i]
      )
    })
  }

  hc <- hc |>
    highcharter::hc_chart(type = "dumbbell", inverted = TRUE) |>
    highcharter::hc_title(
      text        = nz_or_null(title_options$title),
      margin      = title_options$margin %||% 10,
      widthAdjust = title_options$widthAdjust %||% -60
    ) |>
    highcharter::hc_subtitle(
      text          = nz_or_null(subtitle_options$subtitle),
      useHTML       = subtitle_options$useHTML %||% TRUE,
      verticalAlign = subtitle_options$verticalAlign %||% "bottom",
      y             = subtitle_options$y %||% 30,
      x             = subtitle_options$x %||% 0
    ) |>
    highcharter::hc_caption(
      text    = caption_options$text,
      useHTML = caption_options$useHTML %||% TRUE,
      margin  = caption_options$margin %||% 20
    ) |>
    highcharter::hc_xAxis(
      type       = "category",
      categories = categories,
      title      = list(text = xAxis_options$title %||% NULL)
    ) |>
    highcharter::hc_yAxis(
      title  = list(text = yAxis_options$title %||% NULL),
      min    = yAxis_options$min %||% NULL,
      max    = yAxis_options$max %||% NULL,
      labels = labels_with_style(yAxis_options$labels)
    ) |>
    highcharter::hc_plotOptions(
      dumbbell = list(
        marker         = list(radius = dumbbell_options$markerRadius %||% 6),
        connectorWidth = dumbbell_options$connectorWidth %||% 3
      )
    ) |>
    highcharter::hc_tooltip(
      pointFormat = tooltip_options$pointFormat %||%
        "<b>{point.name}</b><br>Low: {point.low}<br>High: {point.high}",
      useHTML = tooltip_options$useHTML %||% TRUE
    ) |>
    highcharter::hc_legend(
      enabled       = legend_options$enabled %||% has_groups,
      layout        = legend_options$layout %||% "horizontal",
      align         = legend_options$align %||% "center",
      verticalAlign = legend_options$verticalAlign %||% "bottom"
    )

  if (has_groups) {
    for (g in unique(data[[group_col]])) {
      sub <- data[data[[group_col]] == g, , drop = FALSE]
      hc <- highcharter::hc_add_series(hc, name = as.character(g),
                                       data = build_points(sub))
    }
  } else {
    hc <- highcharter::hc_add_series(hc, name = nz_or_null(title_options$title),
                                     data = build_points(data))
  }

  hc
}
