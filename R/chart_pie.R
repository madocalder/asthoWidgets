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
#' @param   size_options   Optional sizing controls. By default the donut keeps
#'   its fixed height but grows by \code{perItem} (default 26) px for each
#'   legend item beyond \code{baselineItems} (default 3), so a long category
#'   list gets room beneath the pie; \code{maxHeight} (default 700) clamps it.
#'   This is driven by the slice count known in R, so it is reliable for pies
#'   drawn with \code{suspendWhenHidden = FALSE}. Set \code{legendBump = FALSE}
#'   for a flat fixed height, or \code{fillWidth = TRUE} to instead size the
#'   plot to the container width (only sensible for an always-visible donut).
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
                          caption_options = list(),
                          size_options = list()) {
  series_data <- pie_series_data(data, x_col, y_col, z_col)

  hc |>
    highcharter::hc_chart(crop = FALSE) |>
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
    ) |>
    apply_pie_size_hook(size_options, n_items = nrow(data))
}

#' Legend-item height bump by default (see [add_legend_height_hook()]); opt into
#' width-filling or out of any sizing via `size_options` (see [add_pie_chart()]).
#' @noRd
apply_pie_size_hook <- function(hc, size_options = list(), n_items = NULL) {
  if (isTRUE(size_options$fillWidth)) {
    return(add_fill_width_hook(
      hc,
      min_height = size_options$minHeight %||% 260,
      max_height = size_options$maxHeight %||% 700
    ))
  }
  if (isFALSE(size_options$legendBump) || is.null(n_items)) {
    return(hc)
  }
  add_legend_height_hook(
    hc,
    n_items = n_items,
    per_item = size_options$perItem %||% 26,
    baseline_items = size_options$baselineItems %||% 3,
    max_height = size_options$maxHeight %||% 700
  )
}

#' Build the per-point data list a Highcharts pie series expects.
#'
#' Renames the user-selected x/y/z columns to Highcharts' canonical
#' \code{name}, \code{y}, \code{z} names while keeping all other columns
#' intact so tooltip templates can still reference them via
#' \code{{point.<column>}}.
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
