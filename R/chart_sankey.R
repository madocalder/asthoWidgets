#' Add a Sankey series to a base chart
#'
#' Each row of \code{data} becomes one Sankey flow: a link from
#' \code{from_col} to \code{to_col} with a weight from \code{weight_col}.
#' Node colors come from the theme palette.
#'
#' @param   hc   A \code{highchart} object, typically from
#'   \code{create_base_chart(type = "sankey")}.
#' @param   data   A data.frame with at least \code{from_col},
#'   \code{to_col}, and \code{weight_col}.
#' @param   from_col   Column name (string) for the source node.
#' @param   to_col   Column name (string) for the target node.
#' @param   weight_col   Column name (string) for the flow weight.
#' @param   sankey_options   Optional. Recognised entries:
#'   \code{nodeWidth} (default \code{20}), \code{nodePadding}
#'   (default \code{10}), \code{linkOpacity} (default \code{0.5}),
#'   \code{dataLabels} (full Highcharts dataLabels list).
#' @param   title_options   Essential: \code{title}.
#' @param   subtitle_options   Optional: \code{subtitle}.
#' @param   tooltip_options   Optional: \code{nodeFormat}, \code{pointFormat}.
#' @param   legend_options   Optional. Sankeys do not normally render a
#'   legend, but the option is accepted for parity with other widgets.
#' @param   caption_options   Optional: \code{text}, \code{useHTML},
#'   \code{margin}.
#' @return   A \code{highchart} object.
#' @export
add_sankey_chart <- function(hc,
                             data,
                             from_col,
                             to_col,
                             weight_col,
                             sankey_options = list(),
                             title_options = list(),
                             subtitle_options = list(),
                             tooltip_options = list(),
                             legend_options = list(),
                             caption_options = list()) {
  validate_columns(data, c(from_col, to_col, weight_col), "add_sankey_chart") # nolint: object_usage_linter

  series_data <- build_sankey_point_data(data, from_col, to_col, weight_col)

  hc |>
    highcharter::hc_chart(type = "sankey") |>
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
    highcharter::hc_plotOptions(
      sankey = list(
        nodeWidth = sankey_options$nodeWidth %||% 20,
        nodePadding = sankey_options$nodePadding %||% 10,
        linkOpacity = sankey_options$linkOpacity %||% 0.5,
        dataLabels = sankey_options$dataLabels %||% list(
          enabled = TRUE,
          style = list(fontFamily = "Jost", fontSize = "13px")
        )
      )
    ) |>
    highcharter::hc_tooltip(
      nodeFormat = tooltip_options$nodeFormat %||%
        "{point.name}: <b>{point.sum:,.0f}</b>",
      pointFormat = tooltip_options$pointFormat %||%
        "{point.fromNode.name} \u2192 {point.toNode.name}: <b>{point.weight:,.0f}</b>",
      useHTML = tooltip_options$useHTML %||% TRUE
    ) |>
    highcharter::hc_legend(
      enabled = legend_options$enabled %||% FALSE
    ) |>
    highcharter::hc_add_series(
      type = "sankey",
      name = legend_options$titleText %||% "Flow",
      data = series_data,
      keys = c("from", "to", "weight")
    )
}

#' Build the per-point list a Highcharts Sankey series expects.
#'
#' Highcharts Sankey accepts a flat list where each entry has
#' \code{from}, \code{to}, and \code{weight}. Any other column in
#' \code{data} is preserved for tooltip access via \code{point.<column>}.
#' @noRd
build_sankey_point_data <- function(data, from_col, to_col, weight_col) {
  data$from <- as.character(data[[from_col]])
  data$to <- as.character(data[[to_col]])
  data$weight <- as.numeric(data[[weight_col]])
  highcharter::list_parse(data)
}
