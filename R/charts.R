# Chart-builder family entry point.
#
# `create_base_chart()` produces a themed empty Highchart that the
# `add_*_chart()` helpers (one per chart type) decorate via the option-
# list pattern shared with `create_base_map()` / `add_data_layers_to_map()`.
#
# Per-chart-type implementations live in:
#   - chart_pie.R       (`add_pie_chart`)
#   - chart_column.R    (`add_column_chart`; horizontal-bar variant via
#                        `column_options$inverted = TRUE`)
#   - chart_line.R      (`add_line_chart`)
#   - chart_bubble.R    (`add_bubble_chart`)
#   - chart_wordcloud.R (`add_word_cloud`)
#
# Cross-cutting helpers (option-list defaults, column validation,
# series-data builders, the local `%||%`) live in `chart_helpers.R`.

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
