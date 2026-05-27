# Shared internal helpers used across the chart-builder family
# (chart_pie.R, chart_column.R, chart_line.R, chart_bubble.R,
# chart_wordcloud.R). All helpers below are unexported; they exist
# purely so each `add_*_chart()` function reads as a thin wrapper around
# the highcharter calls and the option-list defaults.

#' Return \code{x} when it is a non-empty string, else \code{""}.
#'
#' Highcharts shows its built-in \code{Chart title} placeholder whenever
#' \code{title.text} is missing from the options blob. Passing
#' \code{NULL} from R causes \code{highcharter::hc_title()} to drop the
#' field via \code{modifyList}, which triggers exactly that placeholder.
#' Passing the empty string instead serialises as \code{{"text": ""}}
#' and renders no title text. Callers can therefore use \code{title: ""}
#' in YAML to mean "no title".
#' @noRd
nz_or_null <- function(x) {
  if (is.null(x)) return("")
  if (length(x) != 1) return(x)
  if (!nzchar(x)) "" else x
}

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
#'
#' Keeps every column in `data` so tooltip templates can reference them
#' via `{point.<col>}`.
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

#' Local null-coalescing infix.
#'
#' Defined once at package level (rather than per-source-file) so all
#' chart helpers share one definition. Used pervasively in
#' \code{add_*_chart()} to fall back from a user-supplied option to a
#' sensible default.
#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a

#' Attach a chart-load handler that grows the chart container only when
#' a long legend genuinely exceeds its visual budget.
#'
#' The hook compares the legend's actual rendered height against 15% of
#' the originally allocated chart height. If the legend is over budget,
#' the parent DOM node's \code{style.height} is bumped by the excess
#' and Highcharts is told to \code{setSize} so the redraw fills the new
#' space. Charts with short legends (or none at all) are left at the
#' size the caller requested - this fixes the previous behaviour where
#' every chart grew to fit a fictional 85% plot-area target even when
#' there was no legend pressure.
#'
#' Calls \code{highcharter::hc_chart()} which merges into existing
#' chart options - safe to call alongside earlier \code{hc_chart()}
#' invocations in the same pipe.
#' @noRd
add_grow_for_legend_hook <- function(hc) {
  highcharter::hc_chart(
    hc,
    events = list(
      load = htmlwidgets::JS(
        "function() {",
        "  var chart = this;",
        "  if (chart._sizedForLegend) return;",
        "  if (typeof chart._origChartHeight === 'undefined') {",
        "    chart._origChartHeight = chart.chartHeight;",
        "  }",
        "  var legend = chart.legend;",
        "  var legendGroup = legend && legend.group;",
        "  if (!legendGroup) return;",
        "  var legendH = legendGroup.getBBox().height;",
        "  if (!legendH) return;",
        "  var budget = Math.floor(chart._origChartHeight * 0.15);",
        "  if (legendH <= budget) return;",
        "  var overflow = legendH - budget;",
        "  var newHeight = chart._origChartHeight + overflow;",
        "  chart._sizedForLegend = true;",
        "  var parent = chart.container && chart.container.parentNode;",
        "  if (parent) { parent.style.height = newHeight + 'px'; }",
        "  chart.setSize(undefined, newHeight, false);",
        "}"
      )
    )
  )
}
