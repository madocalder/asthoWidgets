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

#' Size a chart so its plot area fills the container width.
#'
#' Sets the chart height to \code{plotWidth * fill * (drawnHeight / drawnWidth)
#' + chrome}, where chrome is the measured non-plot space. Measuring the legend
#' rather than guessing keeps it visible whatever its size; a ResizeObserver
#' re-fits on width changes.
#'
#' @param hc A highchart object.
#' @param min_height,max_height Pixel clamp for the derived chart height.
#' @param fill Fraction of the plot width the content should span (< 1 leaves
#'   horizontal padding).
#' @noRd
add_fill_width_hook <- function(hc,
                                min_height,
                                max_height,
                                fill = 1) {
  cfg <- sprintf("var minH = %s, maxH = %s, fill = %s;", min_height, max_height, fill)
  highcharter::hc_chart(
    hc,
    events = list(
      load = htmlwidgets::JS(
        "function() {",
        "  var chart = this;",
        "  if (!chart.container) return;",
        paste0("  ", cfg),
        # Resize every wrapper up to .html-widget-output, not just the inner
        # node, or the outer wrapper keeps its fixed height and clips the chart.
        "  var wrappers = [];",
        "  (function() {",
        "    var node = chart.container.parentNode, hops = 0;",
        "    while (node && hops < 6) {",
        "      wrappers.push(node);",
        "      if (node.classList && node.classList.contains('html-widget-output')) break;",
        "      node = node.parentNode; hops++;",
        "    }",
        "  })();",
        "  var host = wrappers[wrappers.length - 1];",
        "  if (!host) return;",
        "  function setHeights(px) { wrappers.forEach(function(n) { n.style.height = px + 'px'; }); }",
        # Drawn map/pie bounds -> its natural aspect ratio.
        "  function contentBox() {",
        "    var W = 0, H = 0;",
        "    (chart.series || []).forEach(function(s) {",
        "      if (!s || s.visible === false || !s.group) return;",
        "      try {",
        "        var bb = s.group.getBBox();",
        "        if (bb.width > W) { W = bb.width; }",
        "        if (bb.height > H) { H = bb.height; }",
        "      } catch (e) {}",
        "    });",
        "    return { w: W, h: H };",
        "  }",
        "  var retry = 0;",
        "  function fit() {",
        "    var w = host.offsetWidth;",
        "    if (!w) return;",
        "    if (Math.abs(w - chart.chartWidth) > 1) { chart.setSize(w, chart.chartHeight, false); }",
        "    var plotW = chart.plotWidth;",
        "    var box = contentBox();",
        # Revealed from a hidden tab: width is set but getBBox is still 0.
        # Retry next frame so a tab switch self-corrects without a resize.
        "    if (!plotW || !box.w || !box.h) {",
        "      if (retry < 10) { retry++; window.requestAnimationFrame(fit); }",
        "      return;",
        "    }",
        "    retry = 0;",
        "    var chrome = chart.chartHeight - chart.plotHeight;",
        "    var target = Math.round(plotW * fill * (box.h / box.w) + chrome);",
        "    if (target < minH) { target = minH; }",
        "    if (target > maxH) { target = maxH; }",
        "    if (Math.abs(target - chart.chartHeight) > 2) {",
        "      setHeights(target);",
        "      chart.setSize(w, target, false);",
        "    } else {",
        "      setHeights(chart.chartHeight);",
        "    }",
        "  }",
        "  fit();",
        "  if (window.ResizeObserver && !chart._awResizeObs) {",
        "    chart._awLastWidth = host.offsetWidth;",
        "    chart._awResizeObs = new ResizeObserver(function(entries) {",
        "      var cw = entries[0].contentRect.width;",
        "      if (Math.abs(cw - (chart._awLastWidth || 0)) < 1) return;",
        "      chart._awLastWidth = cw;",
        "      retry = 0;",
        "      fit();",
        "    });",
        "    chart._awResizeObs.observe(host);",
        "  }",
        "}"
      )
    )
  )
}

#' Grow a chart's height by its number of legend items.
#'
#' Adds \code{per_item} px for every item beyond \code{baseline_items} so a long
#' bottom legend gets room instead of squeezing the plot. The count comes from R
#' (not a render-time measurement), so it is reliable for outputs drawn with
#' \code{suspendWhenHidden = FALSE}.
#'
#' @param hc A highchart object.
#' @param n_items Number of legend items (slices).
#' @param per_item Pixels added per item beyond the baseline.
#' @param baseline_items Item count the caller's height already accommodates.
#' @param max_height Pixel clamp for the grown height.
#' @noRd
add_legend_height_hook <- function(hc,
                                   n_items,
                                   per_item,
                                   baseline_items,
                                   max_height) {
  extra <- max(0, n_items - baseline_items) * per_item
  if (extra <= 0) {
    return(hc)
  }
  cfg <- sprintf("var extra = %s, maxH = %s;", extra, max_height)
  highcharter::hc_chart(
    hc,
    events = list(
      load = htmlwidgets::JS(
        "function() {",
        "  var chart = this;",
        "  if (!chart.container) return;",
        paste0("  ", cfg),
        # Collect wrappers up to the .html-widget-output element so all of
        # them grow together and the outer container does not clip.
        "  var wrappers = [], host = null, node = chart.container.parentNode, hops = 0;",
        "  while (node && hops < 6) {",
        "    wrappers.push(node);",
        "    if (node.classList && node.classList.contains('html-widget-output')) { host = node; break; }",
        "    node = node.parentNode; hops++;",
        "  }",
        "  if (!host) { host = wrappers[wrappers.length - 1]; }",
        "  if (!host) return;",
        # Base height from the inline style set by *Output(height=); reliable
        # even while the tab is hidden (offsetHeight would be 0 there).
        "  var base = parseInt(host.style.height, 10);",
        "  if (!base || isNaN(base)) { base = chart.chartHeight || host.offsetHeight; }",
        "  if (!base) return;",
        "  var target = base + extra;",
        "  if (target > maxH) { target = maxH; }",
        "  wrappers.forEach(function(w) { w.style.height = target + 'px'; });",
        "  chart.setSize(undefined, target, false);",
        "}"
      )
    )
  )
}
