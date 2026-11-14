#' Add features for downloading figures to a highchart graphic
#'
#' @param   hc   A highchart object
#' @param   export_options   List of options for controlling how exporting works
#' @export
viz_export <- function(hc, export_options = list()) {
  hc <- highcharter::hc_exporting(
    hc,
    enabled = export_options$enabled %||% TRUE,
    accessibility = list(enabled = TRUE),
    formAttributes = list(target = "_blank"),
    sourceHeight = export_options$sourceHeight %||% 700,
    sourceWidth  = export_options$sourceWidth  %||% 1200,
    allowHTML    = export_options$allowHTML    %||% TRUE,
    buttons = list(
      contextButton = list(
        symbol            = export_options$contextButton$symbol            %||% "downloadIcon",
        symbolSize        = export_options$contextButton$symbolSize        %||% 32,
        symbolStrokeWidth = export_options$contextButton$symbolStrokeWidth %||% 0,
        symbolFill        = export_options$contextButton$symbolFill        %||% "#4679AE",
        symbolStroke      = export_options$contextButton$symbolStroke      %||% "none",
        menuItems = export_options$contextButton$menuItems %||% c(
          "downloadPNG", "downloadSVG", "downloadCSV", "downloadXLS", "openInCloud"
        )
      )
    ),
    chartOptions = list(
      chart = list(
        backgroundColor = export_options$chart$backgroundColor %||% "#FFFFFF"
      )
    )
  )

  attach_download_symbol(hc)
}
