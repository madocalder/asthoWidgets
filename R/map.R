#' Create an empty highchart map object into which data layers can be added
#'
#' This includes functionality for exporting a map as an image
#'
#' @param   hc_thm   Highchart theme object, as would be used in `hc_add_theme()`.
#' @param   export_options   List. Options for controlling features/presentation of the image
#'   download interface for the maps. See `viz_export()`.
#' @return   `highchart` htmlwidget
#' @export

create_base_map <- function(hc_thm, export_options = list()) {
  highcharter::highchart(
    type = "map"
  ) |>
    highcharter::hc_add_theme(
      hc_thm = hc_thm
    ) |>
    viz_export(
      export_options
    )
}

#' Add shapefile data, and region-specific values to make Choropleth using highchartr
#'
#' Each of the `*_options` arguments should be a list. These lists override default values for
#' various parts of the highchart object that is returned. There are some entries that must be
#' present, and a number of entries that can be absent (if absent they assume default values).
#' See the individual parameter entries.
#'
#' @param   hc   A highchart object.
#' @param   map_data   Data.frame. Must have entries 'id', 'name', 'path', 'value'.
#' @param   data_series_options Optional: 'borderColor', 'borderWidth', 'dataLabels'.
#' @param   color_options Essential: 'dataClasses', 'colors'. Optional: 'dataClassColor'.
#' @param   plot_options Optional: 'clickEvent'.
#' @param   title_options Essential: 'title'. Optional: 'margin', 'widthAdjust'.
#' @param   subtitle_options Essential: 'subtitle'.
#' @param   caption_options Essential: 'text'. Optional: 'useHTML', 'margin'.
#' @param   tooltip_options Essential: 'pointFormat'. Optional: 'headerFormat', 'useHTML'.
#' @param   legend_options Optional: 'reversed', 'enabled', 'layout', 'align', 'verticalAlign',
#'   'itemMarginBottom', 'itemMarginTop', 'margin', 'titleText'.
#'
#' @return   A highchart object.
#' @export

add_data_layers_to_map <- function(
    hc,
    map_data,
    data_series_options = list(),
    color_options = list(),
    plot_options = list(),
    title_options = list(),
    subtitle_options = list(),
    caption_options = list(),
    tooltip_options = list(),
    legend_options = list()
) {
  # An attempt was made to replace this with highcharter::highchartProxy() and to use data-updating
  # The data-layers in the map were not updated via the proxy though

  # Page-specific aspects of maps
  hc <- hc |>
    highcharter::hc_title(
      text = title_options$title,
      margin = title_options$margin %||% 10,
      widthAdjust = title_options$widthAdjust %||% -60
    ) |>
    highcharter::hc_tooltip(
      pointFormat = tooltip_options$pointFormat,
      headerFormat = c(
        tooltip_options$headerFormat %||% "<span style=\"color:{point.color}\">\u25CF</span> {point.JurisdictionName}"
      ),
      useHTML = tooltip_options$useHTML %||% TRUE
    )

  # Data-column-specific aspects of the map
  hc <- hc |>
    highcharter::hc_colorAxis(
      dataClasses = color_options$dataClasses,
      dataClassColor = color_options$dataClassColor %||% "category"
    ) |>
    highcharter::hc_colors(
      colors = color_options$colors
    ) |>
    highcharter::hc_plotOptions(
      series = list(
        events = list(
          click = plot_options$clickEvent %||% ""
        )
      )
    ) |>
    highcharter::hc_subtitle(
      text = subtitle_options$subtitle # text under title
    ) |>
    highcharter::hc_caption(
      text = caption_options$text, # source and data notes
      useHTML = caption_options$useHTML %||% TRUE,
      margin = caption_options$margin %||% 20
    ) |>
    highcharter::hc_legend(
      reversed = legend_options$reversed %||% FALSE,
      enabled = legend_options$enabled %||% TRUE,
      layout = legend_options$layout %||% "horizontal",
      align = legend_options$align %||% "center",
      verticalAlign = legend_options$verticalAlign %||% "bottom",
      itemMarginBottom = legend_options$itemMarginBottom %||% 3,
      itemMarginTop = legend_options$itemMarginTop %||% 3,
      margin = legend_options$margin %||% 22,
      title = list(
        text = legend_options$titleText %||% "Category"
      )
    ) |>
    highcharter::hc_add_series(
      data = map_data,
      borderColor = data_series_options$borderColor %||% "#020202",
      borderWidth = data_series_options$borderWidth %||% 0.25,
      dataLabels = data_series_options$dataLabels %||% list(
        enabled = FALSE,
        format = "{point.state}"
      )
    )

  return(hc)
}
