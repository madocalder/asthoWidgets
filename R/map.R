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
