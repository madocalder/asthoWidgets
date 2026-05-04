#' ASTHO data-visualisation colour palette
#'
#' Named hex values used across all asthoWidgets charts.
#'
#' \code{main_blue} (\code{#005182}) and \code{main_yellow} (\code{#EBB41F})
#' match the ASTHO bootstrap brand primary/warning colours; if those brand
#' values change, update them here as well.
#'
#' @format A named list of hex colour strings.
#' @export
dataviz_colors <- list(
  dark_accent  = "#242c3d",
  dark_gray    = "#666",
  dark_orange  = "#90361F",
  icon_accent  = "#47BA83",
  light_accent = "#cae6f2",
  light_blue   = "#70C4E8",
  light_yellow = "#FFD780",
  main_blue    = "#005182",
  main_orange  = "#C65227",
  main_yellow  = "#EBB41F",
  na_color     = "#E6E1E7"
)

#' ASTHO data-visualisation colour palettes
#'
#' Named character vectors of hex colours derived from \code{dataviz_colors},
#' suitable for passing to \code{highcharter::hc_colors()} or as the
#' \code{colors} entry in the \code{color_options} list accepted by
#' \code{add_data_layers_to_map()}.
#'
#' @format A named list of character vectors of hex colour strings.
#' @export
dataviz_palettes <- list(
  gov_blues_3       = with(dataviz_colors, c(light_accent, light_blue, main_blue)),
  gov_color_5       = with(dataviz_colors, c(light_blue, main_orange, main_blue, icon_accent, main_yellow)),
  gov_oranges_4     = with(dataviz_colors, c(light_yellow, main_yellow, main_orange, dark_orange)),
  gov_qualitative_3 = with(dataviz_colors, c(na_color, main_orange, icon_accent)),
  main              = with(dataviz_colors, c(light_blue, main_orange, main_yellow, icon_accent, main_blue))
)

#' ASTHO Highcharter theme
#'
#' Returns a \code{highcharter::hc_theme()} object configured with the ASTHO
#' brand fonts and colours from \code{dataviz_colors} and
#' \code{dataviz_palettes}. Pass the returned theme to
#' \code{create_base_map()} (or \code{highcharter::hc_add_theme()} for charts
#' built outside the helpers in this package) so every chart in the app
#' shares one visual language.
#'
#' @return A \code{hc_theme} object.
#' @export
get_astho_hc_theme <- function() {
  defaults <- list(
    fontFamily = "Jost",
    color = dataviz_colors$dark_accent
  )

  axis_theme <- list(
    labels = list(
      style = c(
        defaults["fontFamily"],
        list(
          color = dataviz_colors$dark_gray,
          fontSize = "15px",
          fontWeight = "normal"
        )
      )
    ),
    title = list(
      style = c(
        defaults[c("fontFamily", "color")],
        list(
          fontSize = "15px",
          fontWeight = "500"
        )
      )
    )
  )

  legend_theme <- list(
    itemStyle = c(
      defaults[c("fontFamily", "color")],
      list(
        fontSize = "17px",
        fontWeight = "normal"
      )
    ),
    title = list(
      style = c(
        defaults["fontFamily"],
        list(
          textDecoration = "underline",
          fontSize = "16px"
        )
      )
    )
  )

  highcharter::hc_theme(
    colors = dataviz_palettes[["main"]],
    chart = list(
      backgroundColor = NULL
    ),
    style = defaults["fontFamily"],
    title = list(
      style = c(
        defaults[c("fontFamily", "color")],
        list(
          fontWeight = "bold",
          fontSize = "18px"
        )
      )
    ),
    subtitle = list(
      style = c(
        defaults[c("fontFamily", "color")],
        list(
          fontSize = "14px"
        )
      )
    ),
    caption = list(
      style = c(
        defaults["fontFamily"],
        list(
          color = dataviz_colors$dark_gray,
          fontSize = "13px"
        )
      )
    ),
    xAxis = axis_theme,
    yAxis = axis_theme,
    legend = legend_theme,
    tooltip = list(
      padding = 10,
      borderRadius = 20,
      backgroundColor = "#fff",
      style = c(
        defaults["fontFamily"],
        list(
          fontSize = "14px",
          maxWidth = "50%",
          whiteSpace = "normal"
        )
      )
    ),
    useHTML = TRUE,
    itemHoverStyle = list(
      color = dataviz_colors$light_accent
    )
  )
}
