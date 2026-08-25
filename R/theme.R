#' ASTHO data-visualisation color palette
#'
#' Named hex values used across all asthoWidgets charts.
#'
#' \code{astho_blue} (\code{#005182}) and \code{yellow_ochre} (\code{#EBB41F})
#' match the ASTHO bootstrap brand primary/warning colors; if those brand
#' values change, update them here as well.
#'
#' @format A named list of hex color strings.
#' @export
dataviz_colors <- list(

  # ASTHO brand colors
  astho_blue    = "#005182",
  astho_orange  = "#C05711",
  sky_blue      = "#56bde9",
  light_sage    = "#B6D094",
  pale_lilac    = "#E2CFEA",

  # Other main colors
  yellow_ochre  = "#EBB41F",
  teal_green    = "#35A584",
  burnt_umber   = "#90361F",

  # Neutrals
  dark_slate    = "#242c3d",
  medium_slate  = "#636b7f",
  na_color      = "#C4B8B2",

  # Shades and tints
  palest_blue   = "#CAE6F2"

)


#' ASTHO data-visualisation color palettes
#'
#' Named character vectors of hex colors derived from \code{dataviz_colors},
#' suitable for passing to \code{highcharter::hc_colors()} or as the
#' \code{colors} entry in the \code{color_options} list accepted by
#' \code{add_data_layers_to_map()}.
#'
#' @format A named list of character vectors of hex color strings.
#' @export
dataviz_palettes <- list(

  # continuous ----
  ## old name: gov_blues_3
  blues_3 = with(
    dataviz_colors,
    c(palest_blue, sky_blue, astho_blue)
  ),

  ## old name: gov_oranges_4
  warm_4 = with(
    dataviz_colors,
    c(light_sage, yellow_ochre, astho_orange, burnt_umber)
  ),

  # categorical ----
  ## old name: gov_qualitative_3
  cat_3 = with(
    dataviz_colors,
    c(na_color, astho_orange, sky_blue)
  ),

  ## old name: gov_color_5
  cat_5 = with(
    dataviz_colors,
    c(astho_blue, sky_blue, astho_orange, light_sage, teal_green)
  ),

  main = with(
    dataviz_colors,
    c(
      astho_blue, sky_blue, astho_orange, light_sage, teal_green,
      yellow_ochre, pale_lilac, burnt_umber
    )
  )

)

#' ASTHO Highcharter theme
#'
#' Returns a \code{highcharter::hc_theme()} object configured with the ASTHO
#' brand fonts and colors from \code{dataviz_colors} and
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
    color = dataviz_colors$dark_slate
  )

  axis_theme <- list(
    labels = list(
      style = c(
        defaults["fontFamily"],
        list(
          color = dataviz_colors$dark_slate,
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
          color = dataviz_colors$medium_slate,
          fontSize = "13px"
        )
      )
    ),
    xAxis = axis_theme,
    yAxis = axis_theme,
    legend = legend_theme,
    tooltip = list(
      # render outside the plot so narrow charts don't clip it
      outside = TRUE,
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
      color = dataviz_colors$palest_blue
    )
  )
}
