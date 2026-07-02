# Highcharts pattern-fill colour objects for map and chart series.
#
# Each shape carries its background colour, so a patterned series keeps its
# palette colour with a texture over it. The objects use the
# modules/pattern-fill.js colour syntax and work anywhere Highcharts takes a
# colour: series color, colorAxis dataClass, per-point color.

#' Shape geometry and styling rules for the ASTHO pattern set.
#'
#' `style` says whether the path is drawn as a filled shape or an outline;
#' `shade` says whether the drawing colour derives from the background by
#' darkening or lightening (the wave motif only reads well with a lighter
#' foreground).
#' @noRd
pattern_shape_defs <- function() {
  list(
    chainlink = list(
      width = 40, height = 40, transform = "scale(.75)",
      style = "fill", shade = "darken",
      d = paste0(
        "M4 0v2.667h13.333V0zm0 2.667H0V16h4zM4 16v6.667h13.333V16zm13.333 ",
        "6.667V36H24V22.667h-4zm6.667 0h13.333V16H24ZM37.333 16H40V2.667h-2.",
        "667zm0-13.333V0H24v2.667ZM24 36v4h13.333v-4zm-6.667 0H4v4h13.333z"
      )
    ),
    triangles = list(
      width = 75, height = 150, transform = "scale(.5)",
      style = "fill", shade = "darken",
      d = paste0(
        "M37.5 0 75 25V0zm0 0v50.02l18.78-12.5zm0 0L0 25l18.78 12.51zM18.78 ",
        "37.51 0 74.97l37.5-24.95-18.72-12.5zm18.72 12.5v24.96H75L37.5 50.02",
        "zM75 74.98V25.01L56.28 37.5 75 74.97zm0 0-37.5 25 18.78 12.5L75 74.",
        "99zM56.28 112.5 37.5 150 75 125l-18.72-12.51zM37.5 150 0 125v25zm0 ",
        "0V99.98l-18.72 12.5zm-18.72-37.51L0 74.97V125l18.78-12.5zM0 74.97l",
        "37.5 25.01v-25H0z"
      )
    ),
    parquet = list(
      width = 60, height = 60, transform = "scale(.75)",
      style = "stroke", strokeWidth = 1.5, shade = "darken",
      d = paste0(
        "M10 60V30m10 0v30m10 0H0V30M50 0v30m-10 0V0M30 0h30v30M30 40h30m0 ",
        "10H30m0-20h30v30H30zM0 10h30m0 10H0M0 0h30v30H0z"
      )
    ),
    honeycomb = list(
      width = 29, height = 50.115, transform = "scale(.65)",
      style = "stroke", strokeWidth = 3, shade = "darken",
      d = paste0(
        "M14.499 11.82 4.36 5.968l.002-11.706 10.14-5.855L24.638-5.74l-.001 ",
        "11.707zm0 50.06L4.36 56.029l.002-11.706 10.14-5.855 10.137 5.852-.0",
        "01 11.707zm14.498-25.117L18.858 30.91l.002-11.707L29 13.349l10.137 ",
        "5.853-.001 11.706zm-29 0-10.139-5.852.002-11.707L0 13.349l10.138 5.",
        "853-.002 11.706zm14.501-19.905L0 8.488.002-8.257l14.5-8.374L29-8.26",
        "l-.002 16.745zm0 50.06L0 58.548l.002-16.745 14.5-8.373L29 41.8l-.00",
        "2 16.744zM28.996 41.8l-14.498-8.37.002-16.744L29 8.312l14.498 8.37-",
        ".002 16.745zm-29 0-14.498-8.37.002-16.744L0 8.312l14.498 8.37-.002 ",
        "16.745z"
      )
    ),
    wave = list(
      width = 56.915, height = 30, transform = "scale(1.75)",
      style = "fill", shade = "lighten",
      d = paste0(
        "M10.023 0c1.263 1.051 2.418 2.246 3.592 3.462 1.874 1.944 3.808 3.",
        "938 6.287 5.404-.94.552-1.8 1.18-2.606 1.856-.844-.785-1.66-1.625-2",
        ".452-2.444C11.22 4.525 7.476.646 0 .645v1.71c6.752.001 10.089 3.451",
        " 13.615 7.107.771.8 1.568 1.619 2.397 2.401a62 62 0 0 0-1.785 1.776",
        "C10.785 10.099 7.056 6.646 0 6.645v1.708c6.38.002 9.706 3.085 13.03",
        "8 6.513a51 51 0 0 1-1.878 1.86C8.773 14.73 5.373 12.646 0 12.646v1.",
        "707c4.679.001 7.63 1.687 9.86 3.514-.97.793-2.009 1.5-3.173 2.066C4",
        ".652 19.07 2.46 18.646 0 18.646v1.706c1.494 0 2.872.171 4.17.512-1.",
        "24.332-2.61.517-4.17.517v1.71c7.477-.001 11.22-3.881 14.842-7.63 3.",
        "527-3.654 6.864-7.106 13.615-7.106s10.084 3.452 13.612 7.106c3.622 ",
        "3.75 7.363 7.63 14.842 7.63h.004v-1.71h-.006c-1.56 0-2.932-.186-4.1",
        "71-.517 1.294-.34 2.675-.512 4.17-.512h.007v-1.706h-.004c-2.466 0-4",
        ".654.427-6.686 1.287-1.164-.567-2.206-1.273-3.175-2.066 2.23-1.827 ",
        "5.182-3.514 9.86-3.514h.005v-1.708h-.004c-5.375 0-8.777 2.084-11.16",
        " 4.081a50 50 0 0 1-1.88-1.86c3.33-3.425 6.657-6.513 13.04-6.513h.00",
        "4V6.647h-.004c-7.052 0-10.785 3.449-14.23 6.99a54 54 0 0 0-1.786-1.",
        "774 73 73 0 0 0 2.397-2.4c3.528-3.658 6.864-7.108 13.619-7.108h.004",
        "V.645c-7.479 0-11.225 3.88-14.848 7.633-.793.819-1.606 1.66-2.45 2.",
        "444a19.4 19.4 0 0 0-2.612-1.86c2.482-1.461 4.415-3.46 6.293-5.404C4",
        "4.472 2.243 45.628 1.051 46.89 0h-2.564a56 56 0 0 0-1.644 1.638A57 ",
        "57 0 0 0 41.04 0h-2.563c1.058.878 2.037 1.854 3.017 2.865a57 57 0 0",
        " 1-1.877 1.864C37.23 2.732 33.83.647 28.457.647c-5.375 0-8.776 2.08",
        "5-11.163 4.082a58 58 0 0 1-1.879-1.864c.98-1.01 1.957-1.988 3.016-2",
        ".865H15.87a56 56 0 0 0-1.642 1.638A58 58 0 0 0 12.583 0zm18.432 2.3",
        "55c4.678 0 7.63 1.684 9.86 3.511-.967.79-2.003 1.49-3.167 2.061-1.8",
        "71-.796-4.05-1.281-6.693-1.282-2.65 0-4.825.486-6.696 1.282-1.164-.",
        "567-2.198-1.272-3.165-2.057 2.23-1.83 5.18-3.515 9.861-3.515m.002 1",
        "0.29c-7.479 0-11.224 3.879-14.847 7.628-2.134 2.213-4.16 4.306-6.91",
        "6 5.651a15.8 15.8 0 0 0-3.792-1.063l-.134-.022q-.406-.061-.827-.101",
        "l-.143-.011a31 31 0 0 0-.703-.052l-.234-.009A17 17 0 0 0 0 24.644v1",
        ".708q.393.001.775.019l.211.01q.318.018.636.045c.041.004.089.005.13",
        ".009q.374.036.737.088.07.014.143.024.333.05.655.116l.083.014q.37.0",
        "79.735.171l.053.017q.753.197 1.466.475h.007a13.4 13.4 0 0 1 1.789.8",
        "47h.004c.864.484 1.71 1.079 2.591 1.813h2.568q-.072-.068-.141-.136c",
        ".833-.782 1.624-1.603 2.396-2.402 3.531-3.657 6.868-7.108 13.62-7.1",
        "08 6.75 0 10.083 3.453 13.61 7.106a70 70 0 0 0 2.401 2.408q-.074.06",
        "7-.141.132h2.562c2.534-2.11 5.516-3.646 10.02-3.646h.005v-1.71h-.00",
        "2c-2.646 0-4.825.489-6.697 1.28-2.756-1.349-4.781-3.438-6.918-5.651",
        "-3.62-3.752-7.366-7.628-14.84-7.628zm-.002 1.708c6.751 0 10.084 3.4",
        "53 13.616 7.107 1.875 1.942 3.806 3.94 6.288 5.405-.938.554-1.8 1.1",
        "82-2.608 1.86-.847-.788-1.664-1.632-2.455-2.452-3.62-3.749-7.366-7.",
        "63-14.84-7.63-7.478 0-11.225 3.881-14.845 7.63a62 62 0 0 1-2.455 2.",
        "449 19.3 19.3 0 0 0-2.606-1.857c2.478-1.465 4.411-3.46 6.287-5.404 ",
        "3.53-3.657 6.864-7.108 13.618-7.108m-.001 10.291c-5.953 0-9.538 2.4",
        "6-12.581 5.356h2.556c2.534-2.11 5.52-3.648 10.027-3.648 4.504 0 7.4",
        "85 1.538 10.018 3.648h2.56c-3.038-2.895-6.628-5.356-12.58-5.356"
      )
    ),
    na = list(
      width = 31.25, height = 20, transform = "scale(.2)",
      style = "fill", shade = "darken",
      d = paste0(
        "M.162 1.593c-.054 0-.108.008-.162.009v8.58c.055-.002.107-.017.163-",
        ".017 1.395 0 2.68.567 3.62 1.598l2.049 2.248a13.5 13.5 0 0 0 9.954 ",
        "4.396c3.78 0 7.408-1.603 9.954-4.396l2.051-2.25a4.83 4.83 0 0 1 3.4",
        "59-1.581V1.6a13.5 13.5 0 0 0-9.794 4.388l-2.05 2.25a4.84 4.84 0 0 1",
        "-3.62 1.597 4.84 4.84 0 0 1-3.619-1.598L10.118 5.99A13.5 13.5 0 0 0",
        " .162 1.593"
      )
    )
  )
}

#' Blend a colour toward black (darken) or white (lighten).
#' @noRd
shade_color <- function(hex, direction, amount) {
  target <- if (direction == "darken") "#000000" else "#FFFFFF"
  grDevices::colorRampPalette(c(hex, target))(101)[round(amount * 100) + 1]
}

#' Build a Highcharts pattern-fill colour object
#'
#' Returns a pattern object usable anywhere Highcharts accepts a colour once
#' the \code{modules/pattern-fill.js} module is loaded (it is loaded by
#' \code{\link{create_base_map}} and \code{\link{create_base_chart}}). The
#' pattern draws the shape's motif over \code{background}, in a foreground
#' shade derived from it, so toggling from a solid fill to a pattern keeps
#' the category's colour identity.
#'
#' @param   shape   One of \code{pattern_fill_shapes()}, or \code{"na"} for
#'   the missing-data motif.
#' @param   background   Background colour (hex string), normally the
#'   category's solid palette colour.
#' @param   foreground   Optional drawing colour. Defaults to a darker shade
#'   of \code{background} (lighter for \code{"wave"}).
#' @return   A list of the form \code{list(pattern = list(...))}.
#' @export
pattern_fill <- function(shape, background, foreground = NULL) {
  def <- pattern_shape_defs()[[shape]]
  if (is.null(def)) {
    stop(
      "pattern_fill(): unknown shape '", shape, "'. Available: ",
      paste(names(pattern_shape_defs()), collapse = ", ")
    )
  }
  if (is.null(foreground)) {
    foreground <- if (def$shade == "lighten") {
      shade_color(background, "lighten", 0.45)
    } else {
      shade_color(background, "darken", 0.3)
    }
  }
  path <- if (def$style == "stroke") {
    list(d = def$d, stroke = foreground, strokeWidth = def$strokeWidth,
         fill = "none")
  } else {
    list(d = def$d, fill = foreground, stroke = "none")
  }
  list(pattern = list(
    width = def$width,
    height = def$height,
    patternUnits = "userSpaceOnUse",
    patternTransform = def$transform,
    backgroundColor = background,
    path = path,
    x = 0,
    y = 0
  ))
}

#' Shape names available for category patterns
#'
#' The cycle order interleaves visually distinct motifs; \code{"na"} is
#' excluded because it is reserved for missing-data categories.
#'
#' @return   Character vector of shape names accepted by
#'   \code{\link{pattern_fill}}.
#' @export
pattern_fill_shapes <- function() {
  c("chainlink", "triangles", "parquet", "honeycomb", "wave")
}

#' Assign a colours vector to a chart
#'
#' Like \code{highcharter::hc_colors()}, but list inputs (pattern objects
#' from the pattern hook) are assigned as-is: \code{hc_colors()} re-wraps
#' any length-1 input in a list, which would serialise a single pattern
#' object as \code{[[{...}]]}.
#' @noRd
set_chart_colors <- function(hc, colors) {
  if (is.list(colors)) {
    hc$x$hc_opts$colors <- colors
    return(hc)
  }
  highcharter::hc_colors(hc, colors)
}

#' Run chart colours through the host app's pattern hook
#'
#' The chart and map builders pass their colours (theme palette, explicit
#' pie slice colours, map class colours) through the function registered as
#' \code{options(asthoWidgets.pattern_colors = ...)}. The hook decides
#' whether to substitute pattern objects (e.g. only while the app's
#' pattern-fill toggle is on) and must pass through entries that are
#' already pattern objects. With no hook registered, colours are returned
#' unchanged.
#' @noRd
apply_pattern_colors <- function(colors) {
  hook <- getOption("asthoWidgets.pattern_colors")
  if (is.null(colors) || !is.function(hook)) return(colors)
  hook(colors)
}
