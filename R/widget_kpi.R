#' KPI card
#'
#' Renders a single key-performance-indicator card: a large numeric (or
#' string) value with an optional supporting label, icon, color band,
#' and a small footnote. Built on \code{bslib::value_box()} when
#' available so the visual style is consistent with other bslib cards
#' in the app.
#'
#' @param   value   The headline value. Numbers are formatted with
#'   thousand separators; pass a pre-formatted string (e.g. \code{"$1.2M"})
#'   to bypass formatting.
#' @param   label   Short label rendered above the value.
#' @param   sublabel   Optional smaller text rendered below the value.
#' @param   icon   Optional Font Awesome icon name (without the \code{fa-}
#'   prefix) for the card. Default \code{NULL}.
#' @param   color   Background color. Accepts one of \code{"primary"},
#'   \code{"secondary"}, \code{"success"}, \code{"info"}, \code{"warning"},
#'   or any hex color. Default \code{"primary"}.
#' @param   theme_color   Deprecated alias for \code{color}. Retained for
#'   forward compatibility with bslib API names.
#' @return   A Shiny tag list ready to drop into a UI.
#' @export
aw_kpi_card <- function(value,
                        label,
                        sublabel = NULL,
                        icon = NULL,
                        color = "primary",
                        theme_color = NULL) {
  if (!is.null(theme_color)) color <- theme_color
  resolved_color <- resolve_kpi_color(color)
  formatted_value <- format_kpi_value(value)
  icon_tag <- build_kpi_icon(icon)

  htmltools::tags$div(
    class = "aw-kpi-card",
    style = sprintf(
      paste0(
        "background-color: %s; color: #FFFFFF; padding: 1.25rem 1.5rem; ",
        "border-radius: 0.5rem; display: flex; flex-direction: column; ",
        "justify-content: space-between; height: 100%%; min-height: 9rem; ",
        "box-shadow: 0 1px 3px rgba(0,0,0,0.08);"
      ),
      resolved_color
    ),
    htmltools::tags$div(
      class = "aw-kpi-header",
      style =
        paste(
          "display: flex;",
          "align-items: center;",
          "gap: 0.5rem;",
          "opacity: 0.85;",
          "font-size: 0.95rem;",
          "font-weight: 500;"
        ),
      icon_tag,
      htmltools::tags$span(label)
    ),
    htmltools::tags$div(
      class = "aw-kpi-value",
      style = "font-size: 2.5rem; font-weight: 700; line-height: 1.1; margin-top: 0.5rem;",
      formatted_value
    ),
    if (!is.null(sublabel)) {
      htmltools::tags$div(
        class = "aw-kpi-sublabel",
        style = "font-size: 0.85rem; opacity: 0.85; margin-top: 0.25rem;",
        sublabel
      )
    }
  )
}

#' Grid of KPI cards
#'
#' Lays out a list of \code{aw_kpi_card()} calls in a responsive grid.
#' On small screens, cards stack; at \code{md} and above they sit
#' side-by-side, wrapping every \code{col_widths} cards.
#'
#' @param   ...   One or more \code{aw_kpi_card()} tags.
#' @param   col_widths   How many cards per row at \code{md} and above.
#'   Default \code{3}.
#' @return   A Shiny tag list.
#' @export
aw_kpi_grid <- function(..., col_widths = 3) {
  cards <- list(...)
  if (length(cards) == 0) return(htmltools::tags$div())

  per_row_width <- max(1, floor(12 / col_widths))
  col_class <- sprintf("col-12 col-md-%d", per_row_width)

  htmltools::tags$div(
    class = "row g-3 aw-kpi-grid",
    lapply(cards, function(card) {
      htmltools::tags$div(class = col_class, card)
    })
  )
}

#' Resolve a color key or hex string to a hex value.
#' @noRd
resolve_kpi_color <- function(color) {
  keyed <- list(
    primary = "#005182",
    secondary = "#C65227",
    success = "#47BA83",
    info = "#70C4E8",
    warning = "#EBB41F",
    dark = "#242C3D"
  )
  if (is.null(color)) return(keyed[["primary"]])
  if (color %in% names(keyed)) return(keyed[[color]])
  color
}

#' Format a KPI value for display.
#'
#' Numbers get thousands-separator formatting. Strings pass through.
#' @noRd
format_kpi_value <- function(value) {
  if (is.numeric(value)) {
    formatC(value, format = "d", big.mark = ",")
  } else {
    as.character(value)
  }
}

#' Build the icon tag for a KPI card, or NULL if no icon was supplied.
#' @noRd
build_kpi_icon <- function(icon) {
  if (is.null(icon) || !nzchar(icon)) return(NULL)
  htmltools::tags$i(class = paste0("fa fa-", icon), `aria-hidden` = "true")
}
