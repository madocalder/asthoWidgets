#' Title with hoverable definition popover
#'
#' Wraps a label with a small info icon that triggers a Bootstrap
#' popover showing a longer definition. Useful for chart titles or
#' selector labels where space is tight but the term needs a glossary.
#'
#' The popover relies on Bootstrap 5's tooltip / popover JavaScript,
#' which Shiny's bslib theme already loads. No extra dependencies.
#'
#' @param   label   The visible text.
#' @param   definition   The text shown when the icon is hovered or
#'   focused. Plain text is safest; HTML is allowed (Bootstrap will
#'   render it).
#' @param   title   Optional header for the popover. Defaults to
#'   \code{label}.
#' @param   icon_class   Font Awesome class for the trigger icon.
#'   Default \code{"fa-circle-info"}.
#' @return   A Shiny tag list.
#' @export
aw_definition_popover <- function(label,
                                  definition,
                                  title = NULL,
                                  icon_class = "fa-circle-info") {
  popover_title <- title %||% label

  htmltools::tags$span(
    class = "aw-definition-popover",
    label,
    htmltools::tags$button(
      type = "button",
      class = "btn btn-link p-0 ms-1 align-baseline",
      style = "color: #47BA83; text-decoration: none;",
      `data-bs-toggle` = "popover",
      `data-bs-trigger` = "hover focus",
      `data-bs-html` = "true",
      `data-bs-placement` = "top",
      `data-bs-title` = popover_title,
      `data-bs-content` = definition,
      `aria-label` = paste("Definition of", label),
      htmltools::tags$i(class = paste("fa", icon_class), `aria-hidden` = "true")
    )
  )
}

#' Client-side initializer for popovers added via \code{aw_definition_popover()}.
#'
#' Drop the result into the app head once. It registers all current
#' and future popover triggers on the page. Bootstrap does not auto-
#' wire popovers by default.
#' @export
aw_definition_popover_dependencies <- function() {
  htmltools::tags$script(htmltools::HTML(
    "(function(){function init(){",
    "var els = document.querySelectorAll('[data-bs-toggle=\"popover\"]');",
    "els.forEach(function(el){",
    "if (window.bootstrap && bootstrap.Popover) {",
    "bootstrap.Popover.getOrCreateInstance(el);}});}",
    "document.addEventListener('DOMContentLoaded', init);",
    "if (document.readyState !== 'loading') { init(); }",
    "var obs = new MutationObserver(init);",
    "obs.observe(document.body, { childList: true, subtree: true });",
    "})();"
  ))
}
