#' Themed reactable table
#'
#' Wraps \code{reactable::reactable()} with ASTHO-brand defaults: Jost
#' font, primary-blue header background, zebra striping, sortable
#' columns, page size 10, and a search field. All arguments accepted by
#' \code{reactable::reactable()} can be passed through; the defaults
#' here are overridden whenever the caller supplies the same argument
#' in \code{...}.
#'
#' @param   data   A data.frame or tibble.
#' @param   columns   Optional named list of \code{reactable::colDef()}
#'   objects. Use to customise column headers, formatting, alignment,
#'   etc.
#' @param   defaultPageSize   Default rows per page. Default \code{10}.
#' @param   searchable   Show the search field. Default \code{TRUE}.
#' @param   striped   Zebra-stripe rows. Default \code{TRUE}.
#' @param   highlight   Highlight rows on hover. Default \code{TRUE}.
#' @param   compact   Reduce cell padding. Default \code{FALSE}.
#' @param   ...   Additional arguments passed to
#'   \code{reactable::reactable()}.
#' @return   A \code{reactable} htmlwidget.
#' @export
aw_reactable <- function(data,
                         columns = NULL,
                         defaultPageSize = 10, # nolint: object_name_linter
                         searchable = TRUE,
                         striped = TRUE,
                         highlight = TRUE,
                         compact = FALSE,
                         ...) {
  if (!requireNamespace("reactable", quietly = TRUE)) {
    stop("aw_reactable() requires the 'reactable' package. ",
         "Install it with install.packages('reactable').",
         call. = FALSE)
  }

  theme_obj <- reactable::reactableTheme(
    style = list(fontFamily = "Jost, sans-serif", fontSize = "14px"),
    headerStyle = list(
      backgroundColor = "#005182",
      color = "#FFFFFF",
      fontWeight = "600",
      borderColor = "#003B5F"
    ),
    borderColor = "#E5E5E5",
    stripedColor = "#F4F4F4",
    highlightColor = "#EAF4FB",
    cellPadding = if (compact) "6px 8px" else "10px 12px",
    searchInputStyle = list(
      width = "100%",
      borderColor = "#D5D5D5",
      fontFamily = "Jost, sans-serif"
    )
  )

  reactable::reactable(
    data = data,
    columns = columns,
    defaultPageSize = defaultPageSize,
    searchable = searchable,
    striped = striped,
    highlight = highlight,
    compact = compact,
    bordered = FALSE,
    sortable = TRUE,
    theme = theme_obj,
    ...
  )
}
