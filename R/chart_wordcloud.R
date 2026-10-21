#' Add a word cloud series to a base chart
#'
#' Renders a Highcharts wordcloud where each word's font size is
#' proportional to its weight.
#'
#' @param   hc   A \code{highchart} object, typically from
#'   \code{create_base_chart(type = "wordcloud")}.
#' @param   data   A data.frame.
#' @param   word_col   Column name (string) for the word text.
#' @param   weight_col   Column name (string) for the word weight (frequency).
#' @param   wordcloud_options   Optional: \code{minFontSize},
#'   \code{maxFontSize}, \code{rotation} (a list controlling the word
#'   rotation distribution).
#' @param   title_options,subtitle_options,tooltip_options,caption_options
#'   Same shape as \code{add_column_chart()}.
#' @return   A \code{highchart} object.
#' @export
add_word_cloud <- function(hc,
                           data,
                           word_col,
                           weight_col,
                           wordcloud_options = list(),
                           title_options = list(),
                           subtitle_options = list(),
                           tooltip_options = list(),
                           caption_options = list()) {
  validate_columns(data, c(word_col, weight_col), "add_word_cloud") # nolint: object_usage_linter

  series_data <- lapply(seq_len(nrow(data)), function(i) {
    list(name = data[[word_col]][i], weight = data[[weight_col]][i])
  })

  hc |>
    highcharter::hc_title(
      text = nz_or_null(title_options$title), # nolint: object_usage_linter
      margin = title_options$margin %||% 10,
      widthAdjust = title_options$widthAdjust %||% -60
    ) |>
    add_astho_subtitle(subtitle_options) |> # nolint: object_usage_linter
    highcharter::hc_caption(
      text = caption_options$text,
      useHTML = caption_options$useHTML %||% TRUE,
      margin = caption_options$margin %||% 20
    ) |>
    highcharter::hc_tooltip(
      pointFormat = tooltip_options$pointFormat %||% "<b>{point.name}</b>: {point.weight}",
      headerFormat = tooltip_options$headerFormat %||% "",
      useHTML = tooltip_options$useHTML %||% TRUE
    ) |>
    highcharter::hc_add_series(
      type = "wordcloud",
      data = series_data,
      minFontSize = wordcloud_options$minFontSize %||% 10,
      maxFontSize = wordcloud_options$maxFontSize %||% 60,
      rotation = wordcloud_options$rotation %||% list(from = 0, to = 0)
    )
}
