# asthoWidgets

<!-- badges: start -->
[
  ![R-CMD-check](https://github.com/madocalder/asthoWidgets/actions/workflows/R-CMD-check.yaml/badge.svg)
](
  https://github.com/madocalder/asthoWidgets/actions/workflows/R-CMD-check.yaml
)

[![lint.yaml](https://github.com/madocalder/asthoWidgets/actions/workflows/lint.yaml/badge.svg)](https://github.com/madocalder/asthoWidgets/actions/workflows/lint.yaml)
<!-- badges: end -->

Reusable Shiny widgets for the ASTHO Profile app. Charts, choropleths, tables, KPI cards, and definition popovers, all themed to ASTHO's brand (Jost typography, ASTHO data viz color palette).

## Installation

```r
# install.packages("remotes")
remotes::install_github("madocalder/asthoWidgets")
```

## Widget catalog

### Charts

All charts use a two-step composition: `create_base_chart(type)` returns an empty themed Highchart, then one of the `add_*_chart()` helpers adds the series and series-specific options.

| Function | Renders |
| --- | --- |
| `add_column_chart()` | Vertical columns or horizontal bars (set `column_options$inverted = TRUE`). Supports single, grouped, and stacked series. |
| `add_line_chart()` | Single or multi-series trend lines. |
| `add_pie_chart()` | Pie or donut (default `innerSize = "70%"`). |
| `add_bubble_chart()` | Scatter with bubble size encoding a third variable. |
| `add_dumbbell_chart()` | Horizontal low / high range per category. |
| `add_sankey_chart()` | Sankey flow diagrams. |
| `add_word_cloud()` | Words sized by weight. |

```r
library(asthoWidgets)

df <- data.frame(state = c("AL", "AK", "AZ"), value = c(42, 71, 58))
create_base_chart(type = "column") |>
  add_column_chart(
    data = df, x_col = "state", y_col = "value",
    title_options = list(title = "Value by state")
  )
```

### Maps

```r
create_base_map() |>
  add_data_layers_to_map(map_data = us_map3, ...)
```

### Tables

```r
aw_reactable(mtcars, defaultPageSize = 15, searchable = TRUE)
```

### KPI cards

```r
aw_kpi_grid(
  aw_kpi_card(value = 59, label = "Jurisdictions", icon = "map"),
  aw_kpi_card(value = "$1.2B", label = "Total expenditures", icon = "dollar-sign"),
  aw_kpi_card(value = 102000, label = "FTEs", icon = "users"),
  col_widths = 3
)
```

### Definition popovers

```r
ui <- fluidPage(
  aw_definition_popover_dependencies(),
  aw_definition_popover(
    label = "FTE",
    definition = "Full-time equivalent. Counts a 1.0 FTE as one full-time employee."
  )
)
```

### Theme and export

* `get_astho_hc_theme()` returns the Highcharter theme used by `create_base_chart()`.
* `dataviz_colors` and `dataviz_palettes` expose the ASTHO brand palette.
* `viz_export()` attaches a PNG / SVG / CSV / XLS download menu to any Highchart.

## Development

```r
# Run tests
devtools::test()

# Regenerate man pages
roxygen2::roxygenise()
```
