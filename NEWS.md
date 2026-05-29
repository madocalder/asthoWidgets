# asthoWidgets 0.1.1

* fix: single-category line and column charts no longer render one tick
  per character of the category name.

# asthoWidgets 0.1.0

* Add `add_dumbbell_chart()` for horizontal dumbbell charts (low / high pairs
  per category, with optional grouping).
* Add `add_sankey_chart()` for Sankey flow diagrams.
* Add `aw_reactable()`, a themed wrapper around `reactable::reactable()`
  with ASTHO header styling, Jost font, zebra striping, and a search field.
* Add `aw_kpi_card()` and `aw_kpi_grid()` for headline-number cards arranged
  in a responsive grid.
* Add `aw_definition_popover()` plus a companion script helper so chart
  titles, selectors, and other inline labels can carry a hover-triggered
  definition popover backed by Bootstrap.

# asthoWidgets 0.0.10

* Fix: Empty/blank title and subtitle strings now actually suppress the
  Highcharts `Chart title` placeholder.

# asthoWidgets 0.0.9

* `add_column_chart()` now accepts `column_options$colorByPoint` so a
  single-series column / bar chart can cycle through the theme palette,
  giving each bar its own colour.

# asthoWidgets 0.0.8

* Add `add_bubble_chart()` for bubble-series charts (single-series or
  grouped).
* Refactor `chart.R` into into individual files.

# asthoWidgets 0.0.7

* Fix: Empty title strings (e.g. `title: ""` in YAML) no longer render
  the Highcharts default `Chart title` placeholder. All chart helpers
  treat empty/blank title and subtitle text as "no title".

# asthoWidgets 0.0.6

* Fix: Axis lable settings not being sent correctly.

# asthoWidgets 0.0.5

* Add `add_column_chart()` (single-series, grouped, stacked, and inverted/bar
  variants), `add_line_chart()` (single + grouped trend), and
  `add_word_cloud()`.

# asthoWidgets 0.0.4

* Add `create_base_chart()` and `add_pie_chart()` for building styled
  pie / donut charts.

# asthoWidgets 0.0.3

* Move dataviz theme fromt he app into the package: exports `dataviz_colors`,
  `dataviz_palettes`, and `get_astho_hc_theme()`. Widgets now ship a
   self-contained Highcharter theme.

# asthoWidgets 0.0.2

* Add functions 'create_base_map' and 'add_data_layers_to_map' for creating choropleths.

# asthoWidgets 0.0.1

* Add us_map2 and us_map3 data-objects; these can be used to generate highchart maps.
