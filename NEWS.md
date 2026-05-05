## asthoWidgets (development version)

* New `add_dumbbell_chart()` for low/high paired dumbbell charts.

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
