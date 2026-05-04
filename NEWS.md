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
