# asthoWidgets 0.0.5

* Add `add_column_chart()` (single-series, grouped, stacked, and inverted/bar
  variants), `add_line_chart()` (single + grouped trend), and
  `add_word_cloud()`. All follow the same option-list pattern as the
  existing chart helpers.

# asthoWidgets 0.0.4

* Add `create_base_chart()` and `add_pie_chart()` for building styled
  pie / donut charts. Mirrors the option-list pattern established by
  `create_base_map()` / `add_data_layers_to_map()`.

# asthoWidgets 0.0.3

* Move dataviz theme into the package: exports `dataviz_colors`,
  `dataviz_palettes`, and `get_astho_hc_theme()`. Previously lived in
  asthoProfile2025; widgets now ship a self-contained Highcharter theme.

# asthoWidgets 0.0.2

* Add functions 'create_base_map' and 'add_data_layers_to_map' for creating choropleths.

# asthoWidgets 0.0.1

* Add us_map2 and us_map3 data-objects; these can be used to generate highchart maps.
