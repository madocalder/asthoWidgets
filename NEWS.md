# asthoWidgets 0.3.0

* feat: line charts cycle through eight distinct marker shapes, adding pentagon,
  hexagon and star via `inst/js/aw-marker-symbols.js`, so a sixth series no
  longer repeats the first marker (#66).
* feat: chart downloads are named from the chart title, and CSV/XLSX column
  headers come from the axis titles and series names instead of "Category" and
  "Series 1" (#50).
* fix: tooltips render outside the plot so they aren't clipped on narrow charts
  (#49).
* fix: line markers use radius 6 so they read at a consistent size (#65).
* fix: chart subtitles sit under the title instead of under the caption,
  matching the maps (#53).

# asthoWidgets 0.2.2

* doc: document that `markerEnabled` defaults to `TRUE` in `add_line_chart()`.

# asthoWidgets 0.2.1

* fix: `add_pie_chart()` now honours `pie_options$colors`, colouring each slice
  explicitly (aligned to the data order) so a donut can match a map's palette.
  Previously the option was ignored and slices fell back to the theme sequence.

# asthoWidgets 0.2.0

* feat: maps fill the width of their container. `add_data_layers_to_map()`
  sizes the plot from the measured container width and keeps the full legend
  beneath it, growing the chart for a tall legend rather than shrinking the
  map. It tracks viewport and layout changes and re-fits when revealed from an
  inactive tab.
* feat: donuts grow with their legend. `add_pie_chart()` adds height per legend
  item beyond a baseline, driven by the slice count so it stays reliable for
  outputs drawn with `suspendWhenHidden = FALSE`. Tunable via the new
  `size_options` argument (`perItem`, `baselineItems`, `maxHeight`,
  `legendBump`, `fillWidth`).

# asthoWidgets 0.1.2

* fix: line charts now show point markers by default, so single-point series
  render instead of disappearing.

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
