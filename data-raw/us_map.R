library(dplyr)

# Requires R >= 4.1.0, due to use of \(){} function shorthand

m <- jsonlite::read_json("data-raw/json-shapefiles/mapdata.json")
m2 <- jsonlite::read_json("data-raw/json-shapefiles/mapdata2.json")
m4 <- jsonlite::read_json("data-raw/json-shapefiles/mapdata4.json")

# Ensure 'fips' is an integer
# Ensure region shape/geometry comes from mapdata2.json
us_map <- purrr::map2(
  m[[1]]$mapData,
  m2[[1]]$mapData,
  function(region, new_region) {
    region$fips <- as.numeric(region$fips)
    region$path <- new_region$path
    return(region)
})

# Ensure `id`, `name` and `fips` matches between `us_map3` and `us_map`
# - Used in governance, structure, activities pages
us_map3 <- purrr::map(
  m4[[1]]$mapData,
  function(region) {

    # IDs are mismatched between `us_map` and `mapdata4.json`
    shift_region_id <- function(x) {
      paste0("id", -1 + as.integer(gsub("id", "", x)))
    }
    region$id <- shift_region_id(region$id)

    # Get the matching region from `us_map`
    cn <- us_map |>
      purrr::keep(function(x) {
        x$id == region$id
      })

    if(length(cn) == 0){
      cat("Could not find:", region$id, "\n")
      return(region)
    }

    region$fips <- as.numeric(cn[[1]]$fips)
    region$name <- cn[[1]]$name
    return(region)
})

usethis::use_data(us_map3, overwrite = TRUE)
