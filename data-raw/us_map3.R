library(dplyr)

# Requires R >= 4.1.0, due to use of \(){} function shorthand

m <- jsonlite::read_json("data-raw/json-shapefiles/mapdata.json")
m2 <- jsonlite::read_json("data-raw/json-shapefiles/mapdata2.json")
m4 <- jsonlite::read_json("data-raw/json-shapefiles/mapdata4.json")

us_map <- m[[1]]$mapData
us_map <- purrr::map2(us_map, m2[[1]]$mapData, \(m1, m2) {
  m1$fips <- as.numeric(m1$fips)
  m1$path <- m2$path
  return(m1)
})

us_map3 <- m4[[1]]$mapData
us_map3 <- purrr::map(us_map3, \(m3) {
  m3$id <- m3$id |>
    (\(.) gsub("id", "", .))() |>
    as.integer() |>
    (\(.) . - 1)() |>
    (\(.) paste0("id", .))()

  cn <- us_map |>
    purrr::keep(\(x) {
      x$id == m3$id
    })

  if(length(cn) == 0){
    cat("Could not find:", m3$id, "\n")
    return(m3)
  }

  m3$fips <- as.numeric(cn[[1]]$fips)
  m3$name <- cn[[1]]$name
  return(m3)
})

usethis::use_data(us_map3, overwrite = TRUE)
