library(leaflet)
library(geojsonio)
library(readr)
# MOAS codes: https://geoportal.statistics.gov.uk/datasets/ons::msoa-2011-to-msoa-2021-to-local-authority-district-2022-lookup-for-england-and-wales-version-2-1/explore
# all post codes: https://www.doogal.co.uk/UKPostcodes?Search=BS#google_vignette
# note - MSOA11CD == MSOA Code, I think

# tutorial: https://rstudio.github.io/leaflet/json.html

msoa <- geojson_read(system.file("geojson", "bnssg_msoa.geojson", package="BHIdatalink"), what="sp")
postcode_map <- read_csv(system.file("geojson", "postcode_msoacode.csv", package="BHIdatalink"), show_col_types = FALSE)
postcodes <- postcode_map[sample(nrow(postcode_map), 30), ]


leaflet(msoa, options = leafletOptions(minZoom = 7, maxZoom = 13)) |>
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  setView(lat=51.454514, lng=-2.587910, zoom=9) |>
  addMarkers(~postcodes$lng, ~postcodes$lat, popup = ~as.character(postcodes$postcode))

  #



  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1, fillColor = ~pal(log10(1)))



