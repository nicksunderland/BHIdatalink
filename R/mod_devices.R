#' devices UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @importFrom leaflet leafletOutput
#' @importFrom shiny NS tagList
mod_devices_ui <- function(id){
  ns <- NS(id)
  tagList(
    # A title
    titlePanel("Devices"),
    # Horizontal line
    tags$hr(),
    tags$p("Map to display most recent device therpies in the region"),
    fluidRow(
      column(
        width = 12,
        mainPanel(leafletOutput(outputId=ns("map")))
      )
    ),
  )
}

#' devices Server Functions
#'
#' @noRd
#' @import leaflet
#' @importFrom geojsonio geojson_read
#' @importFrom readr read_csv
#'
mod_devices_server <- function(id){
  moduleServer( id, function(input, output, session){

    msoa <- geojson_read(system.file("geojson", "bnssg_msoa.geojson", package="BHIdatalink"), what="sp")
    postcode_map <- read_csv(system.file("geojson", "postcode_msoacode.csv", package="BHIdatalink"), show_col_types = FALSE)
    postcodes <- postcode_map[sample(nrow(postcode_map), 30), ]

    output$map <- renderLeaflet({

      leaflet(msoa, options = leafletOptions(minZoom = 7, maxZoom = 13)) |>
        addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.5) |>
        addProviderTiles(providers$CartoDB.Positron) |>
        setView(lat=51.454514, lng=-2.587910, zoom=9) |>
        addMarkers(~postcodes$lng, ~postcodes$lat, popup = ~as.character(postcodes$postcode))
    })

  })
}

## To be copied in the UI
# mod_devices_ui("devices_1")

## To be copied in the server
# mod_devices_server("devices_1")
