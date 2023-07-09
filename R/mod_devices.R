#' devices UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_devices_ui <- function(id){
  ns <- NS(id)
  tagList(
    # A title
    titlePanel("Devices"),
    # Horizontal line
    tags$hr(),

    fluidRow(
      column(
        width = 12,
        mainPanel(plotOutput(outputId=ns("device_plot1")))
      )
    ),
  )
}

#' devices Server Functions
#'
#' @noRd
mod_devices_server <- function(id){
  moduleServer( id, function(input, output, session){

    output$device_plot1 <- renderPlot({
      shinipsum::random_ggplot()
    })

  })
}

## To be copied in the UI
# mod_devices_ui("devices_1")

## To be copied in the server
# mod_devices_server("devices_1")
