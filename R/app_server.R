#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  shiny.router::router_server()

  output$data_table <- DT::renderDT({
    shinipsum::random_DT(5, 5)
  })
  output$plot <- renderPlot({
    shinipsum::random_ggplot()
  })
  output$text <- renderText({
    shinipsum::random_text(nwords = 50)
  })
  output$plot2 <- renderPlot({
    shinipsum::random_ggplot()
  })

  # Your application server logic
  filtered_deals <- shiny::reactive({
    filtered_deals <- shiny.fluent::fluentSalesDeals
  })

  output$analysis <- shiny::renderUI({
    items_list <- if(nrow(filtered_deals()) > 0){
      shiny.fluent::DetailsList(items = filtered_deals(), columns = colnames(filtered_deals))
    } else {
      shiny::p("No matching transactions.")
    }

    shiny.fluent::Stack(
      tokens = list(childrenGap = 5),
      shiny.fluent::Text(variant = "large", "Sales deals details", block = TRUE),
      shiny::div(style="max-height: 500px; overflow: auto", items_list)
    )
  })



  #router$server(input, output, session)
}
