#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny plotly
#' @importFrom stats runif
#' @noRd
app_server <- function(input, output, session) {

  shiny.router::router_server()

  df <- readRDS("inst/app/www/data.rds")

  output$plot <- renderPlot({

    # Generate a new histogram at timed intervals, but not when
    # input$n changes.
    output$plot <- renderPlot({
      # Re-execute this reactive expression after 2000 milliseconds
      invalidateLater(5000)
      df$node = df$node + 10*runif(length(df$node))
      df$node = ifelse(df$node>100, 0, round(df$node))
      df$next_node = df$next_node + 10*runif(length(df$next_node))
      df$next_node = ifelse(df$next_node>100, 0, round(df$next_node))
      ggplot2::ggplot(isolate(df), ggplot2::aes(x = x,
                     next_x = next_x,
                     node = node,
                     next_node = next_node,
                     fill = factor(node),
                     label = node)) +
        ggsankey::geom_sankey(flow.alpha = 0.5, node.color = 1) +
        ggsankey::geom_sankey_label(size = 3.5, color = 1, fill = "white") +
        ggplot2::scale_fill_viridis_d(option = "A", alpha = 0.95) +
        ggsankey::theme_sankey(base_size = 16)
      #shinipsum::random_ggplot()
    })

  })



  output$data_table <- DT::renderDT({
    shinipsum::random_DT(5, 5)
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
