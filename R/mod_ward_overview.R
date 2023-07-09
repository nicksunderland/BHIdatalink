
#' @import shiny
#' @noRd
mod_ward_overview_ui <- function(id){
  ns <- NS(id)

  tagList(
    # A title
    titlePanel("Ward Overview"),
    # Horizontal line
    tags$hr(),
    # Header title
    tags$h3("Current ward patient numbers"),
    # Ward overview plots
    fluidRow(
      column(
        width = 12,
        align = "centre",
        prettyRadioButtons(inputId = ns("ward_grouping_variable"),
                           label = "Select Grouping:",
                           choiceNames = c("Ward", "Consultant"),
                           choiceValues = c("ward", "consultant"),
                           selected = "ward",
                           inline = TRUE,
                           animation = "smooth")
      ),
      column(
        width = 12,
        mainPanel(plotOutput(outputId=ns("ward_counts")))
      )
    ),
    # Header title
    tags$h3("Length of stay"),
    # Length of stay plot
    fluidRow(
      column(
        width = 12,
        align = "centre",
        prettyRadioButtons(inputId = ns("los_grouping_variable"),
                           label = "Select Grouping:",
                           choiceNames = c("Ward", "Consultant"),
                           choiceValues = c("ward", "consultant"),
                           selected = "ward",
                           inline = TRUE,
                           animation = "smooth")
      ),
      column(
        width = 12,
        tags$h4("*drag and select to explore data points", style = "color: red; font-size: 11px;")
      ),
      column(
        width = 12,
        mainPanel(plotOutput(outputId = ns("length_of_stay"),
                             brush = "length_of_stay_plot_brush"))
      ),
      tableOutput(outputId = ns("length_of_stay_table"))
    ),
    # Line break
    tags$br()
  )
}


#' @title ward_overview Server Functions
#' @description A shiny Module.
#' @param id,input,output,session Internal parameters for {shiny}.
#' @import shiny ggplot2
#' @importFrom shinyWidgets prettyRadioButtons
#' @importFrom dplyr mutate across select if_else group_by summarise
#' @importFrom lubridate time_length interval with_tz
#' @importFrom wesanderson wes_palette
#' @importFrom stringr str_to_title
#' @import shinipsum
#' @noRd
#'
mod_ward_overview_server <- function(id){
  moduleServer(id, function(input, output, session){

    # calculate length of stay data
    today <- as.POSIXct("2023-07-30 23:59")
    length_of_stay_data <- admissions_data() |>
        mutate(length_of_stay = if_else(is.na(.data$discharge_datetime),
                                        time_length(interval(with_tz(.data$admission_datetime, tzone = "GMT"),
                                                             with_tz(today, tzone = "GMT")), unit="days"),
                                        time_length(interval(with_tz(.data$admission_datetime, tzone = "GMT"),
                                                             with_tz(.data$discharge_datetime, tzone = "GMT")), unit="days"))) |>
      mutate(across(c("ward", "consultant"), ~ if_else(!is.na(.data$discharge_datetime), "average", .x)))


    output$ward_counts <- renderPlot({
      shinipsum::random_ggplot()
      admissions_data() |>
        ggplot(aes(x    = .data[[as.character(input$ward_grouping_variable)]],
                   fill = .data[[as.character(input$ward_grouping_variable)]])) +
        geom_bar() +
        scale_fill_viridis_d() +
        ylab("Number of patients") +
        xlab(gsub("_", " ", str_to_title(as.character(input$ward_grouping_variable))))
    })

    output$length_of_stay <- renderPlot({
      length_of_stay_data |>
        ggplot(aes(x    = .data[[as.character(input$los_grouping_variable)]],
                   y    = .data[["length_of_stay"]],
                   fill = .data[[as.character(input$los_grouping_variable)]])) +
        geom_violin() +
        geom_jitter(shape=16, position=position_jitter(0.1)) +
        scale_fill_viridis_d(option="inferno", begin = 0.35) +
        ylab("Length of stay (days)") +
        xlab(gsub("_", " ", str_to_title(as.character(input$los_grouping_variable))))
    })

    output$length_of_stay_table <- renderTable({
      # Require a click event to have happened
      req(input$length_of_stay_plot_brush)
      # Get the rows of the data frame based on the click info
      brushedPoints(df    = length_of_stay_data(),
                    brush = input$length_of_stay_plot_brush,
                    xvar  = as.character(input$los_grouping_variable),
                    yvar  = "length_of_stay") |>
        # Mutate for nice formatting
        mutate(nhs_number     = format(floor(.data$nhs_number), nsmall=0),
               datetime_start = format(as.POSIXct(.data$datetime_start), format="%d-%m-%Y %H:%M"),
               length_of_stay = format(floor(.data$length_of_stay), nsmall=0)) |>
        # Rename table column names for nice formatting
        select("NHS number"     = .data$nhs_number,
               "Admission date" = .data$datetime_start,
               "Consultant"     = .data$consultant,
               "Ward"           = .data$ward,
               "LoS (days)"     = .data$length_of_stay)
    })

  })
}





# # Generate a new histogram at timed intervals, but not when
# # input$n changes.
# output$plot <- renderPlot({
#   # Re-execute this reactive expression after 2000 milliseconds
#   invalidateLater(5000)
#   df$node = df$node + 10*runif(length(df$node))
#   df$node = ifelse(df$node>100, 0, round(df$node))
#   df$next_node = df$next_node + 10*runif(length(df$next_node))
#   df$next_node = ifelse(df$next_node>100, 0, round(df$next_node))
#   ggplot2::ggplot(isolate(df), ggplot2::aes(x = x,
#                  next_x = next_x,
#                  node = node,
#                  next_node = next_node,
#                  fill = factor(node),
#                  label = node)) +
#     ggsankey::geom_sankey(flow.alpha = 0.5, node.color = 1) +
#     ggsankey::geom_sankey_label(size = 3.5, color = 1, fill = "white") +
#     ggplot2::scale_fill_viridis_d(option = "A", alpha = 0.95) +
#     ggsankey::theme_sankey(base_size = 16)
#   #shinipsum::random_ggplot()
# })
#   # Your application server logic
#   filtered_deals <- shiny::reactive({
#     filtered_deals <- shiny.fluent::fluentSalesDeals
#   })
#
#   output$analysis <- shiny::renderUI({
#     items_list <- if(nrow(filtered_deals()) > 0){
#       shiny.fluent::DetailsList(items = filtered_deals(), columns = colnames(filtered_deals))
#     } else {
#       shiny::p("No matching transactions.")
#     }
#
#     shiny.fluent::Stack(
#       tokens = list(childrenGap = 5),
#       shiny.fluent::Text(variant = "large", "Sales deals details", block = TRUE),
#       shiny::div(style="max-height: 500px; overflow: auto", items_list)
#     )
#   })
