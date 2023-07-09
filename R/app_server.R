#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}. DO NOT REMOVE.
#' @import shiny ggplot2
#' @importFrom wesanderson wes_palette
#' @importFrom odbc dbDisconnect
#' @importFrom DBI dbDisconnect
#' @importFrom dplyr tbl mutate if_else collect select filter summarise rename left_join n group_by
#' @importFrom lubridate time_length interval with_tz
#' @importFrom rlang sym :=
#' @importFrom stringr str_to_title
#' @noRd
app_server <- function(input, output, session) {

  # Do the routing to different pages via the navigation panel
  shiny.router::router_server()

  # shiny::reactive are essentially functions that get called when needed
  admission_data <- reactive({
    # connect to the database
    con <- connect_to_database()
    # Get the data as a data.frame
    admission_data <- tbl(con, "admissions") |> collect()
    admission_data <- as.data.frame(admission_data)
    # Disconnect from the DB
    dbDisconnect(con)
    # return the data
    return(admission_data)
  })

  order_data <- reactive({
    # connect to the database
    con <- connect_to_database()
    # Get the data as a data.frame
    order_data <- tbl(con, "orders") |> collect()
    order_data <- as.data.frame(order_data)
    # Disconnect from the DB
    dbDisconnect(con)
    # return the data
    return(orders)
  })

  procedure_data <- reactive({
    # connect to the database
    con <- connect_to_database()
    # Get the data as a data.frame
    procedure_data <- tbl(con, "procedures") |> collect()
    procedure_data <- as.data.frame(procedure_data)
    # Disconnect from the DB
    dbDisconnect(con)
    # return the data
    return(procedure_data)
  })

  # calculate length of stay data
  length_of_stay_data <- reactive({
    # Calculate the length of stay
    today <- as.POSIXct("2023-07-30 23:59")
    # Get admissions data
    length_of_stay <- admission_data() |>
      mutate(length_of_stay = if_else(is.na(.data$datetime_end),
                                      time_length(interval(with_tz(.data$datetime_start, tzone = "GMT"),
                                                           with_tz(today, tzone = "GMT")), unit="days"),
                                      time_length(interval(with_tz(.data$datetime_start, tzone = "GMT"),
                                                           with_tz(.data$datetime_end, tzone = "GMT")), unit="days"))) |>
      mutate(!!sym(as.character(input$los_grouping_variable)) := if_else(!is.na(.data$datetime_end),
                                                                         "average",
                                                                         !!sym(as.character(input$los_grouping_variable))))
    # return the data
    return(length_of_stay)
  })

  # calculate the cath lab data
  cath_lab_data <- reactive({

    today <- as.POSIXct("2023-07-30 23:59")

    cath_lab_data <- admission_data() |>
      filter(is.na(.data$datetime_end)) |>
      left_join(order_data(), by="nhs_number") |>
      rename("datetime_order" = "datetime") |>
      left_join(procedure_data(), by="nhs_number") |>
      rename("datetime_procedure" = "datetime") |>
      mutate(awaiting     = if_else(!is.na(.data$datetime_order) & is.na(.data$datetime_procedure), TRUE, FALSE),
             waiting_time = if_else(!is.na(.data$datetime_order) & is.na(.data$datetime_procedure),
                                    time_length(interval(with_tz(.data$datetime_order, tzone = "GMT"),
                                                         with_tz(today, tzone = "GMT")), unit="days"),
                                    NA_real_),
             time_to_proc = if_else(!is.na(.data$datetime_order) & !is.na(.data$datetime_procedure),
                                    time_length(interval(with_tz(.data$datetime_order, tzone = "GMT"),
                                                         with_tz(.data$datetime_procedure, tzone = "GMT")), unit="days"),
                                    NA_real_))

    # return the data
    return(cath_lab_data)
  })



  output$ward_counts <- renderPlot({
    admission_data() |>
      ggplot(aes(x = .data[[as.character(input$ward_grouping_variable)]],
                 fill = .data[[as.character(input$ward_grouping_variable)]])) +
      geom_bar() +
      scale_fill_viridis_d() +
      ylab("Number of patients") +
      xlab(gsub("_", " ", str_to_title(as.character(input$ward_grouping_variable))))
  })

  output$length_of_stay <- renderPlot({
    length_of_stay_data() |>
      ggplot(aes(x = .data[[as.character(input$los_grouping_variable)]],
                 y = .data[["length_of_stay"]],
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
    brushedPoints(df = length_of_stay_data(),
                  brush = input$length_of_stay_plot_brush,
                  xvar = as.character(input$los_grouping_variable),
                  yvar = "length_of_stay") |>
      # Mutate for nice formatting
      mutate(nhs_number = format(floor(.data$nhs_number), nsmall=0),
             datetime_start = format(as.POSIXct(.data$datetime_start), format="%d-%m-%Y %H:%M"),
             length_of_stay = format(floor(length_of_stay), nsmall=0)) |>
      # Rename table column names for nice formatting
      select("NHS number" = .data$nhs_number,
             "Admission date" = .data$datetime_start,
             "Consultant" = .data$consultant,
             "Ward" = .data$ward,
             "LoS (days)" = .data$length_of_stay)
  })

  output$cath_lab_awaiting <- renderPlot({

    cath_lab_data() |>
      group_by(.data$awaiting) |>
      summarise(count = n()) |>
      mutate(pct = .data$count / sum(.data$count)) |>
      ggplot(aes(x = "",
                 y = .data$pct,
                 fill = .data$awaiting)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) +
      theme_void() +
      scale_fill_manual(values = wes_palette(n = 2, name = "GrandBudapest1")[1:2],
                        labels = c("None or complete", "Pending")) +
      labs(fill = "") +
      theme(legend.position = "top",
            legend.direction = "horizontal",
            legend.text = element_text(size = 14),
            legend.margin = margin(t = 0, b = -10, unit = "pt")) +
      geom_text(aes(label = paste0(round(pct*100), "% (n=", count, ")")),
                position = position_stack(vjust = 0.5),
                size = 6)
  })

  output$cath_lab_activity <- renderPlot({
    shinipsum::random_ggplot()
  })
  output$cath_lab_waiting_time <- renderPlot({
    shinipsum::random_ggplot()
  })
  output$cath_lab_proc_to_dis_time <- renderPlot({
    shinipsum::random_ggplot()
  })





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


}
