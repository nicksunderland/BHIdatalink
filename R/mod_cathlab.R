#' Cathlab page UI Function
#' @description A function to create the main page for the cathlab.
#'
#' @import shiny
#' @importFrom shinyWidgets prettyRadioButtons sliderTextInput
#' @return a tagList object representing the cathlab page
#' @noRd
#'
mod_cathlab_ui <- function(id){
  # All "outputId" needs to be wrapped in this module's namespace using NS(id, str_for_component_id)
  ns <- NS(id)

  # Create the page content
  tagList(
    # A title
    titlePanel("Cath lab"),
    # Horizontal line
    tags$hr(),
    # Header title
    tags$h3("Inpatients awaiting procedures"),
    #1 number of people awaiting procedures - pie chart
    fluidRow(
      column(
        width = 12,
        mainPanel(plotOutput(outputId=ns("cath_lab_awaiting")))
      )
    ),
    # Header title
    tags$h3("Cath lab activity"),
    # slider - days to cumulate
    # radiobuttons - all(stacked), angio etc
    #2 number of procedures in the last 7 days - bar chart, stacked by type
    fluidRow(
      column(
        width = 12,
        align = "centre",
        prettyRadioButtons(inputId = ns("cathlab_activity_grouping_variable"),
                           label = "Select Grouping:",
                           choiceNames = c("All", "Angiogram", "Pacemaker", "CRT", "Ablation"),
                           choiceValues = c("all", "angiogram", "pacemaker", "crt", "ablation"),
                           selected = "all",
                           inline = TRUE,
                           animation = "smooth")
      ),
      column(
        width = 12,
        align = "centre",
        sliderTextInput(inputId = ns("cathlab_activity_slider"),
                        label = "Over the last (days):",
                        choices = c(1, 14, 30, 90, 180, 270, 365),
                        selected = 30,
                        grid = TRUE)
      ),
      column(
        width = 12,
        mainPanel(plotOutput(outputId=ns("cath_lab_activity")))
      )
    ),


    # Header title
    tags$h3("TBC - Procedure waiting time"),
    # slider - days to analyse
    # radiobuttons - all(stacked), angio etc
    #3 waiting time: average violin next to line graph over last 30d, radiobuttons by type
    fluidRow(
      column(
        width = 12,
        align = "centre",
        prettyRadioButtons(inputId = ns("cathlab_waiting_time_grouping_variable"),
                           label = "Select Grouping:",
                           choiceNames = c("All", "Angiogram", "Device", "Other"),
                           choiceValues = c("all", "angiogram", "device", "other"),
                           selected = "all",
                           inline = TRUE,
                           animation = "smooth")
      ),
      column(
        width = 12,
        mainPanel(plotOutput(outputId=ns("cath_lab_waiting_time")))
      )
    ),


    # Header title
    tags$h3("TBC - Procedure to discharge time"),
    # radiobuttons - all(stacked), angio etc
    #4 procedure to discharge time: average violin next to line average over last X days
    fluidRow(
      column(
        width = 12,
        align = "centre",
        prettyRadioButtons(inputId = ns("cathlab_proc_to_dis_time_grouping_variable"),
                           label = "Select Grouping:",
                           choiceNames = c("All", "Angiogram", "Device", "Other"),
                           choiceValues = c("all", "angiogram", "device", "other"),
                           selected = "all",
                           inline = TRUE,
                           animation = "smooth")
      ),
      column(
        width = 12,
        mainPanel(plotOutput(outputId=ns("cath_lab_proc_to_dis_time")))
      )
    ),
  )
}

#     # A Stack with a card in it
#     Stack(horizontal=TRUE,
#           tokens=list(childrenGap=10),
#           makeCard("Filters", filters, size=4, style="max-height: 320px")),
#     # Some more output
#     uiOutput("analysis")
#
# filters <- shiny.fluent::Stack(
#   tokens = list(childrenGap = 10),
#   shiny.fluent::Stack(
#     horizontal = TRUE,
#     tokens = list(childrenGap = 10),
#     shiny.fluent::DatePicker.shinyInput("fromDate", value = as.Date('2020/01/01'), label = "From date"),
#     shiny.fluent::DatePicker.shinyInput("toDate", value = as.Date('2020/12/31'), label = "To date")
#   ),
#   shiny.fluent::Label("Filter by sales reps", className = "my_class"),
#   shiny.fluent::NormalPeoplePicker.shinyInput(
#     "selectedPeople",
#     class = "my_class",
#     options = shiny.fluent::fluentPeople,
#     pickerSuggestionsProps = list(
#       suggestionsHeaderText = 'Matching people',
#       mostRecentlyUsedHeaderText = 'Sales reps',
#       noResultsFoundText = 'No results found',
#       showRemoveButtons = TRUE
#     )
#   ),
#   shiny.fluent::Slider.shinyInput("slider",
#                                   value = 0, min = 0, max = 1000000, step = 100000,
#                                   label = "Minimum amount",
#                                   valueFormat = shiny.fluent::JS("function(x) { return '$' + x}"),
#                                   snapToStep = TRUE
#   ),
#   shiny.fluent::Toggle.shinyInput("closedOnly", value = TRUE, label = "Include closed deals only?")
# )









#' cathlab Server Functions
#'
#' @importFrom dplyr bind_rows
#' @noRd
#'
mod_cathlab_server <- function(id){
  moduleServer(id, function(input, output, session){

    # calculate the cath lab data
    today <- as.POSIXct("2023-07-30 23:59")
    cath_lab_orders_data <- admissions_data(discharged=FALSE) |>
        left_join(orders_data(), by="nhs_number") |>
        left_join(procedures_data(), by="nhs_number") |>
        mutate(awaiting     = if_else(!is.na(.data$order_datetime) & is.na(.data$procedure_datetime), TRUE, FALSE),
               waiting_time = if_else(!is.na(.data$order_datetime) & is.na(.data$procedure_datetime),
                                      time_length(interval(with_tz(.data$order_datetime, tzone = "GMT"),
                                                           with_tz(today, tzone = "GMT")), unit="days"),
                                      NA_real_),
               time_to_proc = if_else(!is.na(.data$order_datetime) & !is.na(.data$procedure_datetime),
                                      time_length(interval(with_tz(.data$order_datetime, tzone = "GMT"),
                                                           with_tz(.data$procedure_datetime, tzone = "GMT")), unit="days"),
                                      NA_real_))

    # calculate activity over time
    .tmp_cath_lab_activity_data <- procedures_data() |>
      mutate(date = as.Date(.data$procedure_datetime)) |>
      select(-.data$procedure_datetime)

    .tmp_cath_lab_activity_all <- .tmp_cath_lab_activity_data |>
      group_by(.data$date) |>
      summarise(count = n()) |>
      mutate(procedure_type = "all")

    cath_lab_activity_data <- .tmp_cath_lab_activity_data |>
      group_by(.data$date, .data$procedure_type) |>
      summarise(count = n()) |>
      bind_rows(.tmp_cath_lab_activity_all)

    # Pie chart showing % of patients awaiting a procedure
    output$cath_lab_awaiting <- renderPlot({
      cath_lab_orders_data |>
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
        geom_text(aes(label = paste0(round(.data$pct*100), "% (n=", .data$count, ")")),
                  position = position_stack(vjust = 0.5),
                  size = 6)
    })

    output$cath_lab_activity <- renderPlot({

      grouping <- as.character(input$cathlab_activity_grouping_variable)
      today <- as.Date("2023-07-30")
      win_start <- today - input$cathlab_activity_slider

      cath_lab_activity_data |>
        filter(date > win_start) %>%
        {if(grouping!="all") filter(., .data$procedure_type==grouping) else .} |>
        ggplot(aes(x     = .data$date,
                   y     = .data$count,
                   color = .data$procedure_type)) +
        geom_point(shape=21, size=5, color="black", aes(fill=.data$procedure_type)) +
        geom_line(aes(color = .data$procedure_type)) +
        scale_color_viridis_d(option="inferno", end = 0.9) +
        ylab("Procedures per day") +
        xlab("Date") +
        xlim(win_start-1, today+1) +
        theme(legend.position = "top",
              legend.direction = "horizontal",
              legend.text = element_text(size = 12))

    })
    output$cath_lab_waiting_time <- renderPlot({
      shinipsum::random_ggplot()
    })
    output$cath_lab_proc_to_dis_time <- renderPlot({
      shinipsum::random_ggplot()
    })

  })
}
