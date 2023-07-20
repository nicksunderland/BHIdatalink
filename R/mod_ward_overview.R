
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
                             brush = ns("length_of_stay_plot_brush")))
      ),
      column(
        width = 12,
        mainPanel(tableOutput(outputId = ns("length_of_stay_table")))
      )
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
      brushedPoints(df    = length_of_stay_data,
                    brush = input$length_of_stay_plot_brush,
                    xvar  = as.character(input$los_grouping_variable),
                    yvar  = "length_of_stay") |>
        # Mutate for nice formatting
        mutate(nhs_number     = format(floor(.data$nhs_number), nsmall=0),
               datetime_start = format(as.POSIXct(.data$admission_datetime), format="%d-%m-%Y %H:%M"),
               length_of_stay = format(floor(.data$length_of_stay), nsmall=0)) |>
        # Rename table column names for nice formatting
        select("NHS number"     = .data$nhs_number,
               "Admission date" = .data$admission_datetime,
               "Consultant"     = .data$consultant,
               "Ward"           = .data$ward,
               "LoS (days)"     = .data$length_of_stay)
    })

  })
}
