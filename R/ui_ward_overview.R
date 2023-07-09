#' Ward overview page
#' A function to create the ward overview page
#'
#' @import shiny
#' @importFrom DT DTOutput
#' @importFrom shinyWidgets prettyRadioButtons
#' @return a tagList object representing the cathlab page
#' @noRd
#'
ward_overview_page <- function() {

  # Create the page content
  content <- div(
    # A title
    titlePanel("Ward Overview"),
    # Horizontal line
    tags$hr(),
    # Some text
    tableOutput(outputId="text"),
    # Header title
    tags$h3("Current ward patient numbers"),
    # Ward overview plots
    fluidRow(
      column(
        width = 12,
        align = "centre",
        prettyRadioButtons(inputId = "ward_grouping_variable",
                     label = "Select Grouping:",
                     choiceNames = c("Ward", "Consultant"),
                     choiceValues = c("ward", "consultant"),
                     selected = "ward",
                     inline = TRUE,
                     animation = "smooth")
        ),
      column(
        width = 12,
        mainPanel(plotOutput(outputId="ward_counts"))
      )
    ),
    # Header title
    tags$h3("Length of stay"),
    # Length of stay plot
    fluidRow(
      column(
        width = 12,
        align = "centre",
        prettyRadioButtons(inputId = "los_grouping_variable",
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
        mainPanel(plotOutput(outputId = "length_of_stay",
                             brush = "length_of_stay_plot_brush"))
      ),
      tableOutput("length_of_stay_table")
    ),
    # Line break
    tags$br()
  )

  # return the page
  return(content)
}
