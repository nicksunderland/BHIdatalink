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
    # Line break
    tags$br(),
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
    # Line break
    tags$br(),
    # A table title
    h2("Some important data table"),
    # Horizontal line
    tags$hr(),
    # The table
    DTOutput(outputId="data_table"),
  )

  # internal makePage function to standardise colours, fonts, etc
  # page <- makePage(#title="BHI Datalink",
  #                  #subtitle="live cardiology data insights",
  #                  contents=content)

  # return
  return(content)
}
