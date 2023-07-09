#' Cathlab page
#' A function to create the main page for the cathlab.
#'
#' @importFrom shiny div plotOutput uiOutput
#' @importFrom shiny.fluent Stack
#' @return a tagList object representing the cathlab page
#' @noRd
#'
cathlab_page <- function() {

  # Create the page content
  content <- div(
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
        mainPanel(plotOutput(outputId="cath_lab_awaiting"))
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
        prettyRadioButtons(inputId = "cathlab_activity_grouping_variable",
                           label = "Select Grouping:",
                           choiceNames = c("All", "Angiogram", "Device", "Other"),
                           choiceValues = c("all", "angiogram", "device", "other"),
                           selected = "all",
                           inline = TRUE,
                           animation = "smooth")
      ),
      column(
        width = 12,
        mainPanel(plotOutput(outputId="cath_lab_activity"))
      )
    ),


    # Header title
    tags$h3("Procedure waiting time"),
    # slider - days to analyse
    # radiobuttons - all(stacked), angio etc
    #3 waiting time: average violin next to line graph over last 30d, radiobuttons by type
    fluidRow(
      column(
        width = 12,
        align = "centre",
        prettyRadioButtons(inputId = "cathlab_waiting_time_grouping_variable",
                           label = "Select Grouping:",
                           choiceNames = c("All", "Angiogram", "Device", "Other"),
                           choiceValues = c("all", "angiogram", "device", "other"),
                           selected = "all",
                           inline = TRUE,
                           animation = "smooth")
      ),
      column(
        width = 12,
        mainPanel(plotOutput(outputId="cath_lab_waiting_time"))
      )
    ),


    # Header title
    tags$h3("Procedure to discharge time"),
    # radiobuttons - all(stacked), angio etc
    #4 procedure to discharge time: average violin next to line average over last X days
    fluidRow(
      column(
        width = 12,
        align = "centre",
        prettyRadioButtons(inputId = "cathlab_proc_to_dis_time_grouping_variable",
                           label = "Select Grouping:",
                           choiceNames = c("All", "Angiogram", "Device", "Other"),
                           choiceValues = c("all", "angiogram", "device", "other"),
                           selected = "all",
                           inline = TRUE,
                           animation = "smooth")
      ),
      column(
        width = 12,
        mainPanel(plotOutput(outputId="cath_lab_proc_to_dis_time"))
      )
    ),
  )

  # return the page
  return(content)
}





#
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


