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
    # A plot
    plotOutput(outputId="plot2"),
    # A Stack with a card in it
    Stack(horizontal=TRUE,
          tokens=list(childrenGap=10),
          makeCard("Filters", filters, size=4, style="max-height: 320px")),
    # Some more output
    uiOutput("analysis")
  )

  # internal makePage function to standardise colours, fonts, etc
  page <- makePage(title="Cath lab analysis",
                   subtitle="....",
                   contents=content)

  # return
  return(page)
}







filters <- shiny.fluent::Stack(
  tokens = list(childrenGap = 10),
  shiny.fluent::Stack(
    horizontal = TRUE,
    tokens = list(childrenGap = 10),
    shiny.fluent::DatePicker.shinyInput("fromDate", value = as.Date('2020/01/01'), label = "From date"),
    shiny.fluent::DatePicker.shinyInput("toDate", value = as.Date('2020/12/31'), label = "To date")
  ),
  shiny.fluent::Label("Filter by sales reps", className = "my_class"),
  shiny.fluent::NormalPeoplePicker.shinyInput(
    "selectedPeople",
    class = "my_class",
    options = shiny.fluent::fluentPeople,
    pickerSuggestionsProps = list(
      suggestionsHeaderText = 'Matching people',
      mostRecentlyUsedHeaderText = 'Sales reps',
      noResultsFoundText = 'No results found',
      showRemoveButtons = TRUE
    )
  ),
  shiny.fluent::Slider.shinyInput("slider",
                                  value = 0, min = 0, max = 1000000, step = 100000,
                                  label = "Minimum amount",
                                  valueFormat = shiny.fluent::JS("function(x) { return '$' + x}"),
                                  snapToStep = TRUE
  ),
  shiny.fluent::Toggle.shinyInput("closedOnly", value = TRUE, label = "Include closed deals only?")
)


