#' Home page
#' A function to create the homepage.
#'
#' @importFrom shiny div h2 plotOutput tableOutput
#' @importFrom DT DTOutput
#' @return a tagList object representing the cathlab page
#' @noRd
#'
home_page <- function() {

  # Create the page content
  content <- div(
    # Some text
    tableOutput(outputId="text"),
    # A plot
    h2("Cathlab flow"),
    #plotly::plotlyOutput
    plotOutput(outputId="plot"),
    # A table
    h2("Some important data table"),
    DTOutput(outputId="data_table"),
  )

  # internal makePage function to standardise colours, fonts, etc
  page <- makePage(title="BHI Datalink",
                   subtitle="live cardiology data insights",
                   contents=content)

  # return
  return(page)
}
