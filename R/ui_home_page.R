#' Title
#'
#' @importFrom DT DTOutput
#' @importFrom shiny plotOutput
#' @return a page
#' @export
#'
home_page <- function() {
  makePage(
  "BHI Datalink",
  "live cardiology data insights",
  shiny::div(
    h2("A Random DT"),
    DT::DTOutput("data_table"),
    h2("A Random Plot"),
    shiny::plotOutput("plot"),
    h2("A Random Text"),
    shiny::tableOutput("text")
  )
  )
}

