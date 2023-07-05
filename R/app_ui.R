#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`. DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {

  # A list of 'tags'; R objects that represent HTML tags
  tagList(

    # Add external resources
    golem_add_external_resources(),

    # The application UI logic - the page takes the contents of the document body
    fluidPage(

      # Create an HTML 'head' tag - the <head> element is a container for metadata
      tags$head(
        tags$link(href="style.css", rel="stylesheet", type="text/css"), # link to the style sheet
      ),

      # Set the overall layout of the page
      shiny::div(class = "grid-container",
                 shiny::div(class = "header", header()),
                 shiny::div(class = "sidenav", navigation()),
                 #shiny::div(class = "main", foo_test()),
                 #shiny::div(class = "main", home_page()),
                 shiny::div(class = "main", main_page_router),
                 shiny::div(class = "footer", footer())
      )
    )
  )
}

#' Makes a simple page for a Shiny app
#' @param title page title
#' @param subtitle page subtitle
#' @param contents page contents
#' @return the page
#' @export
makePage <- function (title, subtitle, contents) {
  tagList(div(
    class = "page-title",
    span(title, class = "ms-fontSize-32 ms-fontWeight-semibold", style =
           "color: #323130"),
    span(subtitle, class = "ms-fontSize-14 ms-fontWeight-regular", style =
           "color: #605E5C; margin: 14px;")
  ),
  contents)
}

#' Makes a simple card for a Shiny app
#' @param title card title
#' @param content page contents
#' @param size text size
#' @param style style
#' @importFrom glue glue
#' @return the card
#' @export
makeCard <- function(title, content, size = 12, style = "") {
  div(
    class = glue::glue("card ms-depth-8 ms-sm{size} ms-xl{size}"),
    style = style,
    shiny.fluent::Stack(
      tokens = list(childrenGap = 5),
      shiny.fluent::Text(variant = "large", title, block = TRUE),
      content
    )
  )
}

# card1 <- makeCard(
#   "Welcome to the BHI datalink demo!",
#   shiny::div(
#     shiny.fluent::Text("How to build Shiny apps using Microsoft's Fluent UI."),
#     shiny.fluent::Text("Use the menu on the left to explore live demos")
#   ))

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
      shiny::tableOutput("text"),
      h2("A Random Plot"),
      shiny::plotOutput("plot")
    )
  )
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


#' Title
#'
#' @return a page
#' @export
#'
cathlab_page <- function() {
  makePage(
    "Cath lab analysis",
    "....",
    shiny::div(
      shiny::plotOutput("plot2"),
      shiny.fluent::Stack(
        horizontal = TRUE,
        tokens = list(childrenGap = 10),
        makeCard("Filters", filters, size = 4, style = "max-height: 320px")
      ),
      shiny::uiOutput("analysis")
    )
  )
}



main_page_router <- shiny.router::router_ui(
  shiny.router::route("/",  home_page()),
  shiny.router::route("cathlab", cathlab_page())
)



#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "BHIdatalink"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

