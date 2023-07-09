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
      div(class = "grid-container",
          div(class = "header", header()),
          div(class = "sidenav", style = "width: 200px;", navigation()),
          div(class = "main", style = "flex-grow: 1;",  main_page_router()),
          div(class = "footer", footer())
      )
    )
  )
}















