#' Router
#' A function to create the app main webpage router. This enables the central section of the
#' webpage to display different pages depending on clicks on the navigation panel.
#'
#' @importFrom shiny.router router_ui route
#' @return a shiny.router object
#' @noRd
#'
main_page_router <- function() {

  # create the router, adding the functions that create the different pages
  central_page_router <- router_ui(
    route(path="/",       ui=home_page()),
    route(path="cathlab", ui=cathlab_page())
  )

  # return
  return(central_page_router)
}
