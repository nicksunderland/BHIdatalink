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
    route(path="/",        ui=mod_ward_overview_ui("ward_overview")),
    route(path="cathlab",  ui=mod_cathlab_ui("cathlab")),
    route(path="devices",  ui=mod_devices_ui("devices"))
  )

  # return
  return(central_page_router)
}
