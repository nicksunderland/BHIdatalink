#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}. DO NOT REMOVE.
#' @import shiny ggplot2
#' @importFrom wesanderson wes_palette
#' @importFrom odbc dbDisconnect
#' @importFrom DBI dbDisconnect
#' @importFrom dplyr tbl mutate if_else collect select filter summarise rename left_join n group_by
#' @importFrom lubridate time_length interval with_tz
#' @importFrom rlang sym :=
#' @importFrom stringr str_to_title
#' @noRd
app_server <- function(input, output, session) {

  # Do the routing to different pages via the navigation panel
  shiny.router::router_server(root_page = "/")

  # Server functions for the modules
  mod_ward_overview_server("ward_overview")
  mod_cathlab_server("cathlab")
  mod_devices_server("devices")


}
