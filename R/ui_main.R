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






#' Navigation
#' A function to create the app webpage navigation panel.
#' For different icons search Microsoft Fluent UI:
#' https://developer.microsoft.com/en-us/fluentui#/styles/web/icons
#'
#' @importFrom shiny.fluent Nav
#' @return a Nav container object representing the navigation panel
#' @noRd
#'
navigation <- function() {

  # List the pages that will be displayed on the navigation panel
  page_list <-
    list(
      list(links = list(list(name='Overview',      url='#!/',             key='overview',     icon = 'Home'),
                        list(name='Cath lab',      url='#!/cathlab',      key='cathlab',      icon = 'Manufacturing'),
                        list(name='Devices',       url='#!/devices',      key='devices',      icon = 'Health'),
                        list(name='Heart Failure', url='#!/heartfailure', key='heartfailure', icon = 'HeartBroken'),
                        list(name='ACS',           url='#!/acs',          key='acs',          icon = 'Heart'),
                        list(name='Management',    url='#!/management',   key='management',   icon = 'WorkforceManagement'),
                        list(name='BHI',           url='#!/bhi',          key='bhi_website',  icon = 'Website'),
                        list(name='Other',         url='#!/other',        key='other',        icon = 'WebAppBuilderFragment')))
    )

  # Create the navigation container object with suitable defaults
  complete_navigation <- Nav(groups=page_list,
                             initialSelectedKey='home',
                             styles=list(root=list(height='100%',
                                                   boxSizing='border-box',
                                                   overflowY='auto')))

  # return
  return(complete_navigation)
}





#' Header
#' A function to create the app webpage header.
#'
#' @importFrom shiny tagList img div
#' @importFrom shiny.fluent Text CommandBar CommandBarItem
#' @return a tagList object representing the header
#' @noRd
#'
header <- function() {

  # The logo in an image container
  bhi_logo <- img(src="www/bhi_logo.png")

  # A container with some header text in it
  header_text <- div(Text(variant = "xLarge", "Datalink"), class = "title")

  # Generate the command bar
  command_bar <- CommandBar(
    items = list(CommandBarItem("New", "Add", subitems = list(CommandBarItem("Email message", "Mail", key = "emailMessage", href = "mailto:me@example.com"),
                                                              CommandBarItem("Calendar event", "Calendar", key = "calendarEvent"))),
                 CommandBarItem("Upload data", "Upload"),
                 CommandBarItem("Share analysis", "Share"),
                 CommandBarItem("Download report", "Download")),
    farItems = list(CommandBarItem("Grid view", "Tiles", iconOnly = TRUE),
                    CommandBarItem("Info", "Info", iconOnly = TRUE)),
    style = list(width = "100%")
  )

  # A list of 'tags'; R objects that represent HTML tags
  complete_header <- tagList(bhi_logo, header_text, command_bar)

  # Return
  return(complete_header)
}




#' Footer
#' A function to create the app webpage footer.
#'
#' @importFrom shiny.fluent Stack Text
#' @return a Stack container object representing the footer
#' @noRd
#'
footer <- function() {

  complete_footer <- Stack(
    # Combine horizontally
    horizontal = TRUE,
    # Alignment flag
    horizontalAlign = 'space-between',
    # Alignment parameters
    tokens = list(childrenGap = 20),
    # The text containers
    Text(variant = "medium", "Bristol Heart Institute", block=TRUE),
    Text(variant = "medium", nowrap = FALSE, "report bugs to nicholas.sunderland@uhbw.nhs.uk"),
    Text(variant = "medium", nowrap = FALSE, "All rights reserved.")
  )

  # return
  return(complete_footer)
}
