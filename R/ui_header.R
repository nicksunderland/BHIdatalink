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
