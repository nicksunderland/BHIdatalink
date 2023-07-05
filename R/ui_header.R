#' Title
#'
#' @return a header
#' @export
#'
header <- function() {
  shiny::tagList(
    img(src="www/bhi_logo.png"),
    div(shiny.fluent::Text(variant = "xLarge", "Datalink"), class = "title"),
    shiny.fluent::CommandBar(
      items = list(
        shiny.fluent::CommandBarItem("New", "Add", subitems = list(
          shiny.fluent::CommandBarItem("Email message", "Mail", key = "emailMessage", href = "mailto:me@example.com"),
          shiny.fluent::CommandBarItem("Calendar event", "Calendar", key = "calendarEvent")
        )),
        shiny.fluent::CommandBarItem("Upload data", "Upload"),
        shiny.fluent::CommandBarItem("Share analysis", "Share"),
        shiny.fluent::CommandBarItem("Download report", "Download")
      ),
      farItems = list(
        shiny.fluent::CommandBarItem("Grid view", "Tiles", iconOnly = TRUE),
        shiny.fluent::CommandBarItem("Info", "Info", iconOnly = TRUE)
      ),
      style = list(width = "100%"))
  )
}
