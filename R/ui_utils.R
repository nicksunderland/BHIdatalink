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

#' Add external Resources to the Application
#'
#' This function is internally used to add external resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {

  golem::add_resource_path(prefix="www", directoryPath=app_sys("app/www"))

  # Create an HTML 'head' tag - the <head> element is a container for metadata
  tags$head(
    bundle_resources(path = app_sys("app/www"), app_title="BHIdatalink")

  )
}
