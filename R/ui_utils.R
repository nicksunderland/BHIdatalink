#' Makes a simple page for a Shiny app
#' @param title page title
#' @param subtitle page subtitle
#' @param contents page contents
#' @import shiny
#' @return the page
#' @noRd
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
#' @param title page title
#' @param content page contents
#' @param size page contents
#' @param style page contents
#' @import shiny
#' @return the card
#' @noRd
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
