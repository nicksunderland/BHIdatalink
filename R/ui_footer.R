#' Title
#'
#' @return a footer
#' @export
#'
footer <- function() {
  shiny.fluent::Stack(
  horizontal = TRUE,
  horizontalAlign = 'space-between',
  tokens = list(childrenGap = 20),
  shiny.fluent::Text(variant = "medium", "Bristol Heart Institute", block=TRUE),
  shiny.fluent::Text(variant = "medium", nowrap = FALSE, "report bugs to nicholas.sunderland@uhbw.nhs.uk"),
  shiny.fluent::Text(variant = "medium", nowrap = FALSE, "All rights reserved.")
)
}
