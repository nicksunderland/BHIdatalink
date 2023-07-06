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
