#' Title
#'
#' For diffent icons search microsoft Fluent UI: https://developer.microsoft.com/en-us/fluentui#/styles/web/icons
#'
#' @return a navigation bar
#' @export
#'
navigation <- function() {
  shiny.fluent::Nav(
  groups = list(
    list(links = list(
      list(name = 'Home', url = '#!/', key = 'home', icon = 'Home'),
      list(name = 'Cath lab', url = '#!/cathlab', key = 'cathlab', icon = 'Manufacturing'),
      list(name = 'Devices', url = '#!/devices', key = 'devices', icon = 'Health'),
      list(name = 'BHI', url = 'https://www.uhbristol.nhs.uk/patients-and-visitors/your-hospitals/bristol-heart-institute-clinical-services/', key = 'bhi_website', icon = 'Website'),
      list(name = 'Appsilon', url = 'http://appsilon.com', key = 'appsilon', icon = 'WebAppBuilderFragment')
    ))
  ),
  initialSelectedKey = 'home',
  styles = list(
    root = list(
      height = '100%',
      boxSizing = 'border-box',
      overflowY = 'auto'
    )
  )
)
}
