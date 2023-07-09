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
