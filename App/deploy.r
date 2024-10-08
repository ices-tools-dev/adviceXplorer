#' Function used to deploy the app to the shinyapps.io server, this is a rsconnect function
#'
#' @param appName 
#' @param appTitle
#' @param forceUpdate
#' 
#' 
#' @seealso
#'
#' @examples
#' \dontrun{
#' 
#' }
#'
#' @references
#'

#'
#' @export
deployApp(
  appName = "advicexplorer",
  appTitle = "advicexplorer",
  forceUpdate = getOption("rsconnect.force.update.apps", TRUE),
  launch.browser = FALSE
)
