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
  appName = "online-single-stock-advice",
  appTitle = "Online single-stock advice",
  forceUpdate = getOption("rsconnect.force.update.apps", TRUE),
  launch.browser = FALSE
)
