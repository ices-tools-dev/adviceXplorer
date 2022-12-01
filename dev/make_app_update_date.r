#' Creates a date that should represent the last date the app was updated. (in development)
#' 
#' @param null
#'
#' @return HTML string
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' make_app_update_date()
#' }
#'
#' @references
#'
#' 
#'
#' @export
make_app_update_date <- function() {
  string_update <- HTML(
    paste0("<font size= 2><i>App last update: ", Sys.Date(),"</i></font>")
  )

}