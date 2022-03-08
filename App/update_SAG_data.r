#' Returns data from the ICES Stock Assessment database.
#'
#' Downloads summary and reference points from the ICES Stock Assessment database for a specific year
#' and places the two csv files in a year-specific folder called "SAG_xxxx" (for example: "SAG_2021")
#'
#' @param year the year required#'
#'
#' @return 2 csv files
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' years <- c(2021, 2020, 2019, 2018, 2017)
#' for (year in years) {
#'    update_SAG(year)
#' }
#' }
#'
#' @references
#'
#' The ICES stock assessment graphs Database web sevices: \url{http://standardgraphs.ices.dk/stockList.aspx}
#' ICES Transparent Assessment Framework: \url{https://taf.ices.dk}
#'
#' @export
#' 
options(icesSAG.use_token = TRUE)
update_SAG <- function(year){
    mkdir(paste("Data/SAG_", year))
    summary <- load_sag_summary(year)
    write.taf(summary, file = "SAG_summary.csv", dir = paste("Data/SAG_", year))

    refpts <- load_sag_refpts(year)
    write.taf(refpts, file = "SAG_refpts.csv", dir = paste("Data/SAG_", year))
}


## Ideally, this function would run every hour on the server to update sag
## it will take several minuts to run it locally for all the years, for now I will leave this
# function commented out, so it does not waste time when teh app is run locally
years <- c(2021, 2020, 2019, 2018, 2017)
for (year in years) {
    update_SAG(year)
}