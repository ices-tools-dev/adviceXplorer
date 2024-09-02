#' Formats, updates and save the stock list table. It also adds the hyperlinks to the table.
#'
#' @param year the year required
#'
#' @return stock list table
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#'update_SID(2021)
#' }
#'
#' @references
#'
#' https://sid.ices.dk/Default.aspx
#'
#' @export
#'
update_SID <- function(year) {
    mkdir(paste0("Data/SID_", year))

    ### download SID
    stock_list_all <- download_SID(year)
    ### modifify SID table, 1 row == 1 Ecoregion
    stock_list_long <- separate_ecoregions(stock_list_all)
    ### add hyperlinks to table
    stock_list_long <- sid_table_links(stock_list_long)
    ### add component column
    ASDList <- icesASD::getAdviceViewRecord(year = year) %>% rename(StockKeyLabel = stockCode, AssessmentKey = assessmentKey, AssessmentComponent = adviceComponent)
    stock_list_long <- merge(ASDList %>% select(AssessmentKey, StockKeyLabel, AssessmentComponent), stock_list_long, by = "StockKeyLabel", all = TRUE) %>%
        select(!AssessmentKey.y) %>%
        rename(AssessmentKey = AssessmentKey.x)
    # some tidying up and adding description
    stock_list_long <- stock_list_long %>% drop_na(AssessmentKey)

    stock_list_long <- stock_list_long %>%
        dplyr::mutate(
            stock_location = parse_location_from_stock_description(StockKeyDescription)
        )
    write.taf(stock_list_long, file = "SID.csv", dir = paste0("Data/SID_", year), quote = TRUE)
}

#' Updates the data used to run the app
#'
#' @param mode the mode used to update the data
#'
#' @return downloads and save the data to the respective folders
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#'UpdateDataApp(mode = "LatestYear")
#' }
#'
#' @references
#'
#' https://sid.ices.dk/Default.aspx
#'
#' @export
#'
UpdateDataApp <- function(mode = c("AllYears", "LatestYear")) {
    if (mode == "AllYears") {
        years <- c(2024, 2023, 2022, 2021, 2020, 2019, 2018, 2017)
    } else if (mode == "LatestYear") {
        years <- as.integer(format(Sys.Date(), "%Y"))
    }

    for (year in years) {
        update_SAG(year)
        update_SID(year)
    }
}

