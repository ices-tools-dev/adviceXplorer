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

    #### NS cod exeption
    if (year == 2023) {
        row_to_duplicate <- stock_list_long %>% filter(StockKeyLabel == "cod.27.46a7d20" & stock_list_long$EcoRegion %in% c("Greater North Sea Ecoregion", "Celtic Seas Ecoregion"))
        duplicated_df <- row_to_duplicate[rep(row.names(row_to_duplicate), each = 3), ]
        row.names(duplicated_df) <- NULL # Reset row names

        Cod_Keys <- c(18282, 18283, 18284, 18282, 18283, 18284)
        for (i in 1:nrow(duplicated_df)) {
            duplicated_df$AssessmentKey[i] <- Cod_Keys[i]
        }
        stock_list_long <- rbind(stock_list_long, duplicated_df)
        row.names(stock_list_long) <- NULL # Reset row names
    }


    # some tidying up and adding description
    stock_list_long[stock_list_long$EcoRegion == "Iceland Sea Ecoregion", "EcoRegion"] <- "Icelandic Waters Ecoregion"
    stock_list_long <- stock_list_long %>% drop_na(AssessmentKey)

    stock_list_long <- stock_list_long %>% 
        dplyr::mutate(
        EcoRegion = removeWords(EcoRegion, "Ecoregion"),
        stock_location = parse_location_from_stock_description(StockKeyDescription)
      )




    write.taf(stock_list_long, file = "SID.csv", dir = paste0("Data/SID_", year),quote=TRUE)
}
# update_SID(2023)

# year <- 2023
# head(stock_list_long)
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
        years <- c(2023, 2022, 2021, 2020, 2019, 2018, 2017)
    } else if (mode == "LatestYear") {
        years <- as.integer(format(Sys.Date(), "%Y"))
    }

    for (year in years) {
        update_SAG(year)
        update_SID(year)
    }
}

