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

    if (year == 2023) {
        row_to_duplicate <- stock_list_long %>% filter(StockKeyLabel == "cod.27.46a7d20" & stock_list_long$EcoRegion %in% c("Greater North Sea Ecoregion", "Celtic Seas Ecoregion"))
        # row_to_duplicate <- stock_list_long[stock_list_long$StockKeyLabel == "cod.27.46a7d20" & stock_list_long$EcoRegion == "Greater North Sea Ecoregion" , ]
        duplicated_df <- row_to_duplicate[rep(row.names(row_to_duplicate), each = 3), ]
        row.names(duplicated_df) <- NULL # Reset row names

        Cod_Keys <- c(18282, 18283, 18284, 18282, 18283, 18284)
        for (i in 1:nrow(duplicated_df)) {
            duplicated_df$AssessmentKey[i] <- Cod_Keys[i]
        }
        stock_list_long <- rbind(stock_list_long, duplicated_df)
        row.names(stock_list_long) <- NULL # Reset row names
    }







    write.taf(stock_list_long, file = "SID.csv", dir = paste0("Data/SID_", year))
}
# update_SID(2023)
