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
update_SID <- function(year){
    mkdir(paste0("Data/SID_", year))
    
    ### download SID
    stock_list_all <- download_SID(year)
    ### modifify SID table, 1 row == 1 Ecoregion
    stock_list_long <- separate_ecoregions(stock_list_all)
    ### add hyperlinks to table
    stock_list_long <- sid_table_links(stock_list_long)


    write.taf(stock_list_long, file = "SID.csv", dir = paste0("Data/SID_", year))

}
