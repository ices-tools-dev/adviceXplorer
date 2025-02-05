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
    
    # table(stock_list_long$StockKeyLabel)
    ### modifify SID table, 1 row == 1 Ecoregion
    stock_list_long <- separate_ecoregions(stock_list_all)
    ### add hyperlinks to table
    stock_list_long <- sid_table_links(stock_list_long)
    ### add component column
    ASDList <- icesASD::getAdviceViewRecord(year = year) %>%
        mutate(adviceComponent = na_if(adviceComponent, "N.A.")) %>% 
        rename(StockKeyLabel = stockCode, AssessmentKey = assessmentKey, AssessmentComponent = adviceComponent) %>%
        filter(adviceStatus == "Advice")
        

    # stock_list_all  %>% filter(StockKeyLabel == "nep.fu.16")
    stock_list_long <- merge(ASDList %>% select(AssessmentKey, StockKeyLabel, AssessmentComponent), stock_list_long, by = "StockKeyLabel", all = TRUE) %>%
        select(!AssessmentKey.y) %>%
        rename(AssessmentKey = AssessmentKey.x)
    
    
    # # Loop through each row and update AssessmentKey if necessary
    # for (i in 1:nrow(stock_list_long)) {
    #     if (is.na(stock_list_long$AssessmentKey[i]) &&
    #         !is.na(stock_list_long$YearOfLastAssessment[i]) &&
    #         stock_list_long$YearOfLastAssessment[i] != 0) {
    #         # Find the assessment key
    #         assessment_key <- icesSAG::findAssessmentKey(
    #             stock_list_long$StockKeyLabel[i],
    #             year = stock_list_long$YearOfLastAssessment[i]
    #         )

    #         # Check if the assessment key is valid before assigning
    #         if (length(assessment_key) > 0) {
    #             stock_list_long$AssessmentKey[i] <- assessment_key
    #         }
    #     }
    # }


    # Filter rows where AssessmentKey is NA and YearOfLastAssessment is not NA or 0
    valid_rows <- which(is.na(stock_list_long$AssessmentKey) &
        !is.na(stock_list_long$YearOfLastAssessment) &
        stock_list_long$YearOfLastAssessment != 0)

    # Find assessment keys for valid rows
    assessment_keys <- sapply(valid_rows, function(i) {
        icesSAG::findAssessmentKey(stock_list_long$StockKeyLabel[i],
            year = stock_list_long$YearOfLastAssessment[i]
        )
    })

    # Assign valid assessment keys back to the data frame
    valid_keys <- sapply(assessment_keys, length) > 0
    stock_list_long$AssessmentKey[valid_rows[valid_keys]] <- unlist(assessment_keys[valid_keys])
    stock_list_long <- stock_list_long %>% drop_na(AssessmentKey)

    # the following line find the last assessment year for each stock, just fill in assessment keys that are NA, when possible
    # stock_list_long <- stock_list_long %>%
    #     mutate(AssessmentKey = case_when(
    #         is.na(AssessmentKey) & !is.na(YearOfLastAssessment) & YearOfLastAssessment != 0 ~
    #             icesSAG::findAssessmentKey(StockKeyLabel, year = YearOfLastAssessment),
    #         TRUE ~ AssessmentKey
    #     )) %>%
    #     drop_na(AssessmentKey)
    
    stock_list_long <- stock_list_long %>%
        dplyr::mutate(
            stock_location = parse_location_from_stock_description(StockKeyDescription)
        )


    ### save SID table
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

