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
# update_SID <- function(year) {
#     # mkdir(paste0("Data/SID_", year))

#     ### download SID
#     stock_list_all <- download_SID(year)
    
#     # table(stock_list_long$StockKeyLabel)
#     ### modifify SID table, 1 row == 1 Ecoregion
#     stock_list_long <- separate_ecoregions(stock_list_all)
#     ### add hyperlinks to table
#     # stock_list_long <- sid_table_links(stock_list_long)
#     stock_list_long$icon <- paste0('<img src=', "'", match_stockcode_to_illustration(stock_list_long$StockKeyLabel, stock_list_long), "'", ' height=40>') 
#     ### add component column
    
#     ASDList <- icesASD::getAdviceViewRecord(year = year) %>%
#         mutate(adviceComponent = na_if(adviceComponent, "N.A.")) %>% 
#         rename(StockKeyLabel = stockCode, AssessmentKey = assessmentKey, AssessmentComponent = adviceComponent) %>%
#         filter(adviceStatus == "Advice")
        

#     # stock_list_all  %>% filter(StockKeyLabel == "nep.fu.16")
#     stock_list_long <- merge(ASDList %>% select(AssessmentKey, StockKeyLabel, AssessmentComponent), stock_list_long, by = "StockKeyLabel", all = TRUE) %>%
#         select(!AssessmentKey.y) %>%
#         rename(AssessmentKey = AssessmentKey.x)
    
    
#     # Filter rows where AssessmentKey is NA and YearOfLastAssessment is not NA or 0
#     valid_rows <- which(is.na(stock_list_long$AssessmentKey) &
#         !is.na(stock_list_long$YearOfLastAssessment) &
#         stock_list_long$YearOfLastAssessment != 0)

#     # Find assessment keys for valid rows
#     assessment_keys <- sapply(valid_rows, function(i) {
#         icesSAG::findAssessmentKey(stock_list_long$StockKeyLabel[i],
#             year = stock_list_long$YearOfLastAssessment[i]
#         )
#     })

#     # Assign valid assessment keys back to the data frame
#     valid_keys <- sapply(assessment_keys, length) > 0
#     stock_list_long$AssessmentKey[valid_rows[valid_keys]] <- unlist(assessment_keys[valid_keys])
#     stock_list_long <- stock_list_long %>% drop_na(AssessmentKey)

    
#     stock_list_long <- stock_list_long %>%
#         dplyr::mutate(
#             stock_location = parse_location_from_stock_description(StockKeyDescription)
#         )

#     return(stock_list_long)
# }
getSID <- function(year) {
    message("Downloading SID data for year: ", year)
    stock_list_all <- download_SID(year)

    # Convert 1 row per ecoregion
    stock_list_long <- separate_ecoregions(stock_list_all)

    # # Add icons using stock illustrations
    # stock_list_long$icon <- paste0(
    #     '<img src="', match_stockcode_to_illustration(stock_list_long$StockKeyLabel, stock_list_long), '" height=40>'
    # )
    # Add icons using stock illustrations
    setDT(stock_list_long)
    stock_list_long[, icon := paste0('<img src="', match_stockcode_to_illustration(StockKeyLabel, stock_list_long), '" height=40>')]

    
    # Get unique valid years (excluding NA and 0)
    valid_years <- unique(stock_list_long$YearOfLastAssessment)
    valid_years <- valid_years[!is.na(valid_years) & valid_years != 0]

    
    # Parallelized API calls for ASD records
    ASDList <- rbindlist(future_lapply(valid_years, function(y) {
        message("Fetching ASD advice records for year: ", y)
        as.data.table(icesASD::getAdviceViewRecord(year = y))
    }), fill = TRUE)
    
    ASDList <- ASDList %>% group_by(stockCode) %>% filter(assessmentYear == max(assessmentYear, na.rm = TRUE, finite = TRUE)) %>% ungroup()
    
    
    # Ensure ASDList is a valid data frame
    if (is.null(ASDList) || identical(ASDList, list()) || nrow(ASDList) == 0) {
        ASDList <- data.frame(
            StockKeyLabel = character(),
            AssessmentKey = character(),
            AssessmentComponent = character(),
            stringsAsFactors = FALSE
        )
    } else {
        ASDList <- ASDList %>%
            mutate(adviceComponent = na_if(adviceComponent, "N.A.")) %>%
            rename(
                StockKeyLabel = stockCode,
                AssessmentKey = assessmentKey,
                AssessmentComponent = adviceComponent
            ) %>%
            filter(adviceStatus == "Advice")
    }
    setDT(ASDList)

    # Efficient merge using data.table
    stock_list_long <- ASDList[stock_list_long, on = "StockKeyLabel"]
    # Merge stock list with ASDList
    message("Merging SID and ASD records...")
    # leaving this in for now
    # stock_list_long <- merge(ASDList %>% select(AssessmentKey, StockKeyLabel, AssessmentComponent),
    #     stock_list_long,
    #     by = "StockKeyLabel",
    #     all = TRUE
    # ) %>%
    #     select(-AssessmentKey.y) %>%
    #     rename(AssessmentKey = AssessmentKey.x)
    
    # Filter out rows where AssessmentKey is NA and YearOfLastAssessment is NA or 0
    missing_keys <- which(!is.na(stock_list_long$AssessmentKey) &
        !is.na(stock_list_long$YearOfLastAssessment) &
        stock_list_long$YearOfLastAssessment != 0)

    stock_list_long <- stock_list_long[missing_keys,]

    
    # if (length(missing_keys) > 0) {
    #     message("Finding missing assessment keys...")

    #     # Retrieve assessment keys (returns list)
    #     assessment_keys <- lapply(missing_keys, function(i) {
    #         keys <- findAssessmentKey(stock_list_long$StockKeyLabel[i],
    #             year = stock_list_long$YearOfLastAssessment[i]
    #         )
    #         if (length(keys) > 0) keys[1] else NA # Take only the first key or return NA
    #     })

    #     # Convert list to vector and assign
    #     stock_list_long$AssessmentKey[missing_keys] <- unlist(assessment_keys)
        
    # }


    # this solution spreads the different calls across threads, but each thread is still calling
    # the webservice multiple times, which is not ideal
    # # Identify missing AssessmentKeys
    # missing_rows <- stock_list_long %>%
    #     filter(is.na(AssessmentKey) & !is.na(YearOfLastAssessment) & YearOfLastAssessment != 0)

    # # Get unique (StockKeyLabel, YearOfLastAssessment) pairs
    # missing_pairs <- unique(missing_rows[, c("StockKeyLabel", "YearOfLastAssessment")])

    # if (nrow(missing_pairs) > 0) {
    #     message("Finding missing assessment keys in parallel...")

    #     # Parallel batch processing
    #     results <- future_lapply(seq_len(nrow(missing_pairs)), function(i) {
    #         stock <- missing_pairs$StockKeyLabel[i]
    #         year <- missing_pairs$YearOfLastAssessment[i]

    #         keys <- icesSAG::findAssessmentKey(stock, year)
    #         if (length(keys) > 0) keys[1] else NA  # Return only the first key
    #     })

    #     # Convert results to a lookup table
    #     assessment_lookup <- data.frame(
    #         StockKeyLabel = missing_pairs$StockKeyLabel,
    #         YearOfLastAssessment = missing_pairs$YearOfLastAssessment,
    #         AssessmentKey = unlist(results)
    #     )

    #     # Merge back into stock_list_long
    #     stock_list_long <- stock_list_long %>%
    #         left_join(assessment_lookup, by = c("StockKeyLabel", "YearOfLastAssessment")) %>%
    #         mutate(AssessmentKey = coalesce(AssessmentKey.x, AssessmentKey.y)) %>%
    #         select(-AssessmentKey.x, -AssessmentKey.y)
    # }

    # Drop rows where AssessmentKey is still NA
    # stock_list_long <- stock_list_long[!is.na(AssessmentKey)]

    # Extract stock location
    stock_list_long[, stock_location := parse_location_from_stock_description(StockKeyDescription)]


    message("Data processing complete.")
    return(stock_list_long)
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



