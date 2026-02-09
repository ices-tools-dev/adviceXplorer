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
    
    # Filter out rows where AssessmentKey is NA and YearOfLastAssessment is NA or 0
    missing_keys <- which(!is.na(stock_list_long$AssessmentKey) &
        !is.na(stock_list_long$YearOfLastAssessment) &
        stock_list_long$YearOfLastAssessment != 0)

    stock_list_long <- stock_list_long[missing_keys,]

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
# UpdateDataApp <- function(mode = c("AllYears", "LatestYear")) {
#     if (mode == "AllYears") {
#         years <- c(2024, 2023, 2022, 2021, 2020, 2019, 2018, 2017)
#     } else if (mode == "LatestYear") {
#         years <- as.integer(format(Sys.Date(), "%Y"))
#     }

#     for (year in years) {
#         update_SAG(year)
#         update_SID(year)
#     }
# }


 # --- SID: metadata + ecoregions only (no ASD/SAG here) ---
  getSID_meta_cached <- memoise::memoise(function(year) {
    message("Downloading SID (meta) for year: ", year)
    x_all  <- download_SID(year)
    x_long <- separate_ecoregions(x_all)
    data.table::setDT(x_long)

    # icons
    src <- match_stockcode_to_illustration(x_long$StockKeyLabel, x_long)
    x_long[, icon := paste0('<img src="', src, '" height=40>')]

    # keep rows with meaningful last assessment info
    x_long <- x_long[!is.na(YearOfLastAssessment) & YearOfLastAssessment != 0]
    x_long[, stock_location := parse_location_from_stock_description(StockKeyDescription)]
    x_long[]
  }, cache = sid_cache_by_year)

  # --- SAG: latest advice map (single call) ---
  getSAG_latest_cached <- memoise::memoise(function() {
    message("Fetching SAG latest stock advice list")
    dt <- data.table::as.data.table(icesSAG::getLatestStockAdviceList())
    # Normalise names to what your table expects
    # (Adjust if your returned column names differ)
    if ("stockCode" %in% names(dt)) dt[, StockKeyLabel := stockCode]
    if ("assessmentKey" %in% names(dt)) dt[, AssessmentKey := assessmentKey]
    if ("adviceComponent" %in% names(dt)) dt[, AssessmentComponent := adviceComponent]
    if ("AssessmentYear" %in% names(dt)) dt[, AssessmentYear := AssessmentYear]
    dt[, .(StockKeyLabel, AssessmentKey, AssessmentComponent, AssessmentYear)]
  }, cache = cachem::cache_mem(max_age = 6 * 3600))

  # --- SAG: stock list by year (one call per year; cached) ---
  getSAG_stocklist_year_cached <- memoise::memoise(function(year) {
    message("Fetching SAG StockList for year: ", year)
    url <- sprintf("https://sag.ices.dk/SAG_API/api/StockList?assessmentKey=0&year=%s", year)
    dt <- data.table::as.data.table(jsonlite::fromJSON(url))

    # Normalise
    if ("stockCode" %in% names(dt)) dt[, StockKeyLabel := stockCode]
    if ("assessmentKey" %in% names(dt)) dt[, AssessmentKey := assessmentKey]
    if ("adviceComponent" %in% names(dt)) dt[, AssessmentComponent := adviceComponent]
    if ("AssessmentYear" %in% names(dt)) dt[, AssessmentYear := AssessmentYear]
    if ("assessmentYear" %in% names(dt)) dt[, AssessmentYear := assessmentYear]

    # Keep only what we need for joining
    dt[, .(StockKeyLabel, AssessmentKey, AssessmentComponent, AssessmentYear)]
  }, cache = sag_cache_by_year)


  # --- ASD: advice view record by assessmentKey (lazy, cached) ---
  getASD_by_key_cached <- memoise::memoise(function(assessment_key) {
    message("Fetching ASD advice view record for assessmentKey: ", assessment_key)
    icesASD::getAdviceViewRecord(assessmentKey = assessment_key)
  }, cache = asd_cache_by_key)

  pick_asd_record_for_year <- function(df, active_year, assessment_component = NULL) {
  if (is.null(df) || nrow(df) == 0) return(df)

  df <- df %>%
    dplyr::filter(adviceViewPublished == TRUE, adviceStatus == "Advice")

  if (nrow(df) == 0) return(df)

  # Optional component filter (only if it actually narrows results)
  comp <- assessment_component
  if (is.na(comp) || comp == "NA") comp <- "N.A."

  if (!is.null(comp) && nzchar(comp)) {
    df_comp <- df %>%
      dplyr::filter(adviceComponent == comp | (is.na(adviceComponent) & comp == "N.A."))
    if (nrow(df_comp) > 0) df <- df_comp
  }

  active_date <- as.Date(sprintf("%d-07-01", as.integer(active_year)))

  df <- df %>%
    dplyr::mutate(
      from  = as.Date(substr(adviceApplicableFrom,  1, 10)),
      until = as.Date(substr(adviceApplicableUntil, 1, 10)),

      contains_year = (is.na(from)  | from  <= active_date) &
                      (is.na(until) | active_date <= until),

      # always defined; used only if contains_year == FALSE
      abs_start_dist = dplyr::if_else(
        is.na(from),
        Inf,
        abs(as.numeric(from - active_date))
      )
    )

  # Ranking:
  # 1) Prefer intervals that contain the active year
  # 2) If inside interval: prefer later 'from' (more specific)
  # 3) If not inside: prefer closest start date (abs_start_dist)
  # 4) Then later 'until'
  # 5) Then higher adviceKey (stable tie-break)
  df %>%
    dplyr::arrange(
      dplyr::desc(contains_year),
      dplyr::desc(from),
      abs_start_dist,
      dplyr::desc(until),
      dplyr::desc(adviceKey)
    ) %>%
    dplyr::slice(1)
}

getSAG_valid_for_year_from_sid <- function(active_year, sid_dt) {
  y <- as.integer(active_year)

  # Use SID only to decide which SAG years to fetch (no hard-coded lookback)
  yrs <- sort(unique(sid_dt$YearOfLastAssessment))
  yrs <- yrs[!is.na(yrs) & yrs != 0 & yrs <= y]

  # Safety: ensure the active year is included (in case SID misses it)
  if (!y %in% yrs) yrs <- c(yrs, y)

  # Fetch and combine SAG lists for those years (each year fetch is cached)
  sag_all <- data.table::rbindlist(lapply(yrs, getSAG_stocklist_year_cached), fill = TRUE)
  data.table::setDT(sag_all)

  sag_all <- sag_all[!is.na(StockKeyLabel) & !is.na(AssessmentKey)]
  sag_all <- unique(sag_all, by = c("StockKeyLabel", "AssessmentKey", "AssessmentComponent", "AssessmentYear"))

  # Only keep assessments up to the active year
  sag_all <- sag_all[AssessmentYear <= y]

  # Normalise component for grouping
  sag_all[, comp_norm := data.table::fifelse(
    is.na(AssessmentComponent) | AssessmentComponent %in% c("", "NA", "N.A."),
    "<none>",
    AssessmentComponent
  )]

  # For each stock + component, keep the most recent assessment (tie-break by AssessmentKey)
  sag_all <- sag_all[order(StockKeyLabel, comp_norm, -AssessmentYear, -AssessmentKey)]
  sag_keep <- sag_all[, .SD[1], by = .(StockKeyLabel, comp_norm)]

  sag_keep[, comp_norm := NULL]
  sag_keep[]
}

