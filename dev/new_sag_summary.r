load_sag_summary <- function(year) {
#   out <- icesSAG::getSAG(
#     stock = NULL, year, data = "summary",
#     purpose = "Advice", combine = TRUE
#   )
  out <- getSAGSummary(year, combine = TRUE)
  sid <- load_sid(year)
  sid <- dplyr::filter(sid, !is.na(YearOfLastAssessment))
  sid <- dplyr::select(
    sid, StockKeyLabel, YearOfLastAssessment,
    PreviousStockKeyLabel
  )
  colnames(sid) <- c("fishstock", "AssessmentYear", "PreviousStockKeyLabel")
  old <- dplyr::filter(sid, AssessmentYear < 2017)
  out1 <- merge(out, sid,
    by = c("fishstock", "AssessmentYear"),
    all = FALSE
  )
#   out2 <- merge(out, old,
#     by.x = c("fishstock", "AssessmentYear"),
#     by.y = c("PreviousStockKeyLabel", "AssessmentYear"),
#     all = FALSE
#   )
#   out2$fishstock <- out2$fishstock.y
#   out2 <- subset(out2, select = -fishstock.y)
#   out <- merge(out1, out2, all = TRUE)
#   out <- subset(out, select = -PreviousStockKeyLabel)
  unique(out1)
}
sag_df <- load_sag_summary(2022)


getSAGSummary <- function(year, combine = TRUE) {
    
    datalist = list()
    assessmentkey <- findAssessmentKey(year = year)

    for (i in seq_len(length(assessmentkey))){


    out <- jsonlite::fromJSON(
        URLencode(
            sprintf("https://sag.ices.dk/SAG_API/api/SummaryTable?assessmentKey=%s", assessmentkey[i])
            # sprintf("https://sag.ices.dk/SAG_API/api/StockSettings?assessmentKey=%s", assessmentkey)
        )
    ) 

# drop any null entries (happens when not published stocks creep in)
  out <- out[!sapply(out, is.null)]
    
  # combine tables
  if (length(out) > 1 && combine) {
    # form new column names for combined data frame
    outNames <- unique(unlist(lapply(out, names)))

    # rbind, adding in missing columns as characters
    out1 <-
      do.call(rbind,
        lapply(unname(out), function(x) {
          # are any columns missing?
          missing.cols <- !outNames %in% names(x)
          if (any(missing.cols)) {
            # add on missing columns as characters
            x[outNames[missing.cols]] <- NA
          }
          # reorder columns
          x[outNames]
        }))
        
        # take out rows where all columns are NA
        df2 <- subset(out1,!is.na(out1[,1]))

        # identify the columns that are not "lines" (ie. where values are stored)
        columns.to.add <- grep('lines', names(out), invert = TRUE, value = TRUE)

        # turn list to df and replicate values for the size of df2
        fixed_df <- bind_rows(out[columns.to.add]) %>% slice(rep(1:n(), each = nrow(df2)))

        # combine the 2 df to obtain final SAG data
        final_df <- cbind(fixed_df, df2)
        
        # renumber the rows
        rownames(final_df) = NULL

        datalist[[i]] <- final_df
    # finally resimplify
    
  } else if (length(out) == 1) {
    final_df <- out[[1]]
    datalist[[i]] <- final_df
  }
  print(paste0("Getting data for assessmentKey:", assessmentkey[i]))
    }
    big_data <- data.table::rbindlist(datalist, fill=TRUE)
    # big_data = do.call(rbind, datalist)

    big_data <- big_data %>% rename(AssessmentKey = assessmentKey,
                                    StockPublishNote = stockPublishNote,
                                    Purpose = purpose,
                                    fishstock = fishStock, 
                                    AssessmentYear = assessmentYear, 
                                    units = units,
                                    stockSizeDescription = stockSizeDescription,
                                    stockSizeUnits = stockSizeUnits,
                                    fishingPressureDescription = fishingPressureDescription,
                                    Year = year,
                                    recruitment = recruitment,
                                    high_recruitment = high_Recruitment,
                                    low_recruitment = low_Recruitment,
                                    low_SSB = low_SSB,
                                    SSB = ssb,
                                    high_SSB = high_SSB,
                                    catches = catches,
                                    landings = landings,
                                    discards = discards,
                                    IBC = ibc,
                                    Unallocated_removals = unallocated_Removals,
                                    low_F = low_F,
                                    F = f,
                                    high_F = high_F,
                                    Fage = fAge,
                                    recruitment_age = recruitmentAge,
                                    fishingPressureUnits = fishingPressureUnits                            
                                    )

    return(big_data)
}


df <- getSAGSummary(2022)

ass <- findAssessmentKey(year = 2022)

update_SAG <- function(year){
    mkdir(paste0("Data/SAG_", year))

    # lookup for assessment key for summary
    sag <-
      getListStocks(year = year) %>%
      select(AssessmentKey, StockKeyLabel, AssessmentYear, Purpose, StockDescription, ModifiedDate, SAGStamp, LinkToAdvice) %>%
      rename(fishstock = StockKeyLabel)

    summary <- getSAGSummary(year, combine = TRUE) %>%
      left_join(sag, by = c("fishstock", "AssessmentYear", "Purpose"))

    write.taf(summary, file = "SAG_summary.csv", dir = paste0("Data/SAG_", year), quote = TRUE)

    refpts <- load_sag_refpts(year)
    write.taf(refpts, file = "SAG_refpts.csv", dir = paste0("Data/SAG_", year))
}
update_SAG(2022)
