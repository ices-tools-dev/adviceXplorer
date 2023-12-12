
Year <- 2023

StockList <- jsonlite::fromJSON(
      URLencode(
        sprintf("https://sag.ices.dk/SAG_API/api/StockList?year=%s", Year) 
      )
    )

# AssessmentKey <- 18223
StockList$AssessmentKey

SAGdf <- jsonlite::fromJSON(
      URLencode(
        sprintf("https://sag.ices.dk/SAG_API/api/StockDownload?assessmentKey=%s", AssessmentKey) 
      )
    )
names(SAGdf)
write.csv(SAGdf, "test2.csv")
