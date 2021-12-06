update_SAG <- function(year){
    mkdir(paste("SAG_", year))
    summary <- load_sag_summary(year)
    write.taf(summary, file = "SAG_summary.csv", dir = paste("SAG_", year))

    refpts <- load_sag_refpts(year)
    write.taf(refpts, file = "SAG_refpts.csv", dir = paste("SAG_", year))
}


## Ideally, this function would run every hour on the server to update sag
## it will take several minuts to run it locally for all the years, for now I will leave this
# function commented out, so it does not waste time when teh app is run locally
years <- c(2021, 2020, 2019, 2018, 2017)
for (year in years) {
    update_SAG(year)
}