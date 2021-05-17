access_sag_data <- function(stock_code, year) {

    # Dowload the data
    SAGsummary <- getSAG(stock_code, year,
        data = "summary", combine = TRUE, purpose = "Advice"
    )
    SAGrefpts <- getSAG(stock_code, year,
        data = "refpts", combine = TRUE, purpose = "Advice"
    )

    data_sag <- cbind(SAGsummary, SAGrefpts)
    data_sag <- subset(data_sag, select = -fishstock)
    #print(data_sag %>% tibble())
}

# stock_list_all <- jsonlite::fromJSON(
#             URLencode(
#                 #"http://sd.ices.dk/services/odata4/StockListDWs4?$filter=ActiveYear eq 2020&$select=AssessmentKey,DataCategory,StockKey, StockKeyLabel, EcoRegion, SpeciesScientificName,  SpeciesCommonName, ExpertGroup"
#                 "http://sd.ices.dk/services/odata4/StockListDWs4?$filter=ActiveYear eq 2021"
#                 #"http://sd.ices.dk/services/odata4/StockListDWs4?$filter=ActiveYear eq 2021&$DataCategory eq 1"
#             )
#         )$value
# stock_list_cat1 <- stock_list_all  %>% filter(DataCategory == "1")
# stock_list_all  %>% tibble()


# function to dowload the quality assessemnt data
quality_assessment_data <- function(stock_code){

years <- c(2021, 2020, 2019, 2018, 2017)
datalist = list()

for (i in years) {
    print(i)
    data_temp <- try(access_sag_data(stock_code, i)) # "had.27.6b"

    ###############
    if (isTRUE(class(data_temp) == "try-error")) {
        next
    }
    else {
        #
        data_temp <- filter(data_temp, between(Year, 2005, 2021))
        data_temp <- data_temp %>% select(Year, recruitment, SSB, F, Bpa, Blim, MSYBtrigger, FLim, Fpa, FMSY, RecruitmentAge, AssessmentYear)
        datalist[[i]] <- data_temp
        # }
    }
}


### bind data in unique df
big_data <- dplyr::bind_rows(datalist)

# find last asseement year
last_year <- tail(big_data$AssessmentYear, n=1)
big_data_last_year <- big_data  %>% filter(AssessmentYear == last_year)

#make assessmentYear as factor
big_data$AssessmentYear <- as.factor(big_data$AssessmentYear)
big_data_last_year$AssessmentYear <- as.factor(big_data_last_year$AssessmentYear)

df_list <- list(big_data, big_data_last_year)
return(df_list)
}
# list_df<-quality_assessment_data("her.27.3a47d")
