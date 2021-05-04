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
#                 "http://sd.ices.dk/services/odata4/StockListDWs4?$filter=ActiveYear eq 2020&$select=AssessmentKey,DataCategory,StockKey, StockKeyLabel, EcoRegion, SpeciesScientificName,  SpeciesCommonName, ExpertGroup"
#                 #"http://sd.ices.dk/services/odata4/StockListDWs4?$filter=ActiveYear eq 2021"
#             )
#         )$value

# stock_list_all  %>% tibble()


# data <- access_sag_data("ory.27.nea",2020)
# data %>% tibble()
# source("Shiny/utilities_plotting.r")
# figure_1_catches(data, data$Year, data$catches, data$landings, data$discards)

# if (all(is.na(data[,'landings']))){
# data$landings <- data$catches
# }
# fig1 <- plot_ly(
#         data = data,
#         x = ~Year,
#         y = ~landings,
#         name = "Landings",
#         type = "bar",
#         hoverinfo = 'text',
#         text = ~paste('Year:', Year, '<br>Landings:', landings),
#         marker = list(color = '#66a4cd',
#                       line = list(color = 'black',
#                                   width = 0.5)),
#         showlegend = TRUE)

#     fig1 <- fig1 %>% add_trace(
#         data = data,
#         x = ~Year,
#         y = ~discards,
#         name = "Discards",
#         type = "bar",
#         hoverinfo = 'text',
#         text = ~paste('Year:', Year, '<br>Discards:', discards),
#         marker = list(color = '#a00130',
#                         line = list(color = 'black',
#                                     width = 0.5)),
#         showlegend = TRUE)

#     fig1 <- fig1 %>% layout(title = "Catches",
#             xaxis = list(title = "Years",
#             titlefont = titlefont_format(),
#             tickfont = tickfont_format(),
#             showticklabels = TRUE),
#             barmode = "stack",
#             legend = legend_format(),
#             yaxis = list(title = "Catches",
#             titlefont = titlefont_format(),
#             tickfont = tickfont_format(),
#             showticklabels = TRUE))
#     fig1
