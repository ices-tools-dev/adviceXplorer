server <- function(input, output) {
    
    # shape <- read_sf(dsn = "D:/Profile/Documents/GitHub/VISA_tool/R/explorationVisa/Shiny/ICES_ecoregions", 
    # layer = "ICES_ecoregions_20171207_erase_ESRI")
    ecoR_lowres <- st_read(
        dsn = "Shiny/test_lowres",
        layer = "ecoR_lowres"
    )
    levels(ecoR_lowres$Ecoregion)[match("Icelandic Waters", levels(ecoR_lowres$Ecoregion))] <- "Iceland Sea"

    ecoregion = "Celtic Seas Ecoregion"
    eu <- area_definition(ecoregion)
    eu_shape <- eu$europe_shape

    
    
    bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
    pal <- colorBin("YlOrRd", domain = ecoR_lowres$Shape_Area, bins = bins)

    labels <- sprintf(
        "<strong>%s Ecoregion</strong><br/>%g Shape Area ",
        ecoR_lowres$Ecoregion, ecoR_lowres$Shape_Area
    ) %>% lapply(htmltools::HTML)

    output$mymap <- renderLeaflet({
        leaflet() %>%
            addPolygons(
                data = ecoR_lowres, color = "#444444", weight = 1,
                smoothFactor = 0.5,
                opacity = 0.7, fillOpacity = 0.5,
                fillColor = ~ pal(ecoR_lowres$Shape_Area),
                # fillColor = ~qpal(ecoR_lowres$Shape_Area),
                highlightOptions = highlightOptions(
                    color = "white", weight = 2,
                    bringToFront = TRUE
                ),
                label = labels,
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"
                )
            ) %>%
            addPolygons(
                data = eu_shape, color = "black", weight = 1,
                smoothFactor = 0.5,
                opacity = 0.7, fillOpacity = 0.5,
                fillColor = "grey"
            ) %>%
            setView(lng = 25.783660, lat = 71.170953, zoom = 3) # nordKap coordinates
    })

  
    ######################### Advice panel
    SAGsummary <- getSAG("cod.27.22-24", 2018,
    data = "summary", combine = TRUE, purpose = "Advice"
    )
    SAGrefpts <- getSAG("cod.27.22-24", 2018,
    data = "refpts", combine = TRUE, purpose = "Advice"
    )

    data <- cbind(SAGsummary, SAGrefpts)
    data <- subset(data, select = -fishstock)


  rv <- reactiveValues(
      catches = data %>% select(Year, catches, landings, discards) %>% na.omit(),
      R = data %>% select(Year, low_recruitment, recruitment, high_recruitment)
          %>% na.omit(),
      f = data %>% select(Year, low_F, F, high_F, FLim, Fpa, FMSY) %>% na.omit(),
      SSB = data %>% select(Year, low_SSB, SSB, high_SSB, Blim, Bpa, MSYBtrigger) %>% na.omit()
  )
  #observeEvent(input$renorm, { rv$norm <- rnorm(500) })
  #observeEvent(input$reunif, { rv$unif <- runif(500) })
  #observeEvent(input$rechisq, { rv$chisq <- rchisq(500, 2) })
  
  output$catches <- renderPlotly({
      
      figure_1_catches(rv$catches, rv$catches$Year, rv$catches$landings, rv$catches$discards)
      
      #### First plot

#     fig1 <- plot_ly(
#         data = rv$catches,
#         x = ~Year,
#         y = ~landings,
#         name = "landings",
#         type = "bar",
#         hoverinfo = 'text',
#         text = ~paste('Year:', Year, '<br>Landings (tons):', landings),
#         marker = list(color = '#66a4cd',
#                       line = list(color = 'black',
#                                   width = 0.5)),
#         showlegend = TRUE
#                                   )
#     fig1 <- fig1 %>% add_trace(
#         data = rv$catches,
#         x = ~Year,
#         y = ~discards,
#         name = "discards",
#         type = "bar",
#         hoverinfo = 'text',
#         text = ~paste('Year:', Year, '<br>Discards (tons):', discards),
#         marker = list(color = '#a00130',
#                         line = list(color = 'black',
#                                     width = 0.5)),
#         showlegend = TRUE
#                                     )
  
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

# fig1
  })

  output$R <- renderPlotly({
      figure_2_recruitment(rv$R, rv$R$Year, rv$R$recruitment,rv$R$low_recruitment,rv$R$high_recruitment)
     #### second plot
#     fig2 <- plot_ly(
#         data = rv$R,
#         x = ~Year,
#         y = ~recruitment,
#         name = "recruitment",
#         type = "bar",
#         hoverinfo = "text",
#         text = ~ paste("Year:", Year, "<br>recruitment:", recruitment),
#         marker = list(
#             color = "#cd6666",
#             line = list(
#                 color = "black",
#                 width = 0.5
#             )
#         ),
#         error_y = list(
#             array = ~err,
#             type = "data",
#             color = "#000000"
#         )
#     )

#     fig2 <- fig2 %>% layout(
#         title = "Recruitment",
#         xaxis = list(
#             title = "Years",
#             titlefont = titlefont_format(),
#             tickfont = tickfont_format(),
#             showticklabels = TRUE
#         ),
#         yaxis = list(
#             title = "recruitment",
#             titlefont = titlefont_format(),
#             tickfont = tickfont_format(),
#             showticklabels = TRUE
#         )
#     )

# fig2 
  })

  output$f <- renderPlotly({
      #### third plot
      figure_3_fish_mortality(rv$f, rv$f$Year, rv$f$low_F, rv$f$F, rv$f$high_F, rv$f$FLim, rv$f$Fpa, rv$f$FMSY)

## Add high low and F
# fig3 <- plot_ly(
#     data = rv$f, x = ~Year, y = ~high_F, type = "scatter", mode = "lines",
#     line = list(color = "transparent", shape = "spline"),
#     showlegend = FALSE, name = "high_F"
# )
# fig3 <- fig3 %>% add_trace(
#     data = rv$f, y = ~low_F, type = "scatter", mode = "lines",
#     fill = "tonexty", fillcolor = "rgba(0,100,80,0.2)",
#     line = list(color = "transparent", shape = "spline"),
#     showlegend = FALSE, name = "low_F"
# )
# fig3 <- fig3 %>% add_trace(
#     data = rv$f, x = ~Year, y = ~F, type = "scatter", mode = "lines+markers",
#     line = list(color = "rgb(0,100,80)", shape = "spline"), name = "F",
#     marker = list(size = 10), showlegend = TRUE
# )

# ## Add horizontal lines
# fig3 <- fig3 %>% add_trace(
#     data = rv$f, x = ~Year, y = ~FLim, name = "FLim", type = "scatter", mode = "lines",
#     line = list(color = "black", shape = "linear", dash = "dash"), showlegend = TRUE
# )
# fig3 <- fig3 %>% add_trace(
#     data = rv$f, x = ~Year, y = ~Fpa, name = "Fpa", type = "scatter", mode = "lines",
#     line = list(color = "black", shape = "linear", dash = "dot"), showlegend = TRUE
# )
# fig3 <- fig3 %>% add_trace(
#     data = rv$f, x = ~Year, y = ~FMSY, name = "FMSY", type = "scatter", mode = "lines",
#     line = list(color = "orange", shape = "linear", dash = "dash"), showlegend = TRUE
# )

# fig3 <- fig3 %>% layout(
#     title = "F", legend = legend_format(),
#     paper_bgcolor = "rgb(255,255,255)", plot_bgcolor = "rgb(229,229,229)",
#     xaxis = list(
#         title = "Years",
#         gridcolor = "rgb(255,255,255)",
#         showgrid = TRUE,
#         showline = TRUE,
#         showticklabels = TRUE,
#         tickcolor = "rgb(127,127,127)",
#         ticks = "outside",
#         zeroline = TRUE,
#         titlefont = titlefont_format(),
#         tickfont = tickfont_format()
#     ),
#     yaxis = list(
#         title = "F",
#         gridcolor = "rgb(255,255,255)",
#         showgrid = TRUE,
#         showline = TRUE,
#         showticklabels = TRUE,
#         tickcolor = "rgb(127,127,127)",
#         ticks = "outside",
#         zeroline = TRUE,
#         titlefont = titlefont_format(),
#         tickfont = tickfont_format()
#     )
# )

# fig3
  })
  output$SSB <- renderPlotly({
      ### forth plot
      figure_4_SSB(SSB, SSB$Year, SSB$low_SSB, SSB$SSB, SSB$high_SSB, SSB$Blim, SSB$Bpa, SSB$MSYBtrigger)

## Add high low and SSB
# fig4 <- plot_ly(
#     data = rv$SSB, x = ~Year, y = ~high_SSB, type = "scatter", mode = "lines",
#     line = list(color = "transparent", shape = "spline"),
#     showlegend = FALSE, name = "high_SSB"
# )
# fig4 <- fig4 %>% add_trace(
#     data = rv$SSB, y = ~low_SSB, type = "scatter", mode = "lines",
#     fill = "tonexty", fillcolor = "rgba(0,100,80,0.2)",
#     line = list(color = "transparent", shape = "spline"),
#     showlegend = FALSE, name = "low_SSB"
# )
# fig4 <- fig4 %>% add_trace(
#     data = rv$SSB, x = ~Year, y = ~SSB, type = "scatter", mode = "lines+markers",
#     line = list(color = "rgb(0,100,80)", shape = "spline"), name = "SSB",
#     marker = list(size = 10), showlegend = TRUE
# )

# ## Add horizontal lines
# fig4 <- fig4 %>% add_trace(
#     data = rv$SSB, x = ~Year, y = ~Blim, name = "Blim", type = "scatter", mode = "lines",
#     line = list(color = "black", shape = "linear", dash = "dash"), showlegend = TRUE
# )
# fig4 <- fig4 %>% add_trace(
#     data = rv$SSB, x = ~Year, y = ~Bpa, name = "Bpa", type = "scatter", mode = "lines",
#     line = list(color = "black", shape = "linear", dash = "dot"), showlegend = TRUE
# )
# fig4 <- fig4 %>% add_trace(
#     data = rv$SSB, x = ~Year, y = ~MSYBtrigger, name = "MSYBtrigger", type = "scatter", mode = "lines",
#     line = list(color = "orange", shape = "linear", dash = "dash"), showlegend = TRUE
# )

# fig4 <- fig4 %>% layout(
#     title = "SSB", legend = legend_format(),
#     paper_bgcolor = "rgb(255,255,255)", plot_bgcolor = "rgb(229,229,229)",
#     xaxis = list(
#         title = "Years",
#         gridcolor = "rgb(255,255,255)",
#         showgrid = TRUE,
#         showline = TRUE,
#         showticklabels = TRUE,
#         tickcolor = "rgb(127,127,127)",
#         ticks = "outside",
#         zeroline = TRUE,
#         titlefont = titlefont_format(),
#         tickfont = tickfont_format()
#     ),
#     yaxis = list(
#         title = "SSB",
#         gridcolor = "rgb(255,255,255)",
#         showgrid = TRUE,
#         showline = TRUE,
#         showticklabels = TRUE,
#         tickcolor = "rgb(127,127,127)",
#         ticks = "outside",
#         zeroline = TRUE,
#         titlefont = titlefont_format(),
#         tickfont = tickfont_format()
#     )
# )


# fig4
  })
}
