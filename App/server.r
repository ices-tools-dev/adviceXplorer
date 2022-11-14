
msg <- function(...) {
  emph <- "\n****************\n"
  cat(emph, ..., emph)
}


# required if using most recent version of sf
sf::sf_use_s2(FALSE)

options(icesSAG.use_token = FALSE)




############# Start server function ################

server <- function(input, output, session) {
  msg("server loop start:\n  ", getwd())

  help_server(input, output, session)

  # values of the query string and first visit flag
  query <- reactiveValues(query_from_table = FALSE)

  map_panel_server(input, output, session)
  
  ###########################################################  function to use the input from the maps and the sid filtering


  # Update the year of selection

  updateSelectizeInput(session,
    inputId = "selected_years",
    label = "Year",
    choices = Years$Year,
    selected = 2021
  )


  eco_filter <- reactive({
    req(input$selected_locations, input$selected_years)
    
    stock_list_long <- fread(sprintf("Data/SID_%s/SID.csv", input$selected_years))
    stock_list_long <- stock_list_long %>% drop_na(AssessmentKey)

    ### reshuffle some columns    
    stock_list_long <- stock_list_long %>%
      relocate(icon, .before = SpeciesCommonName) %>% 
      relocate(c(doi, FO_doi), .before = EcoRegion) %>%
      relocate(group_url, .before = DataCategory) %>%
      relocate(c(doi, FO_doi), .before = AssessmentKey) 
     

    temp_df <- data.frame()
    for (i in 1:length(input$selected_locations)) {
      temp_1 <- stock_list_long %>% filter(str_detect(EcoRegion, input$selected_locations[i]))
      temp_df <- rbind(temp_df, temp_1)
    }

    stock_list_long <- temp_df
    stock_list_long <- stock_list_long %>% arrange(StockKeyLabel)
    stock_list_long$Select <- sprintf('<input type="radio" name="rdbtn" value="rdbtn_%s"/>', 1:nrow(stock_list_long))
    stock_list_long <- stock_list_long %>%
      relocate(Select, .before = StockKeyLabel)
    
  })

  
  res_mod <- callModule(
    module = selectizeGroupServer,
    id = "my-filters",
    # data = separate_ecoregions(stock_list_all, selected_1$groups),
    data = eco_filter,
    vars = c(
      "StockKeyLabel", "SpeciesCommonName",
      "ExpertGroup", "DataCategory", "YearOfLastAssessment",
      "AdviceCategory"
    )
  )
  

  #### last update of the app 
  output$app_last_update <- renderUI({
    make_app_update_date()
  })
  ###########################################################  Render table in stock selection tab

  output$tbl <- DT::renderDT(
    
    res_modo <- res_mod() %>% rename("Select" = Select,
                                      "Stock code" = StockKeyLabel,
                                      "Ecoregion" = EcoRegion,
                                      " " = icon,
                                      "Common name" = SpeciesCommonName,
                                      "Expert group" = group_url,
                                      "Data category" = DataCategory,
                                      "Year of last assessment" = YearOfLastAssessment,
                                      "Advice category" = AdviceCategory,
                                      "Advice doi" = doi,
                                      "Fisheries Overview doi" = FO_doi,
                                      "Assessment data" = SAG_url,
                                      "GIS data" = visa_url),
    
    escape = FALSE,
    selection = 'none', 
    server = FALSE,    
    caption = "Select the fish stock of interest and then click on one of panels on the right",
    options = list(
      order = list(2, "asc"),
      dom = "Bfrtip",
      pageLength = 300,
      columnDefs = list(
        list(visible = FALSE, targets = c(0, 6, 13)),
        list(className = "dt-center", targets = c(1, 4, 7, 11, 12, 14, 15))
      )
    ),
    callback = JS(callback1(res_mod()))
)
  
  

  ## process radio button selection
  observeEvent(input$rdbtn, {
    
    filtered_row <- res_mod()[str_detect(res_mod()$Select, regex(paste0("\\b", input$rdbtn,"\\b"))), ]
    print(filtered_row$SpeciesCommonName)
    
    updateQueryString(paste0("?assessmentkey=", filtered_row$AssessmentKey), mode = "push") ####

    ###

    query$query_from_table <- TRUE

    msg("stock selected from table:", filtered_row$StockKeyLabel)
    msg("year of SAG/SID selected from table:", input$selected_years) #####
  })



  observe({
    # read url string
    query_string <- getQueryString()
    names(query_string) <- tolower(names(query_string))    

    query$assessmentkey <- query_string$assessmentkey

    if (!is.null(query$assessmentkey) && !query$query_from_table) {
      info <- getFishStockReferencePoints(query$assessmentkey)[[1]]

      query$stockkeylabel <- info$StockKeyLabel
      query$year <- info$AssessmentYear ####

      msg("stock selected from url:", query$stockkeylabel)
      msg("year of SAG/SID selected from url:", query$year) #####

      updateNavbarPage(session, "tabset", selected = "Development over time")
    }
  })



  ######### SAG data
  SAG_data_reactive <- eventReactive(req(query$assessmentkey), {
    info <- getFishStockReferencePoints(query$assessmentkey)[[1]]
    query$stockkeylabel <- info$StockKeyLabel
    query$year <- info$AssessmentYear ####

    stock_name <- query$stockkeylabel
    msg("downloading:", stock_name)

    year <- query$year #####
    msg("downloading:", year)
    #   # Dowload the data
    access_sag_data_local(stock_name, year)
  })

  sagSettings <- eventReactive(req(query$assessmentkey),{
    # options(icesSAG.use_token = TRUE)
    # icesSAG::getSAGSettingsForAStock(query$assessmentkey)
    getSAGSettings(query$assessmentkey)
  })

######## download IBC and unallocated_Removals (temporary solution until icesSAG is updated)
additional_LandingData <- eventReactive((req(query$assessmentkey)),{
  out <- jsonlite::fromJSON(
        URLencode(
            sprintf("https://sag.ices.dk/SAG_API/api/SummaryTable?assessmentKey=%s", query$assessmentkey)
        )
    )  
  data.frame(Year = out$lines$year, ibc = out$lines$ibc, unallocated_Removals = out$lines$unallocated_Removals)

})



##### get link to library pdf advice
advice_doi <- eventReactive((req(query$assessmentkey)),{
  # doi <- jsonlite::fromJSON(
  get_advice_doi(query$assessmentkey)

})

###### info about the stock selected for top of page
output$stock_infos1 <- renderUI({
  filtered_row <- res_mod()[str_detect(res_mod()$Select, regex(paste0("\\b", input$rdbtn,"\\b"))), ]  
  get_Stock_info(filtered_row$SpeciesCommonName, SAG_data_reactive()$StockKeyLabel[1],  SAG_data_reactive()$AssessmentYear[1]) #SAG_data_reactive()$StockDescription[1],
})

##### advice headline (right side of page)
advice_view_headline <- eventReactive(req(advice_view_info()), {
  get_Advice_View_Headline(advice_view_info())
})
output$Advice_Headline1 <- renderUI({
  advice_view_headline()  
})

#### link to pdf of advice
onclick("library_advice_link1", runjs(paste0("window.open('", advice_doi(),"', '_blank')")))


##### button to download SAG data
output$download_SAG_Data <- downloadHandler(
    filename = function() {
      paste("SAG_data-", Sys.Date(), ".csv", sep="")#### add species and year and data disclaimer
    },
    content = function(file) {
      write.csv(SAG_data_reactive(), file)
    }
  )

######################### Stock development over time plots

  output$plot1 <- renderPlotly(
    ICES_plot_1(SAG_data_reactive(), sagSettings(), additional_LandingData())

  ) # %>%
  # bindCache(SAG_data_reactive(), SAG_stamp(), cache = "session")

  output$plot2 <- renderPlotly({
    validate(
      need(SAG_data_reactive()$recruitment != "", "Data not available for this stock")
    )
    ICES_plot_2(SAG_data_reactive(), sagSettings())
  })
  
  output$plot3 <- renderPlotly({
    validate(
      need(SAG_data_reactive()$F != "", "Data not available for this stock")
    )

    ICES_plot_3(SAG_data_reactive(), sagSettings())
  })
  
  output$plot4 <- renderPlotly({
    validate(
      need(SAG_data_reactive()$SSB != "", "Data not available for this stock")
    )
    ICES_plot_4(SAG_data_reactive(), sagSettings())
  })


####################### Quality of assessment data
  advice_action_quality <- eventReactive(req(query$assessmentkey,query$year), {
    info <- getFishStockReferencePoints(query$assessmentkey)[[1]]
    query$stockkeylabel <- info$StockKeyLabel
    query$year <- info$AssessmentYear ####

    stock_name <- query$stockkeylabel
    # msg("downloading:", stock_name)

    year <- query$year #####
    # msg("downloading:", year)
    #   # Dowload the data
    quality_assessment_data_local(stock_name, year)
  })


###### info about the stock selected for top of page
output$stock_infos2 <- renderUI({
  filtered_row <- res_mod()[str_detect(res_mod()$Select, regex(paste0("\\b", input$rdbtn,"\\b"))), ]  
  get_Stock_info(filtered_row$SpeciesCommonName, SAG_data_reactive()$StockKeyLabel[1],  SAG_data_reactive()$AssessmentYear[1]) #SAG_data_reactive()$StockDescription[1],
})

output$Advice_Headline2 <- renderUI({
  advice_view_headline()  
})
##### button to download SAG data for quality of assessemnt
  output$download_SAG_Quality_Data <- downloadHandler(
    filename = function() {
      paste("SAG_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(advice_action_quality(), file)
    }
  )

#### link to pdf of advice
onclick("library_advice_link2", runjs(paste0("window.open('", advice_doi(),"', '_blank')")))

  ######################### quality of assessment plots
  output$plot5 <- renderPlotly({
    validate(
      need(advice_action_quality()$SSB != "", "Data not available for this stock")
    )

    ICES_plot_5(advice_action_quality(), sagSettings())

  })
  output$plot6 <- renderPlotly({
    validate(
      need(advice_action_quality()$F != "", "Data not available for this stock")
    )

    ICES_plot_6(advice_action_quality(), sagSettings())

  })
  output$plot7 <- renderPlotly({
    validate(
      need(advice_action_quality()$recruitment != "", "Data not available for this stock")
    )
    ICES_plot_7(advice_action_quality())
  })
  

##### Advice view info
advice_view_info <- eventReactive(req(query$stockkeylabel,query$year), {
  get_Advice_View_info(query$stockkeylabel, query$year)
})


##### Advice view info previous year
advice_view_info_previous_year <- eventReactive(req(query$stockkeylabel,query$year), {
  get_Advice_View_info(query$stockkeylabel, query$year-1)
})



##### catch scenarios table
catch_scenario_table <- eventReactive(req(advice_view_info()), {
  standardize_catch_scenario_table(get_catch_scenario_table(advice_view_info()))
})

##### catch scenarios table previous year in percentages (for radial plot)
catch_scenario_table_previous_year <- eventReactive(req(advice_view_info_previous_year()), {
  standardize_catch_scenario_table(get_catch_scenario_table(advice_view_info_previous_year()))
  # scale_catch_scenarios_for_radialPlot(catch_scenario_table_previous_year, catch_scenario_table())
})

##### catch scenario table scaled with the values of previous advice to get percentage of change
catch_scenario_table_percentages <- eventReactive(req(catch_scenario_table_previous_year(),catch_scenario_table()), {
  # standardize_catch_scenario_table(get_catch_scenario_table(advice_view_info_previous_year()))
  scale_catch_scenarios_for_radialPlot(catch_scenario_table_previous_year(), catch_scenario_table())
})


#### link for the advice view link button to the full stock record
onclick("advice_view_link", runjs(paste0("window.open('https://sg.ices.dk/adviceview/viewAdvice/", advice_view_info()$adviceKey,"', '_blank')")))

#### link to pdf of advice
onclick("library_advice_link3", runjs(paste0("window.open('", advice_doi(),"', '_blank')")))

##### Advice and stock infos
# advice_view_summary <- eventReactive(req(advice_view_info()), {
#   get_Advice_View_Summary(advice_view_info(), SAG_data_reactive()$StockDescription[1])
# })
# output$Advice_Summary <- renderUI({
#   advice_view_summary()  
# }) 


#%>%
  # bindCache(advice_view_sentence(), advice_view_info())
###### info about the stock selected for top of page
output$stock_infos3 <- renderUI({
  filtered_row <- res_mod()[str_detect(res_mod()$Select, regex(paste0("\\b", input$rdbtn,"\\b"))), ]  
  get_Stock_info(filtered_row$SpeciesCommonName, SAG_data_reactive()$StockKeyLabel[1],  SAG_data_reactive()$AssessmentYear[1]) #SAG_data_reactive()$StockDescription[1],
})

##### advice headline (right side of page)
# advice_view_headline <- eventReactive(req(advice_view_info()), {
#   get_Advice_View_Headline(advice_view_info())
# })
output$Advice_Headline3 <- renderUI({
  advice_view_headline()  
})


### F_SSB and chatches plot linked to table
output$catch_scenario_plot_3 <- renderPlotly({
  
  validate(
      need(!is_empty(catch_scenario_table()), "Data not available for this stock")
    )
  tmp <- arrange(catch_scenario_table(), F)
  catch_scenarios_plot2(tmp, SAG_data_reactive())
}) #%>%
  # bindCache(catch_scenario_table(), SAG_data_reactive())



########## Historical catches panel (preparation of data)
test_table <- eventReactive(catch_scenario_table(), {
  req(query$stockkeylabel, query$year)
  validate(
    need(!is_empty(catch_scenario_table()), "Data not available for this stock")
  )
  wrangle_catches_with_scenarios(access_sag_data_local(query$stockkeylabel, query$year), catch_scenario_table(), query$stockkeylabel, query$year)
})

########## Historical catches panel (Definition of basisi of advice)
Basis <- eventReactive(catch_scenario_table_percentages(),{
  validate(
    need(!is_empty(catch_scenario_table_percentages()), "Data not available for this stock")
  )
  catch_scenario_table_percentages()[catch_scenario_table_percentages()$cS_Purpose == "Basis Of Advice", ]
})

########## Historical catches panel (Selection panel)
output$catch_scenarios <- renderUI({  
  
  if (!is_empty(test_table())) {
  selectizeInput(
    inputId = "catch_choice",
    label = "Select a scenario",
    choices = unique(test_table()$cat),
    selected = c("Historical Catches", Basis()$cat),
    multiple = TRUE
  )
  } else {
    HTML("No data available")
  }
})

########## Historical catches panel (Plot)
output$TAC_timeline <- renderPlotly({
  # validate(
  #     need(!is_empty(test_table()), "Data not available for this stock")
  #   )
  TAC_timeline(test_table(), input$catch_choice, SAG_data_reactive())
})


############ Radial plot panel (Selection panel)
output$catch_scenarios_radial <- renderUI({
  if (!is_empty(catch_scenario_table_percentages())) {

    selectizeInput(
      inputId = "catch_choice_radial",
      label = "Select a scenario",
      choices = unique(catch_scenario_table_percentages()$cat),
      selected = c(Basis()$cat),
      multiple = TRUE
    )
  } else {
    HTML("No data available")
  }
})

############ Radial plot panel (radial plot)
output$Radial_plot <- renderPlotly({
  # validate(
  #   need(!is_empty(catch_scenario_table_percentages()), "Data not available for this stock")
  # )
  radial_plot(catch_scenario_table_percentages(), input$catch_choice_radial)
})


############ Lollipop plot panel (Selection panel) 
output$catch_indicators_lollipop <- renderUI({
  if (!is_empty(catch_scenario_table_percentages())) {
    
    selectizeInput(
      inputId = "indicator_choice_lollipop",
      label = "Select an indicator",
      choices = names(catch_scenario_table_percentages()),
      selected = c("F"),
      multiple = TRUE
    )
  } else {
    HTML("No data available")
  }
})

############ Lollipop plot panel (Lollipop plot) 
output$Lollipop_plot <- renderPlotly({
  # validate(
  #     need(!is_empty(catch_scenario_table_percentages()), "Data not available for this stock")
  #   )
  lollipop_plot(catch_scenario_table_percentages(),input$indicator_choice_lollipop)
})



###### Calendar of stock with modal
observeEvent(input$preview, {
    # Show a modal when the button is pressed
    shinyalert(title= " Advice Calendar", 
    
    tags$body(HTML(html_calendar(advice_view_info(), res_mod(), input$rdbtn))),
            type = "info",
            html=TRUE,
            closeOnClickOutside = TRUE,
            confirmButtonText = "Close",
            size = "s",
            )
  })

# addTooltip(session=session,id="help_tab5",title="Link to Advice View record") # not working



############### Catch scenario plot
catch_table_names <- eventReactive(catch_scenario_table(),{
  req(query$stockkeylabel, query$year)
  gsub("â€“", " - ",names(fread(file = "Data/catch_scen_col_names.txt", sep = ",")), fixed = TRUE)

})

catch_scenario_table_collated <- eventReactive(catch_scenario_table(),{
  validate(
      need(!is_empty(catch_scenario_table()), "Data not available for this stock")
    )
    catch_scenario_table() %>%
    arrange(F) %>%
    rename_all(funs(catch_table_names())) %>%
    rename("Basis" = cS_Label, " " = cS_Purpose)
})

output$table <- DT::renderDT(
  # tab <- catch_scenario_table() %>%
  #   arrange(F) %>%
  #   rename_all(funs(catch_table_names())) %>%
  #   rename("Basis" = cS_Label, " " = cS_Purpose),

catch_scenario_table_collated(),
  # tab <- catch_scenario_table() %>%
  #   arrange(F) %>%
  #   rename_all(funs(catch_table_names())) %>%
  #   rename("Basis" = cS_Label, " " = cS_Purpose),
 
  # arrange(catch_scenario_table(), F) %>% select(-Year),
  selection = "single",
  class = "display",
  caption = "Subset of catch scenario table",
  rownames = FALSE,
  options = list(
    # order = list("cS_Purpose", "asc"),
    dom = "Bfrtip",
    pageLength = 100,
    buttons =
      list("copy", "print", list(
        extend = "collection",
        buttons = c("csv", "excel"),
        text = "Download"
      )),
    columnDefs = list(
      list(visible = FALSE, targets = c(0))
    )
  ),
  callback = JS("table.on('mouseover', 'td', function() {
                              $(this).parent().addClass('hover')
                              });
                              table.on('mouseout', 'td', function() {
                              $(this).parent().removeClass('hover')
                              });
                         return table;
                          ")
)

##### connection between F/SSB plot points and table rows 
table_proxy = dataTableProxy('table')

selected_scenario <- reactive({
      if (is.null(event_data("plotly_hover", source = "ranking")))
        return(NULL)
      event_data("plotly_hover", source = "ranking")
      })

    observe({
      selectRows(table_proxy, selected=(selected_scenario()[[2]]+1))
    })


##### footnotes of catch scenario table
footnotes <- eventReactive(req(advice_view_info()), {
  get_catch_scenario_notes(advice_view_info())
})
output$footnotes <-renderUI({
  validate(
      need(!is_empty(footnotes()), " ")
    )
  
  footnotes()
  })


##### Last page text, citation, data usage, feedback etcc
output$citation <- renderUI({
  make_app_citation()
  
})





}
