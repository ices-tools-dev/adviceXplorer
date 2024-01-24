
msg <- function(...) {
  emph <- "\n****************\n"
  cat(emph, ..., emph)
}


# required if using most recent version of sf
sf::sf_use_s2(FALSE)

options(spinner.type = 5, 
        spinner.color = "#f15d22")





############# Start server function ################

server <- function(input, output, session) {
  msg("server loop start:\n  ", getwd())
  shinyjs::disable(selector = '.navbar-nav a[data-value="Development over time"')
  shinyjs::disable(selector = '.navbar-nav a[data-value="Quality of assessment"')
  shinyjs::disable(selector = '.navbar-nav a[data-value="Catch scenarios"')
  
  # values of the query string and first visit flag
  query <- reactiveValues(query_from_table = FALSE)

  map_panel_server(input, output, session)
  
  ###########################################################  function to use the input from the maps and the sid filtering


  # Update the year of selection

  eco_filter <- reactive({
    req(input$selected_locations, input$selected_years)
    
    stock_list_long <- fread(sprintf("Data/SID_%s/SID.csv", input$selected_years))
    stock_list_long[stock_list_long$EcoRegion == "Iceland Sea Ecoregion", "EcoRegion"] <- "Icelandic Waters Ecoregion"
    stock_list_long <- stock_list_long %>% drop_na(AssessmentKey)
    stock_list_long <- purrr::map_dfr(
      .x = input$selected_locations,
      .f = function(.x) stock_list_long %>% dplyr::filter(str_detect(EcoRegion, .x))
    )
    
    if (nrow(stock_list_long) != 0) {
    stock_list_long %>% 
      dplyr::arrange(StockKeyLabel) %>%
      dplyr::mutate(
        EcoRegion = removeWords(EcoRegion, "Ecoregion"),
        Select = sprintf('<input type="radio" name="rdbtn" value="rdbtn_%s"/>', 1:nrow(.)),
        stock_description = purrr::map_chr(StockKeyLabel, .f = ~ access_sag_data_local(.x, input$selected_years)$StockDescription[1]),
        stock_location = parse_location_from_stock_description(stock_description)
      )
  }
  }) %>%
    bindCache(input$selected_locations, input$selected_years) %>%
    bindEvent(input$selected_locations, input$selected_years)


  res_mod <- select_group_server(
    id = "my-filters",
    data = eco_filter,
    vars = reactive(c("StockKeyLabel", "SpeciesCommonName"))
  )
  
  ###########################################################  Render stock selection table

  res_modo <- reactive({ 
    validate(
      need(!nrow(eco_filter()) == 0, "No published stocks in the selected ecoregion and year")
    )
   
   res_mod() %>% select(
      "Select",
      "StockKeyLabel",
      "EcoRegion",
      "icon",
      "SpeciesCommonName",
      "stock_location"
    ) %>%
      rename(
        "Select" = Select,
        "Stock code" = StockKeyLabel,
        "Ecoregion" = EcoRegion,
        " " = icon,
        "Common name" = SpeciesCommonName,
        "Location" = stock_location
      )
  })
  
  output$tbl <- DT::renderDT(
   
    res_modo(),
    escape = FALSE,
    selection = "none",
    server = FALSE,
    caption = HTML("<b><font size= 6> Stock selection</b></font></br><font size= 5> To select a stock, click on the corresponding button in the 'Select' column. </font>"),
    options = list(
      order = list(2, "asc"),
      dom = "Bfrtip",
      pageLength = 300,
      columnDefs = list(
        list(visible = FALSE, targets = c(0, 3)),
        list(className = "dt-center", targets = c(1, 4))
      )
    ),
    callback = JS(callback)
  )
  
  

  ## process radio button selection
  observeEvent(input$rdbtn, {
    shinyjs::enable(selector = '.navbar-nav a[data-value="Development over time"')
    shinyjs::enable(selector = '.navbar-nav a[data-value="Quality of assessment"')
    shinyjs::enable(selector = '.navbar-nav a[data-value="Catch scenarios"')
    
    filtered_row <- res_mod()[str_detect(res_mod()$Select, regex(paste0("\\b", input$rdbtn,"\\b"))), ]
        
    updateQueryString(paste0("?assessmentkey=", filtered_row$AssessmentKey), mode = "push") ####

    query$query_from_table <- TRUE

    msg("stock selected from table:", filtered_row$StockKeyLabel)
    msg("year of SAG/SID selected from table:", input$selected_years) #####

    ### this allow to trigger the "Development over time" tab when the radio button is clicked
    updateNavbarPage(session, "tabset", selected = "Development over time")
    
  })

  
  observe({
    # read url string
    query_string <- getQueryString()
    names(query_string) <- tolower(names(query_string))    

    query$assessmentkey <- query_string$assessmentkey

    if (!is.null(query$assessmentkey) && !query$query_from_table) {
      
      info <- FishStockReferencePoints(query$assessmentkey)

      query$stockkeylabel <- info$StockKeyLabel
      query$year <- info$AssessmentYear 

      msg("stock selected from url:", query$stockkeylabel)
      msg("year of SAG/SID selected from url:", query$year)

      updateNavbarPage(session, "tabset", selected = "Development over time")
      shinyjs::enable(selector = '.navbar-nav a[data-value="Development over time"')
      shinyjs::enable(selector = '.navbar-nav a[data-value="Quality of assessment"')
      shinyjs::enable(selector = '.navbar-nav a[data-value="Catch scenarios"')
      
    }
  })

  ######### SAG data
  SAG_data_reactive <- reactive({
    
    info <- FishStockReferencePoints(query$assessmentkey)
    query$stockkeylabel <- info$StockKeyLabel
    query$year <- info$AssessmentYear ####

    stock_name <- query$stockkeylabel
    msg("downloading:", stock_name)

    year <- query$year #####
    msg("downloading:", year)
    
    #   # Dowload the data
    access_sag_data_local(stock_name, year) %>% filter(AssessmentKey == query$assessmentkey)
  }) 
  
  sagSettings <- reactive({
    temp_setting <- getSAGSettings(query$assessmentkey)
    temp_setting[!(temp_setting$settingValue == ""), ]

  })  
  
  drop_plots <- reactive({
      filter(sagSettings(), settingKey == 22 & settingValue == "yes" | settingValue == "y") %>%
      pull(SAGChartKey) %>%
      as.numeric})
  
######## download IBC and unallocated_Removals (temporary solution until icesSAG is updated)
additional_LandingData <- reactive({
  get_additional_landing_data(query$assessmentkey)
}) 

##### get link to library pdf advice
advice_doi <- eventReactive((req(query$assessmentkey)),{   
  get_advice_doi(query$assessmentkey)
})

replaced_advice_doi <- eventReactive(req(query$year, query$stockkeylabel), {
  get_link_replaced_advice(query$year, query$stockkeylabel)
})
###### info about the stock selected for top of page
stock_info <- reactive({
  filtered_row <- res_mod()[res_mod()$AssessmentKey == query$assessmentkey,] 
  get_Stock_info(filtered_row$SpeciesCommonName, SAG_data_reactive()$StockKeyLabel[1],  SAG_data_reactive()$AssessmentYear[1], SAG_data_reactive()$StockDescription[1]) #,
}) 

output$stock_infos1 <- output$stock_infos2 <- output$stock_infos3 <- renderUI(
  stock_info()
  )

##### advice headline (right side of page)
advice_view_headline <- reactive({
  get_Advice_View_Headline(advice_view_info(),  replaced_advice_doi(), input$tabset, catch_scenario_table()$table, drop_plots())
}) 

output$Advice_Headline1 <- output$Advice_Headline2 <- output$Advice_Headline3 <- renderUI({
  advice_view_headline()  
}) 

 ### link to pdf of advice (NOT ACTIVE)
onclick("library_advice_link1", runjs(paste0("window.open('", advice_doi(),"', '_blank')")))


output$download_SAG_Data <- downloadHandler(
    filename = paste0("adviceXplorer_data-", Sys.Date(), ".zip"),
    content = function(fname) {
      
      fs <- c("Disclaimer.txt", "adviceXplorer_SAG_data.csv")
      write.csv(SAG_data_reactive(), file = "adviceXplorer_SAG_data.csv")
      write.table(read.delim("https://raw.githubusercontent.com/ices-tools-prod/disclaimers/master/Disclaimer_adviceXplorer.txt"),  file = "Disclaimer.txt", row.names = FALSE)
      
      zip::zip(zipfile=fname, files=fs)
    },
    contentType = "application/zip"
  )





######################### Stock development over time plots

  output$plot1 <- renderPlotly({
     validate(
      need(c(SAG_data_reactive()$Landings,SAG_data_reactive()$Catches) != "", "Landings not available for this stock")#,
      # need(all(!c(0, 1) %in% drop_plots()), "Figure not included in the published advice for this stock")
    )
    suppressWarnings(ICES_plot_1(SAG_data_reactive(), sagSettings()))

})

  output$plot2 <- renderPlotly({
    validate(
      need(SAG_data_reactive()$Recruitment != "", "Recruitment not available for this stock")#,
      # need(all(!c(0, 2) %in% drop_plots()), "Figure not included in the published advice for this stock")
    )
    suppressWarnings(ICES_plot_2(SAG_data_reactive(), sagSettings()))
  })
  
  output$plot3 <- renderPlotly({
    validate(
      need(SAG_data_reactive()$F != "", "F not available for this stock")#,
      # need(all(!c(0, 3) %in% drop_plots()), "Figure not included in the published advice for this stock")
    )

    suppressWarnings(ICES_plot_3(SAG_data_reactive(), sagSettings()))
  })
  
  output$plot4 <- renderPlotly({
    validate(
      need(SAG_data_reactive()$SSB != "", "SSB not available for this stock")#,
      # need(all(!c(0,4) %in% drop_plots()), "Figure not included in the published advice for this stock")
      
    )
    suppressWarnings(ICES_plot_4(SAG_data_reactive(), sagSettings()))
  })


####################### Quality of assessment data
  advice_action_quality <- reactive({
    
    info <- FishStockReferencePoints(query$assessmentkey)
    query$stockkeylabel <- info$StockKeyLabel
    query$year <- info$AssessmentYear 

    stock_name <- query$stockkeylabel

    year <- query$year 
    
    quality_assessment_data_local(stock_name, year)
  }) 
  


##### button to download SAG data for quality of assessemnt
  output$download_QualAss_Data <- downloadHandler(
    filename = paste0("adviceXplorer_data-", Sys.Date(), ".zip"),
    content = function(fname) {
      
      fs <- c("Disclaimer.txt", "adviceXplorer_QualityofAssessment_data.csv")
      write.csv(advice_action_quality(), file = "adviceXplorer_QualityofAssessment_data.csv")
      write.table(read.delim("https://raw.githubusercontent.com/ices-tools-prod/disclaimers/master/Disclaimer_adviceXplorer.txt"),  file = "Disclaimer.txt", row.names = FALSE)
      
      zip::zip(zipfile=fname, files=fs)
    },
    contentType = "application/zip"
  )

#### link to pdf of advice (NOT ACTIVE)
onclick("library_advice_link2", runjs(paste0("window.open('", advice_doi(),"', '_blank')")))

  ######################### quality of assessment plots
  output$plot5 <- renderPlotly({
    validate(
      need(advice_action_quality()$SSB != "", "SSB not available for this stock"),
      need(all(!10 %in% drop_plots()), "Figure not included in the published advice for this stock")
    )

    suppressWarnings(ICES_plot_5(advice_action_quality(), sagSettings()))

  })
  output$plot6 <- renderPlotly({
    validate(
      need(advice_action_quality()$F != "", "F not available for this stock"),
      need(all(!10 %in% drop_plots()), "Figure not included in the published advice for this stock")
    )

    suppressWarnings(ICES_plot_6(advice_action_quality(), sagSettings()))

  })
  output$plot7 <- renderPlotly({
    validate(
      need(advice_action_quality()$Recruitment != "", "Recruitment not available for this stock"),
      need(all(!10 %in% drop_plots()), "Figure not included in the published advice for this stock")
    )
    suppressWarnings(ICES_plot_7(advice_action_quality(), sagSettings()))
  })
  

##### Advice view info
advice_view_info <- reactive({
  asd_record <- getAdviceViewRecord(assessmentKey = query$assessmentkey)
  if (!is_empty(asd_record)){ 
    asd_record <- asd_record %>% filter(adviceViewPublished == TRUE, adviceStatus == "Advice") 
  }  
}) 


##### Advice view info previous year
advice_view_info_previous_year <- eventReactive(req(query$stockkeylabel,query$year), {
  filtered_row <- res_mod()[res_mod()$AssessmentKey == query$assessmentkey,] 
  asd_record_previous <- getAdviceViewRecord(query$stockkeylabel, query$year - filtered_row$AssessmentFrequency)

  if (!is_empty(asd_record_previous)){ 
    asd_record_previous <- asd_record_previous %>% filter(adviceViewPublished == TRUE, adviceStatus == "Advice") 
  } 
})



##### catch scenarios table
catch_scenario_table <- eventReactive(req(advice_view_info()), {
  standardize_catch_scenario_table(icesASD::get_catch_scenario_table(advice_view_info()$adviceKey, query$year))
})

##### catch scenarios table previous year in percentages (for radial plot)
catch_scenario_table_previous_year <- eventReactive(req(advice_view_info_previous_year()), {
  standardize_catch_scenario_table(icesASD::get_catch_scenario_table(advice_view_info_previous_year()$adviceKey, query$year))
  
})

##### catch scenario table scaled with the values of previous advice to get percentage of change
catch_scenario_table_percentages <- eventReactive(req(catch_scenario_table_previous_year(),catch_scenario_table()), {
  validate(
    need(!is_empty(catch_scenario_table_previous_year()$table), "No catch scenario table in previous assessment year"),
    need(!is_empty(catch_scenario_table()$table), "No catch scenario table in this assessment year")
  )

  scale_catch_scenarios_for_radialPlot(catch_scenario_table_previous_year()$table, catch_scenario_table()$table)
})


#### link for the advice view link button to the full stock record (NOT ACTIVE)
onclick("advice_view_link", runjs(paste0("window.open('https://sg.ices.dk/adviceview/viewAdvice/", advice_view_info()$adviceKey,"', '_blank')")))

#### link to pdf of advice (NOT ACTIVE)
onclick("library_advice_link3", runjs(paste0("window.open('", advice_doi(),"', '_blank')")))


### F_SSB and chatches plot linked to table
output$catch_scenario_plot_F_SSB_Catch <- renderPlotly({
  
  validate(
      need(!is_empty(catch_scenario_table()$table), "Catch scenarios not available for this stock")
    )

  if (str_detect(tail(query$stockkeylabel), "nep")) {
    catch_scenario_plot_1_nephrops(catch_scenario_table(), SAG_data_reactive(), sagSettings())
  } else {
    catch_scenario_plot_1(catch_scenario_table(), SAG_data_reactive(), sagSettings())
  }
}) 

########## Historical catches panel (preparation of data)
test_table <- eventReactive(catch_scenario_table(), {
  req(query$stockkeylabel, query$year)
  validate(
    need(!is_empty(catch_scenario_table()$table), ""),
    need(!is_empty(advice_view_info_previous_year()), "No ASD entry in previous assessment year")
   
  )
  wrangle_catches_with_scenarios(access_sag_data_local(query$stockkeylabel, query$year), catch_scenario_table()$table, advice_view_info_previous_year(), query$stockkeylabel, query$year)
})

########## Historical catches panel (Definition of basis of advice)
Basis <- eventReactive(catch_scenario_table(),{
    
    catch_scenario_table()$table[catch_scenario_table()$table$cS_Purpose == "Basis Of Advice", ]

})

########## Historical catches panel (Selection panel)
output$catch_scenarios <- renderUI({  
  
  if (!is_empty(test_table())) {
    virtualSelectInput(
      inputId = "catch_choice",
      label = "Select one or more catch scenarios:",
      choices = unique(test_table()$cat),
      selected = c("Historical Catches", Basis()$cat),
      multiple = TRUE,
      width = "100%",
      search = TRUE
    )
  # selectizeInput(
  #   inputId = "catch_choice",
  #   label = "Select one or more catch scenarios",
  #   choices = unique(test_table()$cat),
  #   selected = c("Historical Catches", Basis()$cat),
  #   multiple = TRUE
  # )
  } else {
    HTML("No data available")
  }
})

########## Historical catches panel (Plot)
output$TAC_timeline <- renderPlotly({
  validate(
    need(!is_empty(catch_scenario_table()$table), "Catch scenarios not available for this stock")
  )
  TAC_timeline(test_table(), input$catch_choice, SAG_data_reactive())
})


############ Radial plot panel (Selection panel)
output$catch_scenarios_radial <- renderUI({
 
  if (!is_empty(catch_scenario_table_previous_year()$table)) {

    virtualSelectInput(
      inputId = "catch_choice_radial",
      label = "Select one or more catch scenarios:",
      choices = unique(catch_scenario_table_percentages()$cat),
      selected = c(Basis()$cat),
      multiple = TRUE,
      width = "100%",
      search = TRUE
    )

    # selectizeInput(
    #   inputId = "catch_choice_radial",
    #   label = "Select one or more catch scenarios:",
    #   choices = unique(catch_scenario_table_percentages()$cat),
    #   selected = c(Basis()$cat),
    #   multiple = TRUE
    # )
  } else {
    HTML("")
  }
})

############ Radial plot panel (radial plot)
output$Radial_plot <- renderPlotly({
  
  validate(
    need(!is_empty(advice_view_info()), "No ASD entry in assessment year"),
    need(!is_empty(advice_view_info_previous_year()), "No ASD entry in previous assessment year")
  )
  radial_plot(catch_scenario_table_percentages(), input$catch_choice_radial)
})

output$Radial_plot_disclaimer <- renderUI(
  HTML("<br><br> Disclaimer: the relative change for F, F wanted and HR has been calculated using the basis of advice of the previous year assessment. <br/>
  The scale of the plot is relative across the scenarios presented, please refer to the table or the % of change plot for actual percentage of change.")
)
############ Lollipop plot panel (Selection panel) 
output$catch_indicators_lollipop <- renderUI({

  if (!is_empty(catch_scenario_table_previous_year()$table)) { 

    virtualSelectInput(
      inputId = "indicator_choice_lollipop",
      label = "Select one ore more indicators:",
      choices = catch_scenario_table_percentages() %>% 
        select(where(~ !any(is.na(.)))) %>%
        names() %>%
        str_subset(pattern = c("Year|cat|cS_Purpose"), negate = TRUE),
        
      selected = c("SSB change"),
      multiple = TRUE,
      width = "100%",
      search = TRUE
    )   
    # selectizeInput(
    #   inputId = "indicator_choice_lollipop",
    #   label = "Select one ore more indicators",
    #   choices = catch_scenario_table_percentages() %>% 
    #     select(where(~ !any(is.na(.)))) %>%
    #     names() %>%
    #     str_subset(pattern = c("Year|cat|cS_Purpose"), negate = TRUE),
    #   selected = c("SSB change"),
    #   multiple = TRUE
    # )
  } else {
    HTML("")
  }
})

############ Lollipop plot panel (Lollipop plot) 
output$Lollipop_plot <- renderPlotly({
  validate(
    need(!is_empty(advice_view_info()), "No ASD entry in assessment year"),
    need(!is_empty(advice_view_info_previous_year()), "No ASD entry in previous assessment year")
  )
  
  lollipop_plot(catch_scenario_table_percentages(),input$indicator_choice_lollipop)
})

output$lollipop_plot_disclaimer <- renderUI(
  HTML("<br> <br> Disclaimer: the relative change for F, F wanted and HR has been calculated using the basis of advice of the previous year assessment.")
)


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


catch_scenario_table_collated <- eventReactive(catch_scenario_table(),{
  validate(
      need(!is_empty(catch_scenario_table()$table), "Catch scenarios not available for this stock")
    )
    
    catch_scenario_table()$table %>%
    arrange(cS_Purpose) %>%
    rename_all(~ catch_scenario_table()$cols) %>% 
    rename("Basis" = cS_Label, " " = cS_Purpose) %>% 
    select_if(~!(all(is.na(.)) | all(. == "")))
})


output$table <- DT::renderDT(
  catch_scenario_table_collated(),
  selection = "single",
  class = "display",
  caption = HTML("<b><font size= 6> Catch scenario table</b></font>"),
  rownames = FALSE,
  options = list(
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
  )
)

output$download_catch_table <- downloadHandler(
    filename = paste0("adviceXplorer_data-", Sys.Date(), ".zip"),
    content = function(fname) {
      
      fs <- c("Disclaimer.txt", "adviceXplorer_catchScenario_data.csv","adviceXplorer_catchScenarioNotes_data.csv")
      write.csv(icesASD::get_catch_scenario_table(advice_view_info()$adviceKey, query$year), file = "adviceXplorer_catchScenario_data.csv")
      write.csv(icesASD::getCatchScenariosNotes(advice_view_info()$adviceKey), file = "adviceXplorer_catchScenarioNotes_data.csv")
      write.table(read.delim("https://raw.githubusercontent.com/ices-tools-prod/disclaimers/master/Disclaimer_adviceXplorer.txt"),  file = "Disclaimer.txt", row.names = FALSE)
      
      zip::zip(zipfile=fname, files=fs)
    },
    contentType = "application/zip"
  )


##### footnotes of catch scenario table
footnotes <- eventReactive(req(advice_view_info()), {
  format_catch_scenario_notes(advice_view_info()$adviceKey)
})
output$footnotes <-renderUI({
  validate(
      need(!is_empty(footnotes()), " ")
    )
  
  footnotes()
  })

##### Last page text, citation, data usage, feedback etcc
output$contact_feedback <- renderUI({
  make_contact_and_feedback()
  
})
##### Last page text, citation, data usage, feedback etcc
output$data_sources <- renderUI({
  make_data_sources()
  
})

##### Last page text, citation, data usage, feedback etcc
output$data_disclaimer_policy <- renderUI({
  make_data_disclaimer_and_policy()
  
})

##### Last page text, citation, data usage, feedback etcc
output$citation <- renderUI({
  make_citation()
  
})

}
