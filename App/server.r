
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
    label = "Assessment Year",
    choices = Years$Year,
    selected = 2021
  )


  eco_filter <- reactive({
    req(input$selected_locations, input$selected_years)
    
    stock_list_long <- fread(sprintf("Data/SID_%s/SID.csv", input$selected_years))
    stock_list_long <- stock_list_long %>% drop_na(AssessmentKey) 
    stock_list_long <- purrr::map_dfr(.x = input$selected_locations,
                           .f = function(.x) stock_list_long %>% dplyr::filter(str_detect(EcoRegion, .x))) %>%
      dplyr::arrange(StockKeyLabel) %>%
      dplyr::mutate(EcoRegion = removeWords(EcoRegion, "Ecoregion"),
                    Select = sprintf('<input type="radio" name="rdbtn" value="rdbtn_%s"/>', 1:nrow(.)), 
                    stock_description = purrr::map_chr(StockKeyLabel, .f = ~ access_sag_data_local(.x, input$selected_years)$StockDescription[1]),
                    stock_location = parse_location_from_stock_description(stock_description))

  })

  
  res_mod <- callModule(
    module = selectizeGroupServer,
    id = "my-filters",    
    data = eco_filter,
    vars = c(
      "StockKeyLabel", "SpeciesCommonName"
    )
  )
  
  ###########################################################  Render table in stock selection tab

  output$tbl <- DT::renderDT(
    
    res_modo <- res_mod() %>% select("Select",
                                      "StockKeyLabel",
                                      "EcoRegion",
                                      "icon",
                                      "SpeciesCommonName",
                                      "stock_location") %>% 
                           rename("Select" = Select,
                                      "Stock code" = StockKeyLabel,
                                      "Ecoregion" = EcoRegion,
                                      " " = icon,
                                      "Common name" = SpeciesCommonName,
                                      "Location" = stock_location),
                                      
    
    escape = FALSE,
    selection = 'none', 
    server = FALSE,    
    caption = HTML("<b><font size= 5> To select a stock, click on the corresponding button in the 'Select' column. </font></b>"),
    options = list(
      order = list(2, "asc"),
      dom = "Bfrtip",
      pageLength = 300,
      columnDefs = list(
        list(visible = FALSE, targets = c(0,3)),
        list(className = "dt-center", targets = c(1, 4))
      )
    ),
    callback = JS(callback)
)
  
  

  ## process radio button selection
  observeEvent(input$rdbtn, {
    
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
  
  get_advice_doi(query$assessmentkey)

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
advice_view_headline <- eventReactive(req(advice_view_info()), {
  get_Advice_View_Headline(advice_view_info())
})

output$Advice_Headline1 <- output$Advice_Headline2 <- output$Advice_Headline3 <- renderUI({
  advice_view_headline()  
})


#### link to pdf of advice (NOT ACTIVE)
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

  )

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
    query$year <- info$AssessmentYear 

    stock_name <- query$stockkeylabel

    year <- query$year 
    
    quality_assessment_data_local(stock_name, year)
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

#### link to pdf of advice (NOT ACTIVE)
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
  
})

##### catch scenario table scaled with the values of previous advice to get percentage of change
catch_scenario_table_percentages <- eventReactive(req(catch_scenario_table_previous_year(),catch_scenario_table()), {
  scale_catch_scenarios_for_radialPlot(catch_scenario_table_previous_year()$table, catch_scenario_table()$table)
})


#### link for the advice view link button to the full stock record (NOT ACTIVE)
onclick("advice_view_link", runjs(paste0("window.open('https://sg.ices.dk/adviceview/viewAdvice/", advice_view_info()$adviceKey,"', '_blank')")))

#### link to pdf of advice (NOT ACTIVE)
onclick("library_advice_link3", runjs(paste0("window.open('", advice_doi(),"', '_blank')")))


### F_SSB and chatches plot linked to table
output$catch_scenario_plot_3 <- renderPlotly({
  
  validate(
      need(!is_empty(catch_scenario_table()$table), "Data not available for this stock")
    )
  tmp <- arrange(catch_scenario_table()$table, F)
  catch_scenarios_plot2(tmp, SAG_data_reactive())
}) 

########## Historical catches panel (preparation of data)
test_table <- eventReactive(catch_scenario_table(), {
  req(query$stockkeylabel, query$year)
  validate(
    need(!is_empty(catch_scenario_table()$table), "Data not available for this stock")
  )
  wrangle_catches_with_scenarios(access_sag_data_local(query$stockkeylabel, query$year), catch_scenario_table()$table, query$stockkeylabel, query$year)
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


############### Catch scenario plot
catch_table_names <- eventReactive(catch_scenario_table_previous_year(),{
  req(query$stockkeylabel, query$year)
  catch_scenario_table_previous_year()$cols

})

catch_scenario_table_collated <- eventReactive(catch_scenario_table(),{
  validate(
      need(!is_empty(catch_scenario_table()$table), "Data not available for this stock")
    )
    catch_scenario_table()$table %>%
    arrange(cS_Purpose) %>%
    rename_all(funs(catch_table_names())) %>%
    rename("Basis" = cS_Label, " " = cS_Purpose)
})


output$table <- DT::renderDT(
  catch_scenario_table_collated(),
 
  selection = "single",
  class = "display",
  caption = "Subset of catch scenario table",
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
