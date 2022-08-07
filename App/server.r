
msg <- function(...) {
  emph <- "\n****************\n"
  cat(emph, ..., emph)
}

############# Libraries ############




# required if using most recent version of sf
sf::sf_use_s2(FALSE)

options(icesSAG.use_token = FALSE)

## If this code is run for the first time and the SAG data in not present on the local machine
## the following line will download the last 5 years of SAG data (summary and ref points).
## This process will take several minutes but, once the data is in the local folder,
## the app will run much faster.
# if (!file.exists(
  
#     "Data/SAG_2021/SAG_summary.csv"#,
#     # "Data/SAG_2020/SAG_summary.csv",
#     # "Data/SAG_2019/SAG_summary.csv",
#     # "Data/SAG_2018/SAG_summary.csv",
#     # "Data/SAG_2017/SAG_summary.csv"
  
# )
# ) {
#   source("update_SAG_data.r")
# }

# if (!file.exists(
  
#     "Data/SID_2021/SID.csv"#,
#     # "Data/SID_2020/SID.csv",
#     # "Data/SID_2019/SID.csv",
#     # "Data/SID_2018/SID.csv",
#     # "Data/SID_2017/SID.csv"

# )
# ) {
#   source("update_SID_data.r")
# }


############# Start server function ################

server <- function(input, output, session) {
  msg("server loop start:\n  ", getwd())
  
observe({
        click("help_tab1")
        
      })

  helptext <- reactive(
    help_datatable()
  )
  
  observeEvent(
    eventExpr = input$help_tab1,
    handlerExpr = {
      introjs(session, 
              options = list(
                "showBullets"="false", 
                "showProgress"="true", 
                "showStepNumbers"="false",
                "nextLabel"="Next",
                "prevLabel"="Prev",
                "skipLabel"="Skip",
                "setDontShowAgain" = "true",
                steps=helptext()[tab == "help_tab1"]
              )
      )
    }
  )
  observeEvent(
    eventExpr = input$help_tab2,
    handlerExpr = {
      introjs(session, 
              options = list(
                "showBullets"="false", 
                "showProgress"="true", 
                "showStepNumbers"="false",
                "nextLabel"="Next",
                "prevLabel"="Prev",
                "skipLabel"="Skip",
                steps=helptext()[tab == "help_tab2"]
              )
      )
    }
  )
  observeEvent(
    eventExpr = input$help_tab3,
    handlerExpr = {
      introjs(session, 
              options = list(
                "showBullets"="false", 
                "showProgress"="true", 
                "showStepNumbers"="false",
                "nextLabel"="Next",
                "prevLabel"="Prev",
                "skipLabel"="Skip",
                steps=helptext()[tab == "help_tab3"]
              )
      )
    }
  )
  observeEvent(
    eventExpr = input$help_tab4,
    handlerExpr = {
      introjs(session, 
              options = list(
                "showBullets"="false", 
                "showProgress"="true", 
                "showStepNumbers"="false",
                "nextLabel"="Next",
                "prevLabel"="Prev",
                "skipLabel"="Skip",
                steps=helptext()[tab == "help_tab4"]
              )
      )
    }
  )
  observeEvent(
    eventExpr = input$help_tab5,
    handlerExpr = {
      introjs(session, 
              options = list(
                "showBullets"="false", 
                "showProgress"="true", 
                "showStepNumbers"="false",
                "nextLabel"="Next",
                "prevLabel"="Prev",
                "skipLabel"="Skip",
                steps=helptext()[tab == "help_tab5"]
              )
      )
    }
  )
  # values of the query string and first visit flag
  query <- reactiveValues(query_from_table = FALSE)

  
  ######################### Map panel

  # sf_cent <- st_coordinates(suppressWarnings(st_centroid(shape_eco)))
  # sf_cent_map_X <- mean(sf_cent[, 1])
  # sf_cent_map_Y <- mean(sf_cent[, 2])
  # sf_cent_map <- c(sf_cent_map_X, sf_cent_map_Y)
  # # Define the interactive labels
  # labels <- sprintf(
  #     "<strong>%s Ecoregion</strong><br/>%g Shape Area ",
  #     shape_eco$Ecoregion, shape_eco$Shape_Area
  # ) %>% lapply(htmltools::HTML)

  # Render Map 1
  output$map1 <- renderLeaflet({
    map_ecoregion(shape_eco, eu_shape)
  }) # END RENDER LEAFLET map1

  # # Render Map 2
  # output$map2 <- renderLeaflet({
  #   map_ices_areas(ices_areas, eu_shape)
  # }) # END RENDER LEAFLET map2
  ############################################################### END of MAPS

  ############################## Interactive section Ecoregions ######################
  # define leaflet proxy for Ecoregion map
  proxy_1 <- leafletProxy("map1")

  # create empty vector to hold all click ids
  selected_1 <- reactiveValues(groups = vector())

  # find index
  observeEvent(input$map1_shape_click, {

    
    
    ## calculate index of ecoregion selected in shape_eco
    idx_1 <- match(input$map1_shape_click$id, shape_eco$Ecoregion)
    # print(idx_1)
    if (input$map1_shape_click$group == "Eco_regions") {
      selected_1$groups <- c(selected_1$groups, input$map1_shape_click$id)
      # print(selected_1$groups)
      # print("check########")
      proxy_1 %>%
        showGroup(group = input$map1_shape_click$id) 
        # setView( ## zoom in
        #   lng = sf_cent[idx_1, 1],
        #   lat = sf_cent[idx_1, 2],
        #   zoom = 3
        # )
      
      ## this js code allows for the stock slection tab to be enabled once one coregion is clicked
      runjs("$(tab).removeClass('disabled');")#%>%
        

      # print(match(input$map_shape_click$id, shape_eco$Ecoregion))
    } else {
      selected_1$groups <- setdiff(selected_1$groups, input$map1_shape_click$group)
      proxy_1 %>%
        hideGroup(group = input$map1_shape_click$group) #%>%
        # setView( ## zoom out
        #   lng = sf_cent_map[1],
        #   lat = sf_cent_map[2],
        #   zoom = 1
        # )
        
    }
    updateSelectizeInput(session,
      inputId = "selected_locations",
      label = "ICES Ecoregions",
      choices = shape_eco$Ecoregion,
      selected = selected_1$groups
    )
    
  })

  observeEvent(input$selected_locations,
    {
      removed_via_selectInput <- setdiff(selected_1$groups, input$selected_locations)
      added_via_selectInput <- setdiff(input$selected_locations, selected_1$groups)

      if (length(removed_via_selectInput) > 0) {
        selected_1$groups <- input$selected_locations
        print(selected_1$groups)
        proxy_1 %>% hideGroup(group = removed_via_selectInput)
      }

      if (length(added_via_selectInput) > 0) {
        selected_1$groups <- input$selected_locations
        print(selected_1$groups)
        proxy_1 %>% showGroup(group = added_via_selectInput)
        
        ## this js code allows for the stock slection tab to be enabled once one coregion is clicked
        runjs("$(tab).removeClass('disabled');")
      }
    },
    ignoreNULL = FALSE
  )

  # ############################## Interactive section Areas ######################
  # # define leaflet proxy for Ecoregion map
  # proxy_2 <- leafletProxy("map2")

  # # create empty vector to hold all click ids
  # selected_2 <- reactiveValues(groups = vector())

  # # find index
  # observeEvent(input$map2_shape_click, {
  #   ## calculate index of ecoregion selected in shape_eco
  #   idx_2 <- match(input$map2_shape_click$id, ices_areas$Area_Full)
  #   # print(idx_2)
  #   if (input$map2_shape_click$group == "ices_areas") {
  #     selected_2$groups <- c(selected_2$groups, input$map2_shape_click$id)
  #     proxy_2 %>%
  #       showGroup(group = input$map2_shape_click$id) # %>%
  #     # setView(
  #     #     lng = sf_cent[idx_1, 1],
  #     #     lat = sf_cent[idx_1, 2],
  #     #     zoom = 3
  #     # )

  #     # print(match(input$map_shape_click$id, shape_eco$Ecoregion))
  #   } else {
  #     selected_2$groups <- setdiff(selected_2$groups, input$map2_shape_click$group)
  #     proxy_2 %>%
  #       hideGroup(group = input$map2_shape_click$group) %>%
  #       setView(
  #         lng = sf_cent_map[1],
  #         lat = sf_cent_map[2],
  #         zoom = 1
  #       )
  #   }
  #   updateSelectizeInput(session,
  #     inputId = "selected_areas",
  #     label = "ICES Areas",
  #     choices = ices_areas$Area_Full,
  #     selected = selected_2$groups
  #   )
  # })

  # observeEvent(input$selected_areas,
  #   {
  #     removed_via_selectInput <- setdiff(selected_2$groups, input$selected_areas)
  #     added_via_selectInput <- setdiff(input$selected_areas, selected_2$groups)

  #     if (length(removed_via_selectInput) > 0) {
  #       selected_2$groups <- input$selected_areas
  #       proxy_2 %>% hideGroup(group = removed_via_selectInput)
  #     }

  #     if (length(added_via_selectInput) > 0) {
  #       selected_2$groups <- input$selected_areas
  #       proxy_2 %>% showGroup(group = added_via_selectInput)
  #     }
  #   },
  #   ignoreNULL = FALSE
  # )
  ########################################################### end Maps reactive part

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
    # print(input$selected_locations)

    # ### download SID
    # stock_list_all <- download_SID(input$selected_years)
    # ### modifify SID table, 1 row == 1 Ecoregion
    # stock_list_long <- separate_ecoregions(stock_list_all)
    # ### add hyperlinks to table
    # stock_list_long <- sid_table_links(stock_list_long)
    stock_list_long <- fread(sprintf("Data/SID_%s/SID.csv", input$selected_years))
    stock_list_long <- stock_list_long %>% drop_na(AssessmentKey)

    ### reshuffle some columns
    # stock_list_long <- stock_list_long %>% relocate(icon, .before = SpeciesCommonName)
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
    # print(tibble(temp_df))
    stock_list_long <- temp_df
    stock_list_long <- stock_list_long %>% arrange(StockKeyLabel)
    # for (value in 1:nrow(stock_list_long)){
    #   if (value == 1){
    #     stock_list_long$Select[value] <- sprintf('<input type="radio" name="rdbtn" value="%s" checked="checked"/>', value)
    #   }
    #   else {
    #     stock_list_long$Select[value] <- sprintf('<input type="radio" name="rdbtn" value="%s"/>', value)
    #   }
    # }
    # stock_list_long$Select <- sprintf('<input type="radio" name="rdbtn" value="%s" checked/>', 1)
    stock_list_long$Select <- sprintf('<input type="radio" name="rdbtn" value="rdbtn_%s"/>', 1:nrow(stock_list_long))
    stock_list_long <- stock_list_long %>%
      relocate(Select, .before = StockKeyLabel)
    # print(tibble(stock_list_long))
  })

  # res_mod <- reactive({
  res_mod <- callModule(
    module = selectizeGroupServer,
    id = "my-filters",
    # data = separate_ecoregions(stock_list_all, selected_1$groups),
    data = eco_filter,
    vars = c(
      "StockKeyLabel",  "SpeciesCommonName",
      "ExpertGroup",  "DataCategory", "YearOfLastAssessment",
       "AdviceCategory"#, "Published"
    ) # , "ICES_area","StockDatabaseID", "StockKey","SpeciesScientificName",
    #"AdviceDraftingGroup","AssessmentFrequency","YearOfNextAssessment", "AdviceReleaseDate",
    #"AdviceType", "TrophicGuild","FisheriesGuild", "SizeGuild",)
  )
  

  #### last update of the app 
  output$app_last_update <- renderUI({
    make_app_update_date()
  })
  ###########################################################  Render table in stock selection tab

  output$tbl <- DT::renderDT(
    
#     colnames(eco_filter) <- c("Stock code", "Ecoregion", "icon", "Common Name","ExpertGroup", "Expert Group", "Data Category", "Year Of Last Assessment",
#     "Advice Category", "Advice doi", "Fisheries Overview doi", "AssessmentKey", "Data")
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
    # extensions = "Buttons",
    selection = 'none', 
    server = FALSE,    
    caption = "Select the fish stock of interest and then click on one of panels on the right",
    options = list(
      order = list(2, "asc"),
      dom = "Bfrtip",
      pageLength = 300,
      # buttons = c("csv","xls"),
      # buttons =
      #   list(
      #     "copy", "print",
      #     list(
      #       extend = "collection",
      #       buttons = c("csv", "excel"),
      #       text = "Download"
      #     )
      #   ),
      # rownames = FALSE,
      columnDefs = list(
        list(visible = FALSE, targets = c(0, 6, 13)),
        list(className = "dt-center", targets = c(1, 4, 7, 11, 12, 14, 15))
      )
    ),
    callback = JS(callback1(res_mod()))
)
  
  

  ## process selection
  observeEvent(input$rdbtn, {
    
    filtered_row <- res_mod()[str_detect(res_mod()$Select, regex(paste0("\\b", input$rdbtn,"\\b"))), ]
    # updateQueryString(paste0("?StockKeyLabel=", filtered_row$StockKeyLabel), mode = "push")

    ###
    #updateQueryString(paste0("?StockKeyLabel=", filtered_row$StockKeyLabel, "&", "Year=", input$selected_years), mode = "push") ####
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
    # print(names(query_string))
    #query$stockkeylabel <- query_string$stockkeylabel
    #query$year <- query_string$year ####

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
    options(icesSAG.use_token = TRUE)
    icesSAG::getSAGSettingsForAStock(query$assessmentkey)
  })

###### info about the stock selected for top of page
output$stock_infos <- renderUI({
  get_Stock_info(SAG_data_reactive()$StockKeyLabel[1], SAG_data_reactive()$StockDescription[1], SAG_data_reactive()$AssessmentYear[1])
})

##### button to download SAG data
output$download_SAG_Data <- downloadHandler(
    filename = function() {
      paste("SAG_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(SAG_data_reactive(), file)
    }
  )

######################### Stock development over time plots
  output$plot1 <- renderPlotly(
    ICES_plot_1(SAG_data_reactive(), sagSettings())
  ) # %>%
  # bindCache(SAG_data_reactive(), SAG_stamp(), cache = "session")

  output$plot2 <- renderPlotly(
    ICES_plot_2(SAG_data_reactive(), sagSettings())
  )
  output$plot3 <- renderPlotly(
    ICES_plot_3(SAG_data_reactive(), sagSettings())
  )
  output$plot4 <- renderPlotly(
    ICES_plot_4(SAG_data_reactive(), sagSettings())
  )


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
    get_Stock_info(SAG_data_reactive()$StockKeyLabel[1], SAG_data_reactive()$StockDescription[1], SAG_data_reactive()$AssessmentYear[1])
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

  ######################### quality of assessment plots
  output$plot5 <- renderPlotly(
      ICES_plot_5(advice_action_quality())
  )
  output$plot6 <- renderPlotly(
      ICES_plot_6(advice_action_quality())
  )
  output$plot7 <- renderPlotly(
      ICES_plot_7(advice_action_quality())
  )
  

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


##### Advice and stock infos
advice_view_summary <- eventReactive(req(advice_view_info()), {
  get_Advice_View_Summary(advice_view_info(), SAG_data_reactive()$StockDescription[1])
})
output$Advice_Summary <- renderUI({
  advice_view_summary()  
}) #%>%
  # bindCache(advice_view_sentence(), advice_view_info())


##### advice headline (right side of page)
advice_view_headline <- eventReactive(req(advice_view_info()), {
  get_Advice_View_Headline(advice_view_info())
})
output$Advice_Headline <- renderUI({
  advice_view_headline()  
})


### F_SSB and chatches plot linked to table
output$catch_scenario_plot_3 <- renderPlotly({  
  catch_scenarios_plot2(catch_scenario_table(), SAG_data_reactive())
}) #%>%
  # bindCache(catch_scenario_table(), SAG_data_reactive())



########## Historical catches panel
test_table <- eventReactive(catch_scenario_table(),{
  req(query$stockkeylabel, query$year)
  wrangle_catches_with_scenarios(access_sag_data_local(query$stockkeylabel,query$year),catch_scenario_table(), query$stockkeylabel,query$year)
})
output$catch_scenarios <- renderUI({
  # req(query$stockkeylabel, query$year, catch_scenario_table())
  # df_hist_catch <- wrangle_catches_with_scenarios(access_sag_data_local(query$stockkeylabel,query$year),catch_scenario_table())
  Basis <- catch_scenario_table_percentages()[catch_scenario_table_percentages()$cS_Purpose == "Basis Of Advice",]
  selectizeInput(
        inputId = "catch_choice",
        label = "Select a scenario",
        choices = unique(test_table()$cat),
        selected = c("Historical Catches",Basis$cat),
        multiple = TRUE
      )
})
output$TAC_timeline <- renderPlotly({
    TAC_timeline(test_table(), input$catch_choice, SAG_data_reactive())
})


############ Radial plot panel
output$catch_scenarios_radial <- renderUI({
  Basis <- catch_scenario_table_percentages()[catch_scenario_table_percentages()$cS_Purpose == "Basis Of Advice",]
    selectizeInput(
        inputId = "catch_choice_radial",
        label = "Select a scenario",
        choices = unique(catch_scenario_table_percentages()$cat),
        selected = c(Basis$cat),
        multiple = TRUE
      )
})
output$Radial_plot <- renderPlotly({
  radial_plot(catch_scenario_table_percentages(), input$catch_choice_radial)
})

###### Calendar of stock with modal
observeEvent(input$preview, {
    # Show a modal when the button is pressed
    shinyalert(title= " Advice Calendar", 
    # includeHTML("D:/Profile/Documents/GitHub/online-advice/Shiny/Scripts_in_development/timeline3.html"),
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

output$table <- DT::renderDT(
  tab <- catch_scenario_table() %>%
    arrange(F) %>%
    rename_all(funs(catch_table_names())) %>%
    rename("Basis" = cS_Label, " " = cS_Purpose),
 
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
output$footnotes <-renderUI(footnotes())


##### Last page text, citation, data usage, feedback etcc
output$citation <- renderUI({
  make_app_citation()
  
})





}
