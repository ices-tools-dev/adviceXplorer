
msg <- function(...) {
  emph <- "\n****************\n"
  cat(emph, ..., emph)
}

############# Libraries ############




# required if using most recent version of sf
sf::sf_use_s2(FALSE)



## If this code is run for the first time and the SAG data in not present on the local machine
## the following line will download the last 5 years of SAG data (summary and ref points).
## This process will take several minutes but, once the data is in the local folder,
## the app will run much faster.
if (!file.exists("Data/SAG_2021/SAG_summary.csv")) {
    source("update_SAG_data.r")
}


############# Start server function ################

server <- function(input, output, session) {
  msg("server loop start:\n  ", getwd())

  # values of the query string and first visit flag
  query <- reactiveValues(query_from_table = FALSE)

  ######################### Map panel

  sf_cent <- st_coordinates(suppressWarnings(st_centroid(shape_eco)))
  sf_cent_map_X <- mean(sf_cent[, 1])
  sf_cent_map_Y <- mean(sf_cent[, 2])
  sf_cent_map <- c(sf_cent_map_X, sf_cent_map_Y)
  # # Define the interactive labels
  # labels <- sprintf(
  #     "<strong>%s Ecoregion</strong><br/>%g Shape Area ",
  #     shape_eco$Ecoregion, shape_eco$Shape_Area
  # ) %>% lapply(htmltools::HTML)

  # Render Map 1
  output$map1 <- renderLeaflet({
    map_ecoregion(shape_eco, eu_shape)
  }) # END RENDER LEAFLET map1

  # Render Map 2
  output$map2 <- renderLeaflet({
    map_ices_areas(ices_areas, eu_shape)
  }) # END RENDER LEAFLET map2
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
      print(selected_1$groups)
      proxy_1 %>%
        showGroup(group = input$map1_shape_click$id) #%>%
        # setView( ## zoom in
        #   lng = sf_cent[idx_1, 1],
        #   lat = sf_cent[idx_1, 2],
        #   zoom = 3
        # )

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
      }
    },
    ignoreNULL = FALSE
  )

  ############################## Interactive section Areas ######################
  # define leaflet proxy for Ecoregion map
  proxy_2 <- leafletProxy("map2")

  # create empty vector to hold all click ids
  selected_2 <- reactiveValues(groups = vector())

  # find index
  observeEvent(input$map2_shape_click, {
    ## calculate index of ecoregion selected in shape_eco
    idx_2 <- match(input$map2_shape_click$id, ices_areas$Area_Full)
    # print(idx_2)
    if (input$map2_shape_click$group == "ices_areas") {
      selected_2$groups <- c(selected_2$groups, input$map2_shape_click$id)
      proxy_2 %>%
        showGroup(group = input$map2_shape_click$id) # %>%
      # setView(
      #     lng = sf_cent[idx_1, 1],
      #     lat = sf_cent[idx_1, 2],
      #     zoom = 3
      # )

      # print(match(input$map_shape_click$id, shape_eco$Ecoregion))
    } else {
      selected_2$groups <- setdiff(selected_2$groups, input$map2_shape_click$group)
      proxy_2 %>%
        hideGroup(group = input$map2_shape_click$group) %>%
        setView(
          lng = sf_cent_map[1],
          lat = sf_cent_map[2],
          zoom = 1
        )
    }
    updateSelectizeInput(session,
      inputId = "selected_areas",
      label = "ICES Areas",
      choices = ices_areas$Area_Full,
      selected = selected_2$groups
    )
  })

  observeEvent(input$selected_areas,
    {
      removed_via_selectInput <- setdiff(selected_2$groups, input$selected_areas)
      added_via_selectInput <- setdiff(input$selected_areas, selected_2$groups)

      if (length(removed_via_selectInput) > 0) {
        selected_2$groups <- input$selected_areas
        proxy_2 %>% hideGroup(group = removed_via_selectInput)
      }

      if (length(added_via_selectInput) > 0) {
        selected_2$groups <- input$selected_areas
        proxy_2 %>% showGroup(group = added_via_selectInput)
      }
    },
    ignoreNULL = FALSE
  )
  ########################################################### end Maps reactive part

  ###########################################################  function to use the input from the maps and the sid filtering


  # Update the year of selection

  updateSelectizeInput(session,
    inputId = "selected_years",
    label = "Year SID/SAG",
    choices = Years$Year,
    selected = 2021
  )


  eco_filter <- reactive({
    req(input$selected_locations, input$selected_years)
    # print(input$selected_locations)

    ### download SID
    stock_list_all <- download_SID(input$selected_years)
    ### modifify SID table, 1 row == 1 Ecoregion
    stock_list_long <- separate_ecoregions(stock_list_all)
    ### add hyperlinks to table
    stock_list_long <- sid_table_links(stock_list_long)
    ### reshuffle some columns
    stock_list_long <- stock_list_long %>% relocate(icon, .before = SpeciesCommonName)
    stock_list_long <- stock_list_long %>%
      relocate(advice_url, .before = EcoRegion) %>%
      relocate(group_url, .before = DataCategory) %>%
      # select(-c(ExpertGroup)) %>%
      # rename(StockCode = StockKeyLabel) %>%
      rename(ExpertGroupUrl = group_url) %>%
      rename(StockCode = advice_url)



    temp_df <- data.frame()
    for (i in 1:length(input$selected_locations)) {
      temp_1 <- stock_list_long %>% filter(str_detect(EcoRegion, input$selected_locations[i]))
      temp_df <- rbind(temp_df, temp_1)
    }
    print(tibble(temp_df))
    stock_list_long <- temp_df
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
       "AdviceCategory", "Published"
    ) # , "ICES_area","StockDatabaseID", "StockKey","SpeciesScientificName",
    #"AdviceDraftingGroup","AssessmentFrequency","YearOfNextAssessment", "AdviceReleaseDate",
    #"AdviceType", "TrophicGuild","FisheriesGuild", "SizeGuild",)
  )

  ###########################################################  Render table in stock selection tab

  output$tbl <- DT::renderDT(res_mod(),
    escape = FALSE,
    extensions = "Buttons",
    selection = "single",
    caption = "Select the row for the fish stock of interest and then click on the 'Stock development over time' panel",
    options = list(
      dom = "Bfrtip",
      pageLength = 1000,
      # buttons = c("csv","xls"),
      buttons =
        list("copy", "print", list(
          extend = "collection",
          buttons = c("csv", "excel"),
          text = "Download"
        )),
      # rownames = FALSE,
      columnDefs = list(
        # list(
        #   targets = 4,
        #   render = JS(
        #     "function(data, type, row, meta) {",
        #     "return type === 'display' && data.length > 15 ?",
        #     "'<span title=\"' + data + '\">' + data.substr(0, 15) + '...</span>' : data;",
        #     "}"
        #   )
        # ),
        # list(
        #   targets = 2,
        #   render = JS(
        #     "function(.zoom.hover) {",
        #     "transform: scale(1.5); ",
        #     "}"
        #   )

        # ),
        list(visible = FALSE, targets = c(0, 1, 6))
      )
    )
    # callback=JS(
    #   'table.on("mouseover","td", function() {
    #   $(".small-img").hover(function() {
    #       $(this).css("transform", "scale(3, 3)");
    #     }, function(){
    #           $(this).css("transform", "none");
    #     });
    #   })')
    # callback = JS("table.on('mouseover', 'tr', function() {
    #                           $(.small-img).hover(function() {
    #                             $(this).css(transform,scale(3,3));
    #                             }, function() {
    #                               $(this).css(transform, none);
    #                               });
    #                               })"
    #                           )
  )

  ## process selection
  observeEvent(input$tbl_rows_selected, {
    filtered_row <- res_mod()[input$tbl_rows_selected, ]
    # updateQueryString(paste0("?StockKeyLabel=", filtered_row$StockKeyLabel), mode = "push")

    print(filtered_row)

    ###
    #updateQueryString(paste0("?StockKeyLabel=", filtered_row$StockKeyLabel, "&", "Year=", input$selected_years), mode = "push") ####
    updateQueryString(paste0("?assessmentKey=", filtered_row$assessmentKey), mode = "push") ####

    ###

    query$query_from_table <- TRUE

    msg("stock selected from table:", filtered_row$StockKeyLabel)
    msg("year of SAG/SID selected from table:", input$selected_years) #####
  })



  observe({
    # read url string
    query_string <- getQueryString()
    names(query_string) <- tolower(names(query_string))
    print(names(query_string))
    #query$stockkeylabel <- query_string$stockkeylabel
    #query$year <- query_string$year ####

    query$assessmentkey <- query_string$assessmentkey

    if (!is.null(query$assessmentkey) && !query$query_from_table) {
      info <- getFishStockReferencePoints(query$assessmentkey)[[1]]

      query$stockkeylabel <- info$StockKeyLabel
      query$year <- info$AssessmentYear ####

      msg("stock selected from url:", query$stockkeylabel)
      msg("year of SAG/SID selected from url:", query$year) #####

      updateNavbarPage(session, "tabset", selected = "Stock development over time")
    }
  })

  advice_action <- eventReactive(req(query$stockkeylabel, query$year), {

    stock_name <- query$stockkeylabel
    msg("downloading:", stock_name)

    year <- query$year #####

    #   # Dowload the data
    data_sag <- access_sag_data_local(stock_name, year) #####

    catches <- data_sag %>% select(Year, catches, landings, discards, units,  AssessmentYear) %>% add_column(stock_name_column = stock_name, .after = "units")
    R <- data_sag %>% select(Year, low_recruitment, recruitment, high_recruitment, recruitment_age) # %>% na.omit()
    f <- data_sag %>% select(Year, low_F, F, high_F, FLim, Fpa, FMSY, Fage, fishingPressureDescription)
    SSB <- data_sag %>% select(Year, low_SSB, SSB, high_SSB, Blim, Bpa, MSYBtrigger, stockSizeDescription, stockSizeUnits)
    list_df <- quality_assessment_data_local(stock_name)
    # the bit below could be potentially be replaced by the sag status? summary table option?
    SAG_summary <- data_sag %>% select(
      Year,
      recruitment, high_recruitment, low_recruitment,
      SSB, high_SSB, low_SSB,
      catches, landings,
      F, high_F, low_F
    )

    #     big_data <- list_df[[1]]
    # big_data_last_year <- list_df[[2]]

    list(catches = catches, R = R, f = f, SSB = SSB, big_data = list_df[[1]], big_data_last_year = list_df[[2]], SAG_summary = SAG_summary)
  })

  ######################### Stock development over time

  output$all_plots <- renderPlotly({
    data_list <- advice_action()


    rv <- reactiveValues(
      catches_df = data_list$catches,
      r_df = data_list$R,
      f_df = data_list$f,
      SSB_df = data_list$SSB
    )
    figure_1_plots(
      rv$catches_df, rv$r_df, rv$f_df, rv$SSB_df,
            rv$catches_df$Year, rv$catches_df$catches, rv$catches_df$landings, rv$catches_df$discards, rv$catches_df$units, rv$catches_df$stock_name, rv$catches_df$AssessmentYear,
            rv$r_df$recruitment, rv$r_df$low_recruitment, rv$r_df$high_recruitment, rv$r_df$recruitment_age,
            rv$f_df$low_F, rv$f_df$F, rv$f_df$high_F, rv$f_df$FLim, rv$f_df$Fpa, rv$f_df$FMSY, rv$f_df$Fage, rv$f_df$fishingPressureDescription,
            rv$SSB_df$low_SSB, rv$SSB_df$SSB, rv$SSB_df$high_SSB, rv$SSB_df$Blim, rv$SSB_df$Bpa, rv$SSB_df$MSYBtrigger, rv$SSB_df$stockSizeDescription, rv$SSB_df$stockSizeUnits
    )
  })

  #### Plot 5 quality of assessment
  output$Q_Ass <- renderPlotly({
    data_list <- advice_action()

    rv <- reactiveValues(
      Q_Ass_df1 = data_list$big_data,
      Q_Ass_df2 = data_list$big_data_last_year
    )

    ### forth plot
    quality_assessment_plots(rv$Q_Ass_df1, rv$Q_Ass_df2,
                                    rv$Q_Ass_df1$stockSizeDescription, rv$Q_Ass_df1$stockSizeUnits,
                                    rv$Q_Ass_df1$Fage, rv$Q_Ass_df1$fishingPressureDescription,
                                    rv$Q_Ass_df1$RecruitmentAge)
    # figure_4_SSB(rv$SSB_df, rv$SSB_df$Year, rv$SSB_df$low_SSB, rv$SSB_df$SSB, rv$SSB_df$high_SSB, rv$SSB_df$Blim, rv$SSB_df$Bpa, rv$SSB_df$MSYBtrigger)
  })


##### catch scenarios tab
advice_view_info <- eventReactive(query$stockkeylabel, {
  get_Advice_View_info(query$stockkeylabel)
})

output$Advice_View <- DT::renderDT(
    advice_view_info(),
    selection = "none",
    caption = "Advice view info",
    rownames= FALSE,
    options = list(
        dom = 't',
      pageLength = 50)
)


##### catch scenarios sentence
advice_view_sentence <- eventReactive(query$stockkeylabel, {
  get_Advice_View_sentence(query$stockkeylabel)
})
output$Advice_Sentence <- renderUI({
  HTML(paste0(br(),"<b>","<font size=", 5, ">", advice_view_sentence(),"</font>","</b>", br()))
})


##### catch scenarios table
catch_scenario_table <- eventReactive(query$stockkeylabel, {
  standardize_catch_scenario_table(get_catch_scenario_table(query$stockkeylabel))
})

output$catch_scenario_table <- DT::renderDT(
    catch_scenario_table(),
    selection = "none",
    caption = "Catch Scenario Table",
    rownames= FALSE,
    options = list(
        dom = 't',
      pageLength = 100)
)
# output$Advice_View <- DT::datatable(
#     test(),
#     selection = "none",
#     caption = "Advice view info",

#     options = list(
#         dom = 't',
#       pageLength = 50,
#       initComplete = htmlwidgets::JS(
#           "function(settings, json) {",
#           paste0("$(this.api().table().container()).css({'font-size': '10px'});"),
#           "}"))
# )
output$catch_scenario_plot_1 <- renderPlotly(catch_scenarios_plot1(catch_scenario_table()))

output$catch_scenario_plot_2 <- renderPlotly({
  data_list <- advice_action()
  rv <- reactiveValues(
    catches_df = data_list$catches,
    f_df = data_list$f,
    SSB_df = data_list$SSB
  )
  catch_scenarios_plot2(catch_scenario_table(), rv$f_df$Fage, rv$f_df$fishingPressureDescription, rv$SSB_df$stockSizeDescription, rv$SSB_df$stockSizeUnits,rv$catches_df$units)
})


##### new tab in development left side
output$Advice_Sentence2 <- renderUI({
  HTML(paste0("<b>","<font size=", 5, ">", "Headline advice:","</font>","</b>", br(),"<font size=", 3, ">", advice_view_sentence(),"</font>"))
})

### F_SSB and chatches plot linked to table
output$catch_scenario_plot_3 <- renderPlotly({
  data_list <- advice_action()
  rv <- reactiveValues(
    catches_df = data_list$catches,
    f_df = data_list$f,
    SSB_df = data_list$SSB
  )
  catch_scenarios_plot2(catch_scenario_table(), rv$f_df$Fage, rv$f_df$fishingPressureDescription, rv$SSB_df$stockSizeDescription, rv$SSB_df$stockSizeUnits,rv$catches_df$units)
})

# catches_AND_scenarios_table <- observeEvent(query$stockkeylabel,query$year,catch_scenario_table(),{
# # print(query$stockkeylabel)
#   wrangle_catches_with_scenarios(access_sag_data_local(query$stockkeylabel,query$year),catch_scenario_table())
# })

test_table <- eventReactive(catch_scenario_table(),{
  req(query$stockkeylabel, query$year)
  wrangle_catches_with_scenarios(access_sag_data_local(query$stockkeylabel,query$year),catch_scenario_table())
})
output$catch_scenarios <- renderUI({
  # req(query$stockkeylabel, query$year, catch_scenario_table())
  # df_hist_catch <- wrangle_catches_with_scenarios(access_sag_data_local(query$stockkeylabel,query$year),catch_scenario_table())

  selectizeInput(
        inputId = "catch_choice",
        label = "Select a scenario",
        choices = unique(test_table()$cat),
        selected = "Historical Catches",
        multiple = TRUE
      )
})


output$TAC_timeline <- renderPlotly({
  data_list <- advice_action()
  rv <- reactiveValues(
    catches_df = data_list$catches,
  )
  TAC_timeline(test_table(), input$catch_choice, rv$catches_df$units)
})


### right side
output$advice_timeline <- renderTimevis({
  timevis(get_advice_timeline(query$stockkeylabel, res_mod(), input$tbl_rows_selected))
#   style <- "
#  .vis-timeline {
#    border-color: #269026;
#    background-color: rgb(246,250,251);
#    font-size: 20px;
#    font-family: Sans-serif;
#    color: #9AC2B7;
#  }

#  .vis-item {
#    border: 1px solid #E8EAEA;
#    font-size: 20pt;
#    background: #9AC2B7;
#    font-family: Sans-serif;
#    padding: 5px;
#  }
#  "
#   tagList(tags$style(style), tv)

  # htmltools::html_print(tv)
})

output$table <- DT::renderDT(
  arrange(catch_scenario_table(), F) %>% select(-Year),
  selection = "single",
  class = "display",
  caption = "Catch Scenario Table",
  rownames = FALSE,
  options = list(
    dom = "Bfrtip",
    pageLength = 100,
    buttons =
      list("copy", "print", list(
        extend = "collection",
        buttons = c("csv", "excel"),
        text = "Download"
      ))
    # columnDefs = list(list(visible=FALSE, targets=c(1)))
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

# tableProxy ##
table_proxy = dataTableProxy('table')

selected_scenario <- reactive({
      if (is.null(event_data("plotly_hover", source = "ranking")))
        return(NULL)
      event_data("plotly_hover", source = "ranking")
      })

    observe({
      selectRows(table_proxy, selected=(selected_scenario()[[2]]+1))
    })




 observeEvent(input$tbl_rows_selected, {
    filtered_row <- res_mod()[input$tbl_rows_selected, ]
    WG <- filtered_row$ExpertGroupUrl
    WG <- str_match(WG, "\\>\\s*(.*?)\\s*\\<\\/a>")[,2]
    print(WG)

})

output$citation <- renderUI({
  make_app_citation()
  # HTML(paste0(br(),"<b>","<font size=", 3, ">",br(), make_app_citation(), "</font>","</b>", br()))
})





}
