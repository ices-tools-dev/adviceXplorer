
msg <- function(...) {
  emph <- "\n****************\n"
  cat(emph, ..., emph)
}


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
        showGroup(group = input$map1_shape_click$id) %>%
        setView(
          lng = sf_cent[idx_1, 1],
          lat = sf_cent[idx_1, 2],
          zoom = 3
        )

      # print(match(input$map_shape_click$id, shape_eco$Ecoregion))
    } else {
      selected_1$groups <- setdiff(selected_1$groups, input$map1_shape_click$group)
      proxy_1 %>%
        hideGroup(group = input$map1_shape_click$group) %>%
        setView(
          lng = sf_cent_map[1],
          lat = sf_cent_map[2],
          zoom = 1
        )
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

  eco_filter <- reactive({
    req(input$selected_locations)
    print(input$selected_locations)

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
      "StockDatabaseID", "StockKey", "StockKeyLabel", "SpeciesScientificName", "SpeciesCommonName",
      "ExpertGroup", "AdviceDraftingGroup", "DataCategory", "YearOfLastAssessment", "AssessmentFrequency",
      "YearOfNextAssessment", "AdviceReleaseDate", "AdviceCategory", "AdviceType", "TrophicGuild",
      "FisheriesGuild", "SizeGuild", "Published"
    ) # , "ICES_area")
  )

  ###########################################################  Render table in stock selection tab

  output$tbl <- DT::renderDT(res_mod(),
    extensions = "Buttons",
    selection = "single",
    caption = "Select the row for the fish stock of interest and then click on the 'Stock development over time' panel",
    options = list(
      dom = "Bfrtip",
      pageLength = 300,
      buttons = c("csv"),
      columnDefs = list(
        list(
          targets = 5,
          render = JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data.length > 15 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 15) + '...</span>' : data;",
            "}"
          )
        )
      )
    )
  )

  ## process selection
  observeEvent(input$tbl_rows_selected, {
    filtered_row <- res_mod()[input$tbl_rows_selected, ]
    updateQueryString(paste0("?StockKeyLabel=", filtered_row$StockKeyLabel), mode = "push")

    query$query_from_table <- TRUE

    msg("stock selected from table:", filtered_row$StockKeyLabel)
  })



  observe({
    # read url string
    query_string <- getQueryString()
    names(query_string) <- tolower(names(query_string))
    query$stockkeylabel <- query_string$stockkeylabel

    msg("stock selected from url:", query$stockkeylabel)

    if (!is.null(query$stockkeylabel) && !query$query_from_table) {
      updateNavbarPage(session, "tabset", selected = "Stock development over time")
    }
  })

  advice_action <- eventReactive(query$stockkeylabel, {

    stock_name <- query$stockkeylabel
    msg("downloading:", stock_name)

    #   # Dowload the data
    data_sag <- access_sag_data(stock_name, 2020)

    catches <- data_sag %>% select(Year, catches, landings, discards, units,  AssessmentYear) %>% add_column(stock_name_column = stock_name, .after = "units")
    R <- data_sag %>% select(Year, low_recruitment, recruitment, high_recruitment, recruitment_age) # %>% na.omit()
    f <- data_sag %>% select(Year, low_F, F, high_F, FLim, Fpa, FMSY, Fage, fishingPressureDescription)
    SSB <- data_sag %>% select(Year, low_SSB, SSB, high_SSB, Blim, Bpa, MSYBtrigger, stockSizeDescription, stockSizeUnits)
    list_df <- quality_assessment_data(stock_name)
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

  ######################### Advice panel

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
test <- eventReactive(query$stockkeylabel, {
    get_Advice_View_info(query$stockkeylabel)
})

output$Advice_View <- DT::renderDT(
    test(),
    selection = "none",
    caption = "Advice view info",
    rownames= FALSE,
    options = list(
        dom = 't',
      pageLength = 50)
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


}