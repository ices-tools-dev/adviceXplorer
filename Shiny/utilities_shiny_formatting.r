side_width <- 4

# allocations advice information panel
allocations_infopanel <-
  sidebarPanel(
    width = side_width
  )

# advice plot side panel
allocations_plotspanel <-
  mainPanel(
    width = 12 - side_width,
    tabsetPanel(
        tabPanel(
            title = "Catches",
            plotlyOutput("catches"),
            h5(helpText("Figure 1: Catches"))
            # actionButton("r_catches", "Get Stock Data")
        ),
        tabPanel(
            title = "Recruitment",
            plotlyOutput("R"),
            h5(helpText("Figure 2: Stock recruitment"))
            # actionButton("r_recr", "Get Stock Data")
        ),
        tabPanel(
            title = "Fishing Pressure",
            plotlyOutput("f"),
            h5(helpText("Figure 3: Fish mortality"))
            # actionButton("r_f", "Get Stock Data")
        ),
        tabPanel(
            title = "SSB",
            plotlyOutput("SSB"),
            h5(helpText("Figure 4: SSB"))
            # actionButton("r_SSB", "Get Stock Data")
        ),
        tabPanel(
            title = "Quality of Assessment",
            plotlyOutput("Q_Ass"),
            h5(helpText("Figure 4: Quality of Assessment"))
            # actionButton("r_SSB", "Get Stock Data")
        )
    ),
    DTOutput("tbl_summary")
    )


maps_panels <-
  sidebarPanel(
    width = 8,
    tabsetPanel(
      tabPanel("ICES Ecoregions", leafletOutput("map1", height = 800)),
      tabPanel("ICES Areas", leafletOutput("map2", height = 800))
    )
  )

selectize_panel <- 
mainPanel(
  width = 4,
  panel(
        selectizeInput(
            inputId = "selected_locations",
            label = "ICES Ecoregions",
            choices = shape_eco$Ecoregion,
            selected = NULL,
            multiple = TRUE,
            options = list(
              placeholder = 'Select Ecoregion(s)')
        ),
        selectizeInput(
            inputId = "selected_areas",
            label = "ICES Areas",
            choices = ices_areas$Area_Full,
            selected = NULL,
            multiple = TRUE,
            options = list(
              placeholder = 'Select ICES Area(s)')
        ),
        
          selectizeGroupUI(
            id = "my-filters",
            params = list(
              # EcoRegion = list(inputId = "EcoRegion", title = "EcoRegion:"),
              StockDatabaseID = list(inputId = "StockDatabaseID", title = "StockDatabaseID:"),
              StockKey = list(inputId = "StockKey", title = "StockKey:"),
              StockKeyLabel = list(inputId = "StockKeyLabel", title = "StockKeyLabel:"),
              SpeciesScientificName = list(inputId = "SpeciesScientificName", title = "SpeciesScientificName:"),
              SpeciesCommonName = list(inputId = "SpeciesCommonName", title = "SpeciesCommonName:"),
              ExpertGroup = list(inputId = "ExpertGroup", title = "ExpertGroup:"),
              AdviceDraftingGroup = list(inputId = "AdviceDraftingGroup", title = "AdviceDraftingGroup:"),
              DataCategory = list(inputId = "DataCategory", title = "DataCategory:"),
              YearOfLastAssessment = list(inputId = "YearOfLastAssessment", title = "YearOfLastAssessment:"),
              AssessmentFrequency = list(inputId = "AssessmentFrequency", title = "AssessmentFrequency:"),
              YearOfNextAssessment = list(inputId = "YearOfNextAssessment", title = "YearOfNextAssessment:"),
              AdviceReleaseDate = list(inputId = "AdviceReleaseDate", title = "AdviceReleaseDate:"),
              AdviceCategory = list(inputId = "AdviceCategory", title = "AdviceCategory:"),
              AdviceType = list(inputId = "AdviceType", title = "AdviceType:"),
              TrophicGuild = list(inputId = "TrophicGuild", title = "TrophicGuild:"),
              FisheriesGuild = list(inputId = "FisheriesGuild", title = "FisheriesGuild:"),
              SizeGuild = list(inputId = "SizeGuild", title = "SizeGuild:"),
              Published = list(inputId = "Published", title = "Published:")
              # ICES_area = list(inputId = "ICES_area", title = "ICES_area")
            ),
            inline = FALSE
          ), 
          heading = "Data filtering",
          status = "primary"
        )
)