library(shiny)
library(datamods)
library(shinyWidgets)


ui <- fluidPage(
  # theme = bslib::bs_theme(version = 5L),
  fluidRow(
    column(
      width = 10, offset = 1,
      tags$h3("Filter data with select group module"),
      shinyWidgets::panel(
        select_group_ui(
          id = "my-filters",
          params = list(
            list(inputId = "Manufacturer", label = "Manufacturer:"),
            list(inputId = "Type", label = "Type:"),
            list(inputId = "AirBags", label = "AirBags:"),
            list(inputId = "DriveTrain", label = "DriveTrain:")
          )
        ),
        status = "primary"
      ),
      reactable::reactableOutput(outputId = "table"),
      tags$b("Inputs values:"),
      verbatimTextOutput("inputs")
    )
  )
)

server <- function(input, output, session) {
  res_mod <- select_group_server(
    id = "my-filters",
    data = reactive(MASS::Cars93),
    vars = reactive(c("Manufacturer", "Type", "AirBags", "DriveTrain"))
  )

  output$table <- reactable::renderReactable({
    reactable::reactable(res_mod())
  })

  output$inputs <- renderPrint({
    attr(res_mod(), "inputs")
  })
}

# if (interactive())
  shinyApp(ui, server)

select_group_ui(
      id = "my-filters",
      params = list(
        StockKeyLabel = list(inputId = "StockKeyLabel", label = "Stock code:"),
        SpeciesCommonName = list(inputId = "SpeciesCommonName", label = "Common name:")
      )

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

  res_mod <- callModule(
    
    module = selectizeGroupServer,
    id = "my-filters",
    data = eco_filter,
    vars = c(
      "StockKeyLabel", "SpeciesCommonName"
    ),
    inline = FALSE
  )
  
  ###########################################################  Render table in stock selection tab

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




library(shiny)
library(shinyWidgets)

ui <- fluidPage(
  tags$h2("Virtual Select"),

  fluidRow(
    column(
      width = 4,
      virtualSelectInput(
        inputId = "single",
        label = "Single select :",
        choices = month.name,
        search = TRUE
      ),
      virtualSelectInput(
        inputId = "multiple",
        label = "Multiple select:",
        choices = setNames(month.abb, month.name),
        multiple = TRUE
      )
    ),
    column(
      width = 4,
      tags$b("Single select :"),
      verbatimTextOutput("res_single"),
      tags$b("Is virtual select open ?"),
      verbatimTextOutput(outputId = "res_single_open"),

      tags$br(),

      tags$b("Multiple select :"),
      verbatimTextOutput("res_multiple"),
      tags$b("Is virtual select open ?"),
      verbatimTextOutput(outputId = "res_multiple_open")
    )
  )


)

server <- function(input, output, session) {

  output$res_single <- renderPrint(input$single)
  output$res_single_open <- renderPrint(input$single_open)

  output$res_multiple <- renderPrint(input$multiple)
  output$res_multiple_open <- renderPrint(input$multiple_open)

}

if (interactive())
  shinyApp(ui, server)


  virtualSelectInput(
      inputId = "selected_years",
      label = "Assessment Year",
      choices = Years$Year,
      selected = 2023,
      multiple = FALSE,
      width = "100%",
      search = TRUE
    )