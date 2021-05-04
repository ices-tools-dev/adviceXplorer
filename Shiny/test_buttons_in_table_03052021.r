library(shiny)
library(tidyverse)
library(DT)

source("modal_dialog.R")

ui <- fluidPage(

  # div(style = "display: none;", icon("refresh")),
  div(
    class = "container",
    div(
      style = "margin-top: 50px;",
      shiny::actionButton(
        inputId = "add_car",
        label = "Add Row",
        icon = shiny::icon("plus"),
        class = "btn-success"
      )
    )
  ),
  div(
    class = "container",
    style = "margin-top: 50px;",
    DT::DTOutput(outputId = "dt_table", width = "100%")
  ),

  shiny::includeScript("Shiny/script.js")
)


create_btns <- function(x) {
  x %>%
    purrr::map_chr(~
    paste0(
      '<div class = "btn-group">
                   <button class="btn btn-default action-button btn-info action_button" id="edit_',
      .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-chart-line"></i></button>
                   <button class="btn btn-default action-button btn-danger action_button" id="delete_',
      .x, '" type="button" onclick=get_id(this.id)><i class="fa fa-trash-alt"></i></button></div>'
    ))
}

stock_list_all <- jsonlite::fromJSON(
            URLencode(
                "http://sd.ices.dk/services/odata4/StockListDWs4?$filter=ActiveYear eq 2021&$select=StockKey, StockKeyLabel, EcoRegion, SpeciesScientificName,  SpeciesCommonName, ExpertGroup"
            )
        )$value
x <- create_btns(1:nrow(stock_list_all))

stock_list_all <- stock_list_all %>% 
    dplyr::bind_cols(tibble("Buttons" = x))
# mtcars <- mtcars %>%
#   tibble::rownames_to_column(var = "Car") %>%
#   dplyr::bind_cols(tibble("Buttons" = x)) %>%
#   dplyr::mutate(vs = ifelse(vs == 0, "V-shaped", "Straight")) %>%
#   dplyr::mutate(am = ifelse(am == 0, "automatic", "manual")) 



server <- function(input, output, session) {
    output$dt_table <- DT::renderDT(stock_list_all)
}


shinyApp(ui = ui, server = server)






###########################Example 2

library(shiny)
library(DT)
library(tibble)



library(htmlwidgets)
library(dplyr)
library(ggplot2)
library(dygraphs)
library(htmltools)
library(widgetframe)
library(icesSAG)
library(plotly)

library(shiny)
library(shinythemes)
library(glue)


library(sf)
library(leaflet)
library(fisheryO)
library(DT)
library(tidyverse)

#' Programmatically create a Shiny input
#' 
#' @param FUN function to create the input
#' @param n number of inputs to be created
#' @param id ID prefix for each input
shinyInput <- function(FUN, n, id, ...) {

  # for each of n, create a new input using the FUN function and convert
  # to a character
  vapply(seq_len(n), function(i){
    as.character(FUN(paste0(id, i), ...))
  }, character(1))
  
}
stock_list_all <- jsonlite::fromJSON(
            URLencode(
                "http://sd.ices.dk/services/odata4/StockListDWs4?$filter=ActiveYear eq 2021&$select=StockKey, StockKeyLabel, EcoRegion, SpeciesScientificName,  SpeciesCommonName, ExpertGroup"
            )
        )$value
source("Shiny/utilities_plotting.r")



shinyApp(
  ui <- fluidPage(
    DTOutput("data"),
    textOutput('myText'),
    plotlyOutput("catches"),
    plotlyOutput("R"),
    plotlyOutput("f"),
    plotlyOutput("SSB")

    
  ),
  
  server <- function(input, output) {
    
    # reactive data frame which creates the number of actionButtons needed
    df <- reactiveVal(
      tibble(
          stock_list_all,
        # parameters here:
        #   * actionButton - type of input to create
        #   * 5 - how many we need
        #   * 'button_' - the ID prefix
        #   * label - label to show on the button
        #   * onclick - what to do when clicked
        Actions = shinyInput(
          FUN = actionButton,
          n = nrow(stock_list_all),
          id = 'button_',
          label = "Advice",
          onclick = 'Shiny.setInputValue(\"select_button\", this.id, {priority: \"event\"})'
        )
      )
    )
    
    # Table showing the data
    output$data <- DT::renderDT({
      
      df()
    }, 
    
      # Don't escape any HTML in the table (i.e. the actionButton)
      escape = FALSE,
      
      # turn off row selection otherwise you'll also select that row when you
      # click on the actionButton 
      selection = 'none'
    )
    
    # When a button is clicked, employee is set to the employee name
    #  associated with the clicked row
    advice_action <- eventReactive(input$select_button, {
      # take the value of input$select_button, e.g. "button_1"
      # get the button number (1) and assign to selectedRow
        selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
      
      # get the value of the "Name" column in the data.frame for that row
      
        stock_name <- as.character(df()[selectedRow, "StockKeyLabel"])

    
        source("Shiny/utilities_sag_data.r")
        data_sag <- access_sag_data(stock_name, 2020)

        #output$catches <- renderPlotly({
        # rv <- reactiveValues(
        catches <- data_sag %>% select(Year, catches, landings, discards)#,#,
        R <- data_sag %>% select(Year, low_recruitment, recruitment, high_recruitment) #%>% na.omit()
        f <- data_sag %>% select(Year, low_F, F, high_F, FLim, Fpa, FMSY)
        SSB <- data_sag %>% select(Year, low_SSB, SSB, high_SSB, Blim, Bpa, MSYBtrigger) 
        
        list(catches = catches, R = R, f = f, SSB = SSB)
        
    })
    
    
    #### Plot 1 Landings and discards
    output$catches <- renderPlotly({

         data_list = advice_action()
         #catches_df = data_list$catches
         #rv <- reactiveValues(
             #data_plot1 <- data_list$catches_df)
         #c = data_list$rv$catches
         #print(catches_df)
         rv <- reactiveValues(
             catches_df = data_list$catches
         )
         figure_1_catches(rv$catches_df, rv$catches_df$Year, rv$catches_df$landings, rv$catches_df$discards)
         
         })

    output$R <- renderPlotly({
        data_list = advice_action()
         #catches_df = data_list$catches
         #rv <- reactiveValues(
             #data_plot1 <- data_list$catches_df)
         #c = data_list$rv$catches
         #print(catches_df)
         rv <- reactiveValues(
             r_df = data_list$R
         )
         figure_2_recruitment(rv$r_df, rv$r_df$Year, rv$r_df$recruitment,rv$r_df$low_recruitment,rv$r_df$high_recruitment)

         #advice_action()
         
         })
         #### Plot 3 fish mortality 
    output$f <- renderPlotly({
        data_list = advice_action()
         
         rv <- reactiveValues(
             f_df = data_list$f
         )

        #### third plot
        figure_3_fish_mortality(rv$f_df, rv$f_df$Year, rv$f_df$low_F, rv$f_df$F, rv$f_df$high_F, rv$f_df$FLim, rv$f_df$Fpa, rv$f_df$FMSY)
    })
    #### Plot 4 SSB
    output$SSB <- renderPlotly({

        data_list = advice_action()
         
         rv <- reactiveValues(
             SSB_df = data_list$SSB
         )

        ### forth plot
        figure_4_SSB(rv$SSB_df, rv$SSB_df$Year, rv$SSB_df$low_SSB, rv$SSB_df$SSB, rv$SSB_df$high_SSB, rv$SSB_df$Blim, rv$SSB_df$Bpa, rv$SSB_df$MSYBtrigger)
    })
    #   rv <- reactiveValues(
    #     catches = data_sag %>% select(Year, catches, landings, discards) %>% na.omit()
    #   )
    #     #R = data_sag %>% select(Year, low_recruitment, recruitment, high_recruitment)
    #         #%>% na.omit(),
    #     #f = data_sag %>% select(Year, low_F, F, high_F, FLim, Fpa, FMSY) %>% na.omit(),
    #     #SSB = data_sag %>% select(Year, low_SSB, SSB, high_SSB, Blim, Bpa, MSYBtrigger) %>% na.omit()
    # #)

    #     figure_1_catches(rv$catches, rv$catches$Year, rv$catches$landings, rv$catches$discards)
    # })
      
    })
    
  #}
#)




###########################Example 2

# library(shiny)
# library(DT)
# library(tibble)



# library(htmlwidgets)
# library(dplyr)
# library(ggplot2)
# library(dygraphs)
# library(htmltools)
# library(widgetframe)
# library(icesSAG)
# library(plotly)

# library(shiny)
# library(shinythemes)
# library(glue)


# library(sf)
# library(leaflet)
# library(fisheryO)
# library(DT)
# library(tidyverse)


# #' Programmatically create a Shiny input
# #' 
# #' @param FUN function to create the input
# #' @param n number of inputs to be created
# #' @param id ID prefix for each input
# shinyInput <- function(FUN, n, id, ...) {

#   # for each of n, create a new input using the FUN function and convert
#   # to a character
#   vapply(seq_len(n), function(i){
#     as.character(FUN(paste0(id, i), ...))
#   }, character(1))
  
# }

# shinyApp(
#   ui <- fluidPage(
#     DTOutput("data"),
#     textOutput('myText')
#   ),
  
#   server <- function(input, output) {
    
#     # reactive data frame which creates the number of actionButtons needed
#     df <- reactiveVal(
#       tibble(
      
#         Name = c('Dilbert', 'Alice', 'Wally', 'Ashok', 'Dogbert'),
#         Motivation = c(62, 73, 3, 99, 52),
        
#         # parameters here:
#         #   * actionButton - type of input to create
#         #   * 5 - how many we need
#         #   * 'button_' - the ID prefix
#         #   * label - label to show on the button
#         #   * onclick - what to do when clicked
#         Actions = shinyInput(
#           FUN = actionButton,
#           n = 5,
#           id = 'button_',
#           label = "Fire",
#           onclick = 'Shiny.setInputValue(\"select_button\", this.id, {priority: \"event\"})'
#         )
#       )
#     )
    
#     # Table showing the data
#     output$data <- DT::renderDT({
      
#       df()
      
#     }, 
    
#       # Don't escape any HTML in the table (i.e. the actionButton)
#       escape = FALSE,
      
#       # turn off row selection otherwise you'll also select that row when you
#       # click on the actionButton 
#       selection = 'none'
#     )
    
#     # When a button is clicked, employee is set to the employee name
#     #  associated with the clicked row
#     employee <- eventReactive(input$select_button, {
#       # take the value of input$select_button, e.g. "button_1"
#       # get the button number (1) and assign to selectedRow
#       selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
      
#       # get the value of the "Name" column in the data.frame for that row
#       paste('click on ',df()[selectedRow,"Name"])
#     })
    
#     # Show the name of the employee that has been clicked on
#     output$myText <- renderText({
      
#       employee()
      
#     })
    
#   }
# )