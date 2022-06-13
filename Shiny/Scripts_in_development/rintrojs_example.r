library(rintrojs)
library(shiny)
library(shinydashboard)

ui <- shinyUI(
    dashboardPage(
        dashboardHeader(title = "Basic dashboard"),
        dashboardSidebar(
            introjsUI(),
            sidebarMenu(
                menuItem("Item1", tabName="item1", icon=icon("dashboard")),
                menuItem("Item2", tabName="item2", icon=icon("thumbs-up"))
            )
        ),
        dashboardBody(
            fluidPage(
                titlePanel("Old Faithful Geyser Data"),
                sidebarLayout(
                    sidebarPanel(
                        sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30),
                        actionButton("help", "Press for instructions"),
                    ),
                    mainPanel(
                        plotOutput("distPlot"),
                    )
                )
            )
        )
    )
)

server <- shinyServer(function(input, output, session) {
    steps <- reactive(
        data.frame(
            element=c(".sidebar-menu", ".main-header", ".sidebar-toggle", ".active", "#help"),
            intro=c(
                "This is a sidebar. Note that we access it with '.' instead of '#', because we track its class and not its id.",
                "This is a header.",
                "This is a button that allows to close and open the sidebar.",
                "This is the active element of the sidebar.",
                "This is a button that I added just to show the normal way to point to elements: with their id."
            ),
            position=c("right", "bottom", "bottom", "right", "top")
        )
    )
    observeEvent(input$help,
        introjs(session,
            options = list(steps=steps(),
                "nextLabel"="Next",
                "prevLabel"="Previous",
                "skipLabel"="Skip"
            ),
            events = list("oncomplete"=I('alert("Done")'))
        )
    )

    output$distPlot <- renderPlot({
        x <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
})

shinyApp(ui = ui, server = server)


##################################### Second example

library(rintrojs)
library(shiny)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  introjsUI(),
  
  # Application title
  introBox(
    titlePanel("Old Faithful Geyser Data"),
    data.step = 1,
    data.intro = "This is the title panel"
  ),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(sidebarPanel(
    introBox(
      introBox(
        sliderInput(
          "bins",
          "Number of bins:",
          min = 1,
          max = 50,
          value = 30
        ),
        data.step = 3,
        data.intro = "This is a slider",
        data.hint = "You can slide me"
      ),
      introBox(
        actionButton("help", "Press for instructions"),
        data.step = 4,
        data.intro = "This is a button",
        data.hint = "You can press me"
      ),
      data.step = 2,
      data.intro = "This is the sidebar. Look how intro elements can nest"
    )
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    introBox(
      plotOutput("distPlot"),
      data.step = 5,
      data.intro = "This is the main plot"
    )
  ))
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  # initiate hints on startup with custom button and event
  hintjs(session, options = list("hintButtonLabel"="Hope this hint was helpful"),
         events = list("onhintclose"=I('alert("Wasn\'t that hint helpful")')))
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x,
         breaks = bins,
         col = 'darkgray',
         border = 'white')
  })
  
  # start introjs when button is pressed with custom options and events
  observeEvent(input$help,
               introjs(session, options = list("nextLabel"="Onwards and Upwards",
                                               "prevLabel"="Did you forget something?",
                                               "skipLabel"="Don't be a quitter"),
                                events = list("oncomplete"=I('alert("Glad that is over")')))
  )
})

# Run the application
shinyApp(ui = ui, server = server)

########################################## 3rd example


library(shiny)
library(rintrojs)
ui<-fluidPage(
  introjsUI(),
  titlePanel('My dashboard'),
  tabsetPanel(
    #=============================================Firsttab==================================================
    tabPanel(
      "First tab",
      sidebarLayout(
        sidebarPanel(
          width = 5,
          actionButton("help_tab1", "About this Page"),
          h5("Before you begin any analysis, you may to read this text"),
          br(),
          h5("Here is some text that explains some stuff"),
          numericInput("numericinput1", "Put a number in here",1000),
          actionButton(inputId = "actionbutton1", "Button")),
        mainPanel(
          fluidRow(
            column(5, textOutput("text1"))
          )
        )
      )
    ),
    
    #==================================================================Secondtab===========================================
    tabPanel(
      "Second tab",
      sidebarPanel(
        width = 4,
        actionButton("help_tab2", "About this Page"),
        h5("Here is some text"),
        numericInput("numericinput2","Put a number in here",5), 
        br(),
        h5("Some more text")
      ),
      
      mainPanel(
        textOutput("text5")
      )
    ),#end of tab
    
    #===================================================================================Thirdtab=================================
    tabPanel(
      "Third tab",
      sidebarPanel(
        width = 4,
        actionButton("help_tab3", "About this Page"),
        h5("Here is some text op tab 3"),
        numericInput("numericinput4","Put a number in here",3), 
        br(),
        h5("Some more text"),
        mainPanel(
          textOutput("text6")
        )
      )
    )
  )
)



server <- function(input, output, session) {
  
  help_text <- reactive({
    if (input$help_tab1) whichtab <- "help_tab1"
    if (input$help_tab2) whichtab <- "help_tab2"
    if (input$help_tab3) whichtab <- "help_tab3"
    subset(helptext, tab == whichtab)
  })
  
  observeEvent(input$help_tab1,
               introjs(session, options = list("showBullets"="false", "showProgress"="true", 
                                               "showStepNumbers"="false","nextLabel"="Next","prevLabel"="Prev","skipLabel"="Skip", steps=help_text()))
  )
  
  observeEvent(input$help_tab2,
               introjs(session, options = list("showBullets"="false", "showProgress"="true", 
                                               "showStepNumbers"="false","nextLabel"="Next","prevLabel"="Prev","skipLabel"="Skip", steps=help_text()))
  )
  
  observeEvent(input$help_tab3,
               introjs(session, options = list("showBullets"="false", "showProgress"="true", 
                                               "showStepNumbers"="false","nextLabel"="Next","prevLabel"="Prev","skipLabel"="Skip", steps=help_text()))
  )
  
}

helptext <- data.frame(
  tab = c("help_tab1", "help_tab1", "help_tab2", "help_tab3")
  , step = c(1,2,1,1)
  , element = c("#numericinput1", "#actionbutton1", "#numericinput2", "#numericinput4")
  , intro = c("You should put a number in this box","Press this button for something to happen","Put a number in this box","Put a number in this box")
)


shinyApp(ui,server)     



###################################################


library(shiny)
library(rintrojs)
library(data.table)

ui<-fluidPage(
  introjsUI(),
  titlePanel('My dashboard'),
  tabsetPanel(
    #=============================================Firsttab==================================================
    tabPanel(
      "First tab",
      sidebarLayout(
        sidebarPanel(
          width = 5,
          actionButton("help_tab1", "About this Page"),
          h5("Before you begin any analysis, you may to read this text"),
          br(),
          h5("Here is some text that explains some stuff"),
          numericInput("numericinput1", "Put a number in here",1000),
          actionButton(inputId = "actionbutton1", "Button")),
        mainPanel(
          fluidRow(
            column(5, textOutput("text1"))
          )
        )
      )
    ),
    
    #==================================================================Secondtab===========================================
    tabPanel(
      "Second tab",
      sidebarPanel(
        width = 4,
        actionButton("help_tab2", "About this Page"),
        h5("Here is some text"),
        numericInput("numericinput2","Put a number in here",5), 
        br(),
        h5("Some more text")
      ),
      
      mainPanel(
        textOutput("text5")
      )
    ),#end of tab
    
    #===================================================================================Thirdtab=================================
    tabPanel(
      "Third tab",
      sidebarPanel(
        width = 4,
        actionButton("help_tab3", "About this Page"),
        h5("Here is some text op tab 3"),
        numericInput("numericinput4","Put a number in here",3), 
        br(),
        h5("Some more text"),
        mainPanel(
          textOutput("text6")
        )
      )
    )
  )
)



server <- function(input, output, session) {
  
  helptext <- reactive(data.table(
    tab = c("help_tab1", "help_tab1", "help_tab2", "help_tab3"),
    step = c(1,2,1,1),
    element = c("#numericinput1", "#actionbutton1", "#numericinput2", "#numericinput4"),
    intro = c("You should put a number in this box","Press this button for something to happen","Put a number in this box","Put a number in this box")
  ))
  
  observeEvent(
    eventExpr = input$help_tab1,
    handlerExpr = {
      introjs(session, 
              options = list(
                "showBullets"="false", "showProgress"="true", 
                "showStepNumbers"="false","nextLabel"="Next","prevLabel"="Prev","skipLabel"="Skip",
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
                "showBullets"="false", "showProgress"="true", 
                "showStepNumbers"="false","nextLabel"="Next","prevLabel"="Prev","skipLabel"="Skip",
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
                "showBullets"="false", "showProgress"="true", 
                "showStepNumbers"="false","nextLabel"="Next","prevLabel"="Prev","skipLabel"="Skip",
                steps=helptext()[tab == "help_tab3"]
              )
      )
    }
  )
  
}



shinyApp(ui,server) 


###########################################################################

library(shiny)
library(rintrojs)

ui = shinyUI(tagList(
  introjsUI(),
  navbarPage(
    "Old Faithful Geyser Data",

    tabPanel(
      id = "fTab",
      "First tab",
      introBox(
        h1("Basic Usage"),
        data.step = 1,
        data.intro = "This is a tooltip"
      ),
      sliderInput(
        "bins",
        "Number of bins:",
        min = 1,
        max = 50,
        value = 30
      ),

      plotOutput("distPlot"),
      actionButton("startButton", "Help")
    ),
    tabPanel(
      tabName = "sTab",
      "Second tab",
      id = "tt",
      introBox(
        h1("Basic Usage 2"),
        data.step = 2,
        data.intro = "This is a second tooltip"
      ),
      sliderInput(
        "bins2",
        "Number of bins:",
        min = 1,
        max = 50,
        value = 30
      ),

      plotOutput("distPlot2")
    )
  )
))

server = shinyServer(function(input, output, session) {
  output$distPlot <- renderPlot({
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)


    hist(x,
         breaks = bins,
         col = 'darkgray',
         border = 'white')

  })
  output$distPlot2 <- renderPlot({
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins2 + 1)


    hist(x,
         breaks = bins,
         col = 'darkgray',
         border = 'white')

  })

  observeEvent(input$startButton, {
    introjs(
      session,
      events = list(
        "onchange" = "if (this._currentStep==0) {
        $('a[data-value=\"Second tab\"]').removeClass('active');
        $('a[data-value=\"First tab\"]').addClass('active');
        $('a[data-value=\"First tab\"]').trigger('click');
  }
        if (this._currentStep==1) {
        $('a[data-value=\"First tab\"]').removeClass('active');
        $('a[data-value=\"Second tab\"]').addClass('active');
        $('a[data-value=\"Second tab\"]').trigger('click');
        }"
)
      )

})

  })

shinyApp(ui = ui, server = server)

###################################################
library(shiny)


shinyUI(tagList(
  tags$head(
    HTML("<link rel='stylesheet' type='text/css' href='css/introjs.min.css'>")
  ),
  navbarPage("Old Faithful Geyser Data",
             tabPanel(id = "fTab", "First tab",
                      HTML("<h1 data-step='1' data-intro='This is a tooltip!'>Basic Usage</h1>"),
                      sliderInput("bins",
                                  "Number of bins:",
                                  min = 1,
                                  max = 50,
                                  value = 30),

                      plotOutput("distPlot"),
                      HTML("<a id='startButton' class='btn btn-large btn-success' href='javascript:void(0);'>Help</a>")

             ),
             tabPanel(tabName = "sTab", "Second tab", id = "tt", 
                      HTML("<h1 data-step='2' data-intro='This is a second tooltip!'>Basic Usage</h1>"),
                      sliderInput("bins2",
                                  "Number of bins:",
                                  min = 1,
                                  max = 50,
                                  value = 30),

                      plotOutput("distPlot2")
             )
  ),
  HTML("<script type='text/javascript' src='js/intro.min.js'></script>"),
  HTML("<script type='text/javascript'>document.getElementById('startButton').onclick = function() {
       introJs().onchange(function(targetElement) {
          if (this._currentStep==0) {
             $('a[data-value=\"Second tab\"]').removeClass('active');
             $('a[data-value=\"First tab\"]').addClass('active');
             $('a[data-value=\"First tab\"]').trigger('click');
          }
          if (this._currentStep==1) {
             $('a[data-value=\"First tab\"]').removeClass('active');
             $('a[data-value=\"Second tab\"]').addClass('active');
             $('a[data-value=\"Second tab\"]').trigger('click');
          }
       }).start();
       };</script>")  
    ))

####################################################################################################
library(shiny)
library(rintrojs)

ui <- fluidPage(
  rintrojs::introjsUI(),
  tabsetPanel(id = "tabs",
              tabPanel("one",h1("One", id = "one")),
              tabPanel("two",h1("Two", id = "two"))),
  actionButton("intro", "Start Intro")
)

server <- function(input,output,session) {
  observeEvent(input$intro, {
    if (input$tabs == "one") {
      rintrojs::introjs(session, options = list(
        steps = data.frame(element = c(NA, "#one"),
                   intro = c("This first step is the same regardless of the tab, but the second step is different",
                             "This is the first tab"))
      ))
    } else if (input$tabs == "two") {
      rintrojs::introjs(session, options = list(
        steps = data.frame(element = c(NA, "#two"),
                           intro = c("This first step is the same regardless of the tab, but the second step is different",
                                     "This is the second tab"))
      ))
    }
  })
}

shinyApp(ui,server)
###################################################################################################
library(shiny)
library(rintrojs)

# Example adapted from https://shiny.rstudio.com/articles/tabsets.html
ui <- fluidPage(
  introjsUI(),
  titlePanel("Tabsets"),
  sidebarLayout(
    sidebarPanel(
      introBox(
        radioButtons(
          "dist", "Distribution type:",
          c(
            "Normal" = "norm",
            "Uniform" = "unif",
            "Log-normal" = "lnorm",
            "Exponential" = "exp"
          )
        ),
        data.step = 1,
        data.intro = "This is an input"
      ),
      br(),
      introBox(
        sliderInput(
          "n",
          "Number of observations:",
          value = 500,
          min = 1,
          max = 1000
        ),
        data.step = 2,
        data.intro = "And so is this"
      )
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Histogram",
          introBox(
            plotOutput("histogram"),
            data.step = 3,
            data.intro = "Took a look at this histogram"
          )
        ),
        tabPanel(
          "Boxplot", 
          introBox(
            plotOutput("boxplot"),
            data.step = 4,
            data.intro = "And this handsome boxplot.<br><br>See how the active tab
            switched from 'Histogram' to 'Boxplot' before moving to this step?"
          )
        ),
        tabPanel(
          "Summary",
          tags$br(),
          tabsetPanel(
            tabPanel("Summary sub-tab 1", HTML("<br>Pretty boring tab, right?")),
            tabPanel(
              "Summary sub-tab 2",
              introBox(
                verbatimTextOutput("summary"),
                data.step = 5,
                data.intro = "There it goes again..."
              )
            )
          )
        )
      )
    )
  )
)

server <- shinyServer(function(input, output, session) {
  
  d <- reactive({
    dist <- switch(input$dist,
      norm = rnorm,
      unif = runif,
      lnorm = rlnorm,
      exp = rexp,
      rnorm
    )
    dist(input$n)
  })

  output$histogram <- renderPlot({
    dist <- input$dist
    n <- input$n

    hist(
      d(),
      main = paste("r", dist, "(", n, ")", sep = ""),
      col = "#75AADB", border = "white"
    )
  })
  
  output$boxplot <- renderPlot({
    dist <- input$dist
    n <- input$n

    boxplot(
      d(),
      main = paste("r", dist, "(", n, ")", sep = ""),
      col = "#75AADB", border = "white"
    )
  })
  
  output$summary <- renderPrint({
    summary(d())
  })
  
  introjs(session, events = list(onbeforechange = readCallback("switchTabs")))
})

shinyApp(ui, server)



######################################################

library(shiny)
library(rintrojs)

ui <- navbarPage(

  title = "foo",
  introjsUI(),
  tabPanel(
    title = introBox("Panel 1",
             data.step = 1,
             data.intro = "This is Panel 1"),
    fluidRow(actionButton("button1", "Button 1"))
  ), 

  tabPanel(
    title = introBox("Panel 2",
             data.step = 2,
             data.intro = "This is Panel 2"),
    fluidRow(actionButton("button2", "Button 2"))
  )

  # If you want to see a "normal" app, comment from "introjsUI()" to here, and uncomment the chunk below 
  # tabPanel(title = "Panel 1",
  #          fluidRow(actionButton("button1", "Button 1"))
  # ),
  # tabPanel(title = "Panel 2",
  #          fluidRow(actionButton("button2", "Button 2"))
  # )
)

server <- shinyServer(function(input, output, session) {

  introjs(session)

})

shinyApp(ui, server)


##################### action button in navbar
library(shiny)
library(markdown)

ui <- navbarPage("Navbar!",
                 tabPanel("Plot",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("plotType", "Plot type",
                                           c("Scatter"="p", "Line"="l")
                              )
                            ),
                            mainPanel(
                              plotOutput("plot")
                            )
                          )
                 ),
                 tabPanel("Summary",
                          verbatimTextOutput("summary")
                 ),
                 tags$script(
                   HTML("var header = $('.navbar > .container-fluid');
                              header.append('<div style=\"float:right; padding-top: 8px\"><button id=\"signin\" type=\"button\" class=\"btn btn-primary action-button\" onclick=\"signIn()\">Sign In</button></div>')")
                 )
)



server <- function(input, output, session) {
  output$plot <- renderPlot({
    plot(cars, type=input$plotType)
  })

  output$summary <- renderPrint({
    summary(cars)
  })

  output$table <- DT::renderDataTable({
    DT::datatable(cars)
  })
}

server <- function(input, output, session) {
  output$plot <- renderPlot({
    plot(cars, type=input$plotType)
  })

  output$summary <- renderPrint({
    summary(cars)
  })

  output$table <- DT::renderDataTable({
    DT::datatable(cars)
  })
}

shinyApp(ui = ui, server = server)