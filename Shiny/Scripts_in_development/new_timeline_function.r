# library("gridExtra")
# library("ggpubr")
# figure <- grid.arrange(p1, p2, p3, p4,
#                     # labels = c("A", "B", "C"),
#                     ncol = 2, nrow = 2)
# figure

# ggplotly(figure, width = 1000, height = 500)

# m <- list(
#   l = 10,
#   r = 10,
#   b = 30,
#   t = 30,
#   pad = 4
# )
# (autosize = F, width = 500, height = 500, margin = m)
# plots_panel <-
#   mainPanel(
#     width = 8,
#     panel(
#       title = "plots",
#     #   fillPage(
#     #     tags$style(type = "text/css", "{height: calc(99vh - 200px) !important;}"),
#         fluidRow(
#             column(
#                 width = 6,
#                 plotlyOutput("plot1", height = "100%", width = "100%")
#             ),
#             column(
#                 width = 6,
#                 plotlyOutput("plot2", height = "100%", width = "100%")
#             ),
#         ),
#         fluidRow(
#             column(
#                 width = 6,
#                 plotlyOutput("plot3", height = "100%", width = "100%")
#             ),
#             column(
#                 width = 6,
#                 plotlyOutput("plot4", height = "100%", width = "100%")
#             ),
#         )

#       ),
#       h5(helpText("Stock Development over time"))
#     # )
#   )

# # advice plot side panel
# allocations_plotspanel <-
#   sidebarPanel(
#     width = 4,

#       panel(
#         title = "Quality of Assessment",
#         # fillPage(
#         #   tags$style(type = "text/css", "#plot4 {height: calc(99vh - 200px) !important;}"),
#           plotlyOutput("plot4", height = "100%", width = "100%")
#         ),
#         h5(helpText("Quality of Assessment"))
#         # actionButton("r_SSB", "Get Stock Data")
#     #   )
#         # )
#     # ),
#     # DTOutput("tbl_summary")
#     )

# tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "timeline.css"))

# shinyApp(
    
#     ui = basicPage(
#         tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "timeline.css")),
#         actionButton("show", "Show modal dialog")
#     ),
#     server = function(input, output) {
#         observeEvent(input$show, {
#             showModal(modalDialog(
#                 tags$body(HTML("<div class='container-fluid'>
#                                                     <div class='row'>
#                                                         <div class='col-lg-12'>
#                                                             <div class='card'>
#                                                                 <div class='card-body'>
#                                                                     <h4 class='card-title mb-5'>Horizontal Timeline</h4>

#                                                                     <div class='hori-timeline' dir='ltr'>
#                                                                         <ul class='list-inline events'>
#                                                                             <li class='list-inline-item event-list'>
#                                                                                 <div class='px-4'>
#                                                                                     <div class='event-date bg-soft-primary text-primary'>2 June</div>
#                                                                                     <h5 class='font-size-36'>Event One</h5>
#                                                                                     <p class='text-muted'>It will be as simple as occidental in fact it will be Occidental Cambridge friend</p>
#                                                                                     <div>
#                                                                                         <a href='#' class='btn btn-primary btn-sm'>Read more</a>
#                                                                                     </div>
#                                                                                 </div>
#                                                                             </li>
#                                                                             <li class='list-inline-item event-list'>
#                                                                                 <div class='px-4'>
#                                                                                     <div class='event-date bg-soft-success text-success'>5 June</div>
#                                                                                     <h5 class='font-size-16'>Event Two</h5>
#                                                                                     <p class='text-muted'>Everyone realizes why a new common language one could refuse translators.</p>
#                                                                                     <div>
#                                                                                         <a href='#' class='btn btn-primary btn-sm'>Read more</a>
#                                                                                     </div>
#                                                                                 </div>
#                                                                             </li>
#                                                                             <li class='list-inline-item event-list'>
#                                                                                 <div class='px-4'>
#                                                                                     <div class='event-date bg-soft-danger text-danger'>7 June</div>
#                                                                                     <h5 class='font-size-16'>Event Three</h5>
#                                                                                     <p class='text-muted'>If several languages coalesce the grammar of the resulting simple and regular</p>
#                                                                                     <div>
#                                                                                         <a href='#' class='btn btn-primary btn-sm'>Read more</a>
#                                                                                     </div>
#                                                                                 </div>
#                                                                             </li>
#                                                                             <li class='list-inline-item event-list'>
#                                                                                 <div class='px-4'>
#                                                                                     <div class='event-date bg-soft-warning text-warning'>8 June</div>
#                                                                                     <h5 class='font-size-16'>Event Four</h5>
#                                                                                     <p class='text-muted'>Languages only differ in their pronunciation and their most common words.</p>
#                                                                                     <div>
#                                                                                         <a href='#' class='btn btn-primary btn-sm'>Read more</a>
#                                                                                     </div>
#                                                                                 </div>
#                                                                             </li>
#                                                                         </ul>
#                                                                     </div>
#                                                                 </div>
#                                                             </div>
#                                                             <!-- end card -->
#                                                         </div>
#                                                     </div>")),
#                 # title = "Somewhat important message",
#                 # "This is a somewhat important message.",
#                 size = "l",
#                 easyClose = TRUE,
#                 footer = NULL
#             ))
#         })
#     }
# )


# library(shiny)
# library(shinyBS)

# shinyApp(
    
#  ui =
#  fluidPage(
#      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "timeline.css")),
#    sidebarLayout(
#      sidebarPanel(
#        sliderInput("bins",
#                    "Number of bins:",
#                    min = 1,
#                    max = 50,
#                    value = 30),
#        actionButton("tabBut", "View Table")
#      ),

#      mainPanel(
#        plotOutput("distPlot"),
#        bsModal("modalExample", "Data Table", "tabBut", size = "large",
# tags$body(HTML("<div class='container-fluid'>
#                                                     <div class='row'>
#                                                         <div class='col-lg-12'>
#                                                             <div class='card'>
#                                                                 <div class='card-body'>
#                                                                     <h4 class='card-title mb-5'>Horizontal Timeline</h4>

#                                                                     <div class='hori-timeline' dir='ltr'>
#                                                                         <ul class='list-inline events'>
#                                                                             <li class='list-inline-item event-list'>
#                                                                                 <div class='px-4'>
#                                                                                     <div class='event-date bg-soft-primary text-primary'>2 June</div>
#                                                                                     <h5 class='font-size-16'>Event One</h5>
#                                                                                     <p class='text-muted'>It will be as simple as occidental in fact it will be Occidental Cambridge friend</p>
#                                                                                     <div>
#                                                                                         <a href='#' class='btn btn-primary btn-sm'>Read more</a>
#                                                                                     </div>
#                                                                                 </div>
#                                                                             </li>
#                                                                             <li class='list-inline-item event-list'>
#                                                                                 <div class='px-4'>
#                                                                                     <div class='event-date bg-soft-success text-success'>5 June</div>
#                                                                                     <h5 class='font-size-16'>Event Two</h5>
#                                                                                     <p class='text-muted'>Everyone realizes why a new common language one could refuse translators.</p>
#                                                                                     <div>
#                                                                                         <a href='#' class='btn btn-primary btn-sm'>Read more</a>
#                                                                                     </div>
#                                                                                 </div>
#                                                                             </li>
#                                                                             <li class='list-inline-item event-list'>
#                                                                                 <div class='px-4'>
#                                                                                     <div class='event-date bg-soft-danger text-danger'>7 June</div>
#                                                                                     <h5 class='font-size-16'>Event Three</h5>
#                                                                                     <p class='text-muted'>If several languages coalesce the grammar of the resulting simple and regular</p>
#                                                                                     <div>
#                                                                                         <a href='#' class='btn btn-primary btn-sm'>Read more</a>
#                                                                                     </div>
#                                                                                 </div>
#                                                                             </li>
#                                                                             <li class='list-inline-item event-list'>
#                                                                                 <div class='px-4'>
#                                                                                     <div class='event-date bg-soft-warning text-warning'>8 June</div>
#                                                                                     <h5 class='font-size-16'>Event Four</h5>
#                                                                                     <p class='text-muted'>Languages only differ in their pronunciation and their most common words.</p>
#                                                                                     <div>
#                                                                                         <a href='#' class='btn btn-primary btn-sm'>Read more</a>
#                                                                                     </div>
#                                                                                 </div>
#                                                                             </li>
#                                                                         </ul>
#                                                                     </div>
#                                                                 </div>
#                                                             </div>
#                                                             <!-- end card -->
#                                                         </div>
#                                                     </div>")),

#          dataTableOutput("distTable"))
#      )
#    )
#  ),
#  server =
#  function(input, output, session) {

#    output$distPlot <- renderPlot({

#      x    <- faithful[, 2]
#      bins <- seq(min(x), max(x), length.out = input$bins + 1)

#      # draw the histogram with the specified number of bins
#      hist(x, breaks = bins, col = 'darkgray', border = 'white')

#    })

#    output$distTable <- renderDataTable({

#      x    <- faithful[, 2]
#      bins <- seq(min(x), max(x), length.out = input$bins + 1)

#      # draw the histogram with the specified number of bins
#      tab <- hist(x, breaks = bins, plot = FALSE)
#      tab$breaks <- sapply(seq(length(tab$breaks) - 1), function(i) {
#        paste0(signif(tab$breaks[i], 3), "-", signif(tab$breaks[i+1], 3))
#      })
#      tab <- as.data.frame(do.call(cbind, tab))
#      colnames(tab) <- c("Bins", "Counts", "Density")
#      return(tab[, 1:3])

#    }, options = list(pageLength=10))

#  }
# )
library(shiny)
library(shinyalert)

ui <- fluidPage(
#   useShinyalert(),  # Set up shinyalert

  actionButton("preview", "Advice Timeline", style="color: #fff; background-color: #337ab7; border-color: #2e6da4; padding:10px; font-size:150%")
)

server <- function(input, output, session) {
  observeEvent(input$preview, {
    # Show a modal when the button is pressed
    shinyalert(title= " Advice Timeline", 
    # includeHTML("D:/Profile/Documents/GitHub/online-advice/Shiny/Scripts_in_development/timeline3.html"),
    tags$body(HTML(html_timeline())),
            type = "info",
            html=TRUE,
            closeOnClickOutside = TRUE,
            confirmButtonText = "Close",
            size = "s",
            )
  })
}

shinyApp(ui, server)


get_advice_timeline <- function(stock_code, tbl_sid, tbl_rows_selected) {
    ## this gets the initial dates from the advice view
    timeL <- get_Advice_View_info(stock_code)

    release_date <- timeL[timeL["advice View"] == "adviceReleasedDate", 2]
    applicable_from <- timeL[timeL["advice View"] == "adviceApplicableFrom", 2]
    applicable_until <- timeL[timeL["advice View"] == "adviceApplicableUntil", 2]

    ## This block formats the dates from dd/mm/yyy to Yyyy-mm-dd
    release_date <- strptime(as.character(release_date), "%d/%m/%Y")
    release_date <- format(release_date, "%Y-%m-%d")
    applicable_from <- strptime(as.character(applicable_from), "%d/%m/%Y")
    applicable_from <- format(applicable_from, "%Y-%m-%d")
    applicable_until <- strptime(as.character(applicable_until), "%d/%m/%Y")
    applicable_until <- format(applicable_until, "%Y-%m-%d")


    ## This block gets the name of the working group from the currently selected row
    filtered_row <- tbl_sid[tbl_rows_selected, ]
    WG <- filtered_row$ExpertGroupUrl
    WG <- str_match(WG, "\\>\\s*(.*?)\\s*\\<\\/a>")[,2]

    ## This block scrapes the meeting-calendar webpage to find the dates of the upcoming WG meeting
    page <- read_html(paste0("https://www.ices.dk/news-and-events/meeting-calendar/Pages/ICES-CalendarSearch.aspx?k=", WG))
    
    start_date <- page %>%
        html_nodes("td") %>%
        html_text()

    title_meeting <- start_date[1]
    descr_group <- start_date[4]
    meeting_loc <- start_date[5]
    ## This block extracts and formats the dates as above
    start_WG <- strapplyc(start_date[2], "\\d+/\\d+/\\d+", simplify = TRUE)
    end_WG <- strapplyc(start_date[3], "\\d+/\\d+/\\d+", simplify = TRUE)
    start_WG <- strptime(as.character(start_WG), "%d/%m/%Y")
    start_WG <- format(start_WG, "%Y-%m-%d")
    end_WG <- strptime(as.character(end_WG), "%d/%m/%Y")
    end_WG <- format(end_WG, "%Y-%m-%d")

    ## This blocks create the df that timevis will display
    data <- data.frame(
        id      = 1:3,
        content = c("Advice Release Date", "Advice Applicable Between", title_meeting),
        start   = c(release_date, applicable_from, start_WG),
        end     = c(NA, applicable_until, end_WG)
    )
    

    return(data)
}





html_timeline <- function(stock_code, tbl_sid, tbl_rows_selected) {
    ## this gets the initial dates from the advice view
    timeL <- get_Advice_View_info(stock_code)

    release_date <- timeL[timeL["advice View"] == "adviceReleasedDate", 2]
    applicable_from <- timeL[timeL["advice View"] == "adviceApplicableFrom", 2]
    applicable_until <- timeL[timeL["advice View"] == "adviceApplicableUntil", 2]

    ## This block gets the name of the working group from the currently selected row
    filtered_row <- tbl_sid[tbl_rows_selected, ]
    WG <- filtered_row$ExpertGroupUrl
    WG <- str_match(WG, "\\>\\s*(.*?)\\s*\\<\\/a>")[,2]

    ## This block scrapes the meeting-calendar webpage to find the dates of the upcoming WG meeting
    page <- read_html(paste0("https://www.ices.dk/news-and-events/meeting-calendar/Pages/ICES-CalendarSearch.aspx?k=", WG))
    
    start_date <- page %>%
        html_nodes("td") %>%
        html_text()

    title_meeting <- start_date[1]
    descr_group <- start_date[4]
    meeting_loc <- start_date[5]
    ## This block extracts and formats the dates as above
    start_WG <- strapplyc(start_date[2], "\\d+/\\d+/\\d+", simplify = TRUE)
    end_WG <- strapplyc(start_date[3], "\\d+/\\d+/\\d+", simplify = TRUE)

    html_timeline_string <- paste0("
                                <style>
                            /* (A) TIMELINE CONTAINER */
                        .vtl {
                        /* (A1) RELATIVE POSITION REQUIRED TO PROPERLY POSITION THE TIMELINE */
                        position: relative;

                        /* (A2) RESERVE MORE SPACE TO THE LEFT FOR THE TIMELINE */
                        padding: 10px 10px 10px 50px;

                        /* (A3) OPTIONAL WIDTH RESTRICTION */
                        max-width: 400px;
                        }
                        .vtl, .vtl * { box-sizing: border-box; }

                        /* (B) DRAW VERTICAL LINE USING ::BEFORE */
                        .vtl::before {
                        /* (B1) VERTICAL LINE */
                        content: '';
                        width: 5px;
                        background-color: #de421a;

                        /* (B2) POSITION TO THE LEFT */
                        position: absolute;
                        top: 0; bottom: 0; left: 15px;
                        }

                        /* (C) COSMETICS FOR EVENTS */
                        div.event {
                        padding: 20px 30px;
                        background-color: #ffebeb;
                        position: relative;
                        border-radius: 6px;
                        margin-bottom: 10px;
                        }

                        /* (D) COSMETICS FOR EVENT DATE & TEXT */
                        p.date {
                        font-size: 1.1em;
                        font-weight: 700;
                        color: #ff6a00;
                        }
                        p.txt {
                        margin: 10px 0 0 0;
                        color: #222;
                        }

                        /* (E) EVENT 'SPEECH BUBBLE CALLOUT' */
                        div.event::before {
                        /* (E1) 'MAGIC TRIANGLE' */
                        content: '';
                        border: 10px solid transparent;
                        border-right-color: #ffebeb;
                        border-left: 0;

                        /* (E2) POSITION TO THE LEFT */
                        position: absolute;
                        top: 20%; left: -10px;
                        }

                        /* (F) CIRCLE ON TIMELINE */
                        div.event::after {
                        /* (F1) 'MAGIC CIRCLE' */
                        content: '';
                        background: #fff;
                        border: 4px solid #DE421A;
                        width: 16px; height: 16px;
                        border-radius: 50%;

                        /* (F2) POSITION TO THE LEFT */
                        position: absolute;
                        top: 20%; left: -40px;
                        }
                        </style>


                        <h1>Stockcode: ", stock_code, "</h1>
                        <div class='vtl'>
                        <div class='event'>
                            <p class='date'>", "Not Available", "</p>
                            <p class='txt'>", "Previous Benchmark", "</p>
                        </div>
                        <div class='event'>
                            <p class='date'>", release_date, "</p>
                            <p class='txt'>Advice release</p>
                        </div>
                        <div class='event'>
                            <p class='date'>", applicable_from,  " - ", applicable_until, "</p>
                            <p class='txt'>Advice validity</href>
                        </div>
                        <div class='event'>
                            <p class='date'>", start_WG, " - ", end_WG, "</p>
                            <p class='txt'>", title_meeting, "<br/>", descr_group, "<br/>", "Location: ", meeting_loc, "</p>
                        </div>
                        <div class='event'>
                            <p class='date'>Not Available</p>
                            <p class='txt'>Next benchmark</p>
                        </div>
                        </div>")
return(html_timeline_string)
}
