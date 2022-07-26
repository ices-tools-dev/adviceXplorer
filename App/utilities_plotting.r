### Plotting utilities

## Formatting axis title (titlefont)
titlefont_format <- function() {
    f1 <- list(
        family = "Calibri, sans-serif",
        size = 25,
        color = "darkgrey")
}
## Formatting tick axis font (tickfont)
tickfont_format <- function() {
    f2 <- list(
        family = "Calibri, serif",
        size = 20,
        color = "black")
}
## Formatting legend
legend_format <- function() {
    leg <- list(
        font = list(
        family = "sans-serif",
        size = 20,
        color = "#000"),
        grouptitlefont = list(
            color = "black",
            family = "sans-serif",
            size = 24
        ),
        bgcolor = "rgb(233,244,245)",
        bordercolor = "#FFFFFF",
        borderwidth = 2,
        orientation = 'v',
        tracegroupgap = 30,
        traceorder = "grouped")
        # yanchor = "bottom",
        # xanchor = "center",
        # x = 0.5,
        # y = 0.1)
}

# watermark <- function() {
#     images <- list(
#             source = "ICES_logo.png",
#             xref = "paper",
#             yref = "paper",
#             x= 0.02,
#             y= 0.98,
#             sizex = 0.1,
#             sizey = 0.1,
#             opacity = 0.5)
# }
#' Returns ....
#'
#' Downloads ...
#'
#' @param stock_name
#'
#' @return 
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' 
#' }
#'
#' @references
#'
#' 
#'
#' @export
#' 
################## Plot 1 - Catches ################
# figure_1_catches <- function(data, years, catches, landings, discards) {
#     #Make sure that if the column landings is empy, the column catches is plotted
#     if (all(is.na(data[, "landings"]))) {
#         data$landings <- data$catches
#     }
#     # Start the plot
#     fig1 <- plot_ly(
#         data = data,
#         x = ~years,
#         y = ~landings,
#         name = "Landings",
#         type = "bar",
#         hoverinfo = 'text',
#         text = ~paste('Year:', Year, '<br>Landings:', landings),
#         marker = list(color = '#66a4cd',
#                       line = list(color = 'black',
#                                   width = 0.5)),
#         showlegend = TRUE,
#         legendgroup = "A")

#     fig1 <- fig1 %>% add_trace(
#         data = data,
#         x = ~years,
#         y = ~discards,
#         name = "Discards",
#         type = "bar",
#         hoverinfo = 'text',
#         text = ~paste('Year:', Year, '<br>Discards:', discards),
#         marker = list(color = '#a00130',
#                         line = list(color = 'black',
#                                     width = 0.5)),
#         showlegend = TRUE,
#         legendgroup = "A")

#     fig1 <- fig1 %>% layout(title = "Catches",
#             xaxis = list(title = "Years",
#             titlefont = titlefont_format(),
#             tickfont = tickfont_format(),
#             showticklabels = TRUE),
#             barmode = "stack",
#             legend = legend_format(),
#             yaxis = list(title = "Catches",
#             titlefont = titlefont_format(),
#             tickfont = tickfont_format(),
#             showticklabels = TRUE))
#     fig1

# }

# # figure_1_catches(catches, catches$Year, catches$landings, catches$discards)

# #' Returns ....
# #'
# #' Downloads ...
# #'
# #' @param stock_name
# #'
# #' @return 
# #'
# #' @note
# #' Can add some helpful information here
# #'
# #' @seealso
# #'
# #' @examples
# #' \dontrun{
# #' 
# #' }
# #'
# #' @references
# #'
# #' 
# #'
# #' @export
# #' 
# ################## Plot 2 - Recruitment ################
# figure_2_recruitment <- function(data, years, recruitment, low_recruitment, high_recruitment){
#     fig2 <- plot_ly(
#             data = data,
#             x = ~years,
#             y = ~recruitment,
#             name = "recruitment",
#             type = "bar",
#             hoverinfo = "text",
#             text = ~ paste("Year:", years, "<br>recruitment:", recruitment),
#             marker = list(
#                 color = "#cd6666",
#                 line = list(
#                     color = "black",
#                     width = 0.5
#                 )
#             ),
#             error_y = list(
#                 type = "data",
#                 symmetric = FALSE,
#                 arrayminus = ~low_recruitment,
#                 array = ~ high_recruitment,
#                 color = "#000000"),
#                 legendgroup = "A"
#                 #orientation = "v",
#                 #type = "bar"
#             #)
#             # error_y = list(
#             #     array = ~err,
#             #     type = "data",
#             #     color = "#000000"
#             # )
#         )

#         fig2 <- fig2 %>% layout(
#             title = "Recruitment",
#             xaxis = list(
#                 title = "Years",
#                 titlefont = titlefont_format(),
#                 tickfont = tickfont_format(),
#                 showticklabels = TRUE
#             ),
#             yaxis = list(
#                 title = "Recruitment",
#                 titlefont = titlefont_format(),
#                 tickfont = tickfont_format(),
#                 showticklabels = TRUE
#             )
#         )

#     fig2

# }
# # figure_2_recruitment(R, R$Year, R$recruitment,R$low_recruitment,R$high_recruitment)

# #' Returns ....
# #'
# #' Downloads ...
# #'
# #' @param stock_name
# #'
# #' @return 
# #'
# #' @note
# #' Can add some helpful information here
# #'
# #' @seealso
# #'
# #' @examples
# #' \dontrun{
# #' 
# #' }
# #'
# #' @references
# #'
# #' 
# #'
# #' @export
# #' 
# ################## Plot 3 - Fish Mortality ################
# figure_3_fish_mortality <- function(data, years, low_F, F, high_F, FLim, Fpa, FMSY){
#     fig3 <- plot_ly(
#         data = data, 
#         x = ~years, 
#         y = ~high_F, 
#         type = "scatter", 
#         mode = "lines",
#         line = list(color = "transparent", shape = "linear"),#
#         showlegend = FALSE, 
#         name = "high_F"
#     )
#     fig3 <- fig3 %>% add_trace(
#         data = data, 
#         y = ~low_F, 
#         type = "scatter", 
#         mode = "lines",
#         fill = "tonexty", 
#         fillcolor = "rgba(255,71,26,0.2)", #"rgba(0,100,80,0.2)"
#         line = list(color = "transparent", shape = "linear"),
#         showlegend = FALSE, 
#         name = "low_F"
#     )
#     fig3 <- fig3 %>% add_trace(
#         data = data, 
#         x = ~years, 
#         y = ~F, 
#         type = "scatter", 
#         mode = "lines+markers",
#         line = list(color = "rgb(255,71,26)", shape = "linear"), 
#         name = "F",
#         marker = list(size = 10, color = "rgb(255,71,26)"), 
#         showlegend = TRUE
#     )

#     ## Add horizontal lines
#     fig3 <- fig3 %>% add_trace(
#         data = data, 
#         x = ~years, 
#         y = ~FLim, 
#         name = "FLim", 
#         type = "scatter", 
#         mode = "lines",
#         line = list(color = "black", shape = "linear", dash = "dash"), 
#         showlegend = TRUE
#     )
#     fig3 <- fig3 %>% add_trace(
#         data = data, 
#         x = ~years, 
#         y = ~Fpa, 
#         name = "Fpa", 
#         type = "scatter", 
#         mode = "lines",
#         line = list(color = "black", shape = "linear", dash = "dot"), 
#         showlegend = TRUE
#     )
#     fig3 <- fig3 %>% add_trace(
#         data = data, 
#         x = ~years, 
#         y = ~FMSY, 
#         name = "FMSY", 
#         type = "scatter", 
#         mode = "lines",
#         line = list(color = "orange", shape = "linear", dash = "dash"), 
#         showlegend = TRUE
#     )

#     fig3 <- fig3 %>% layout(
#         title = "F", 
#         legend = legend_format(),
#         paper_bgcolor = "rgb(255,255,255)", 
#         plot_bgcolor = "rgb(229,229,229)",
#         xaxis = list(
#             title = "Years",
#             gridcolor = "rgb(255,255,255)",
#             showgrid = TRUE,
#             showline = TRUE,
#             showticklabels = TRUE,
#             tickcolor = "rgb(127,127,127)",
#             ticks = "outside",
#             zeroline = TRUE,
#             titlefont = titlefont_format(),
#             tickfont = tickfont_format()
#         ),
#         yaxis = list(
#             title = "F",
#             gridcolor = "rgb(255,255,255)",
#             showgrid = TRUE,
#             showline = TRUE,
#             showticklabels = TRUE,
#             tickcolor = "rgb(127,127,127)",
#             ticks = "outside",
#             zeroline = TRUE,
#             titlefont = titlefont_format(),
#             tickfont = tickfont_format()
#         )
#     )

#     fig3
#     }

# # figure_3_fish_mortality(f, f$Year, f$low_F, f$F, f$high_F, f$FLim, f$Fpa, f$FMSY)
# #' Returns ....
# #'
# #' Downloads ...
# #'
# #' @param stock_name
# #'
# #' @return 
# #'
# #' @note
# #' Can add some helpful information here
# #'
# #' @seealso
# #'
# #' @examples
# #' \dontrun{
# #' 
# #' }
# #'
# #' @references
# #'
# #' 
# #'
# #' @export
# #' 
# ################## Plot 4 - SBB ################
# figure_4_SSB <- function(data, years, low_SSB, SSB, high_SSB, Blim, Bpa, MSYBtrigger){
#     fig4 <- plot_ly(
#         data = data, 
#         x = ~years, 
#         y = ~high_SSB, 
#         type = "scatter", 
#         mode = "lines",
#         line = list(color = "transparent", shape = "linear"),
#         showlegend = FALSE, 
#         name = "high_SSB"
#     )
#     fig4 <- fig4 %>% add_trace(
#         data = data, 
#          x = ~years,
#         y = ~low_SSB, 
#         type = "scatter", 
#         mode = "lines",
#         fill = "tonexty", 
#         fillcolor = "rgba(0,100,80,0.2)",
#         line = list(color = "transparent", shape = "linear"),
#         showlegend = FALSE, 
#         name = "low_SSB"
#     )
#     fig4 <- fig4 %>% add_trace(
#         data = data, 
#         x = ~years, 
#         y = ~SSB, 
#         type = "scatter", 
#         mode = "lines+markers",
#         line = list(color = "rgb(0,100,80)", shape = "linear"), 
#         name = "SSB",
#         marker = list(size = 10), 
#         showlegend = TRUE
#     )

#     ## Add horizontal lines
#     fig4 <- fig4 %>% add_trace(
#         data = data, 
#         x = ~years, 
#         y = ~Blim, 
#         name = "Blim", 
#         type = "scatter", 
#         mode = "lines",
#         line = list(color = "black", shape = "linear", dash = "dash"), 
#         showlegend = TRUE
#     )
#     fig4 <- fig4 %>% add_trace(
#         data = data, 
#         x = ~years, 
#         y = ~Bpa, 
#         name = "Bpa", 
#         type = "scatter", 
#         mode = "lines",
#         line = list(color = "black", shape = "linear", dash = "dot"), 
#         showlegend = TRUE
#     )
#     fig4 <- fig4 %>% add_trace(
#         data = data, 
#         x = ~years, 
#         y = ~MSYBtrigger, 
#         name = "MSYBtrigger", 
#         type = "scatter", 
#         mode = "lines",
#         line = list(color = "orange", shape = "linear", dash = "dash"), 
#         showlegend = TRUE
#     )

#     fig4 <- fig4 %>% layout(
#         title = "SSB", 
#         legend = legend_format(),
#         paper_bgcolor = "rgb(255,255,255)", 
#         plot_bgcolor = "rgb(229,229,229)",
#         xaxis = list(
#             title = "Years",
#             gridcolor = "rgb(255,255,255)",
#             showgrid = TRUE,
#             showline = TRUE,
#             showticklabels = TRUE,
#             tickcolor = "rgb(127,127,127)",
#             ticks = "outside",
#             zeroline = TRUE,
#             titlefont = titlefont_format(),
#             tickfont = tickfont_format()
#         ),
#         yaxis = list(
#             title = "SSB",
#             gridcolor = "rgb(255,255,255)",
#             showgrid = TRUE,
#             showline = TRUE,
#             showticklabels = TRUE,
#             tickcolor = "rgb(127,127,127)",
#             ticks = "outside",
#             zeroline = TRUE,
#             titlefont = titlefont_format(),
#             tickfont = tickfont_format()
#         )
#     )


#     fig4
# }

#figure_4_SSB(SSB, SSB$Year, SSB$low_SSB, SSB$SSB, SSB$high_SSB, SSB$Blim, SSB$Bpa, SSB$MSYBtrigger)


#' Returns ....
#'
#' Downloads ...
#'
#' @param stock_name
#'
#' @return 
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' 
#' }
#'
#' @references
#'
#' 
#'
#' @export
#' 
#####################Subplots quality of assessment
# quality_assessment_plots <- function(big_data, big_data_last_year,
#                                         stockSizeDescription,stockSizeUnits,
#                                         Fage, fishingPressureDescription,
#                                         RecruitmentAge) {

# ## Labels for axes and annotation for the plots, taken from SAG
# SSB_yaxis_label <- sprintf("%s (%s)", dplyr::last(stockSizeDescription), dplyr::last(stockSizeUnits))
# F_yaxis_label <- sprintf("%s <sub>(ages %s)</sub>",dplyr::last(fishingPressureDescription), dplyr::last(Fage))
# R_yaxis_label <- sprintf("Recruitment <sub>(age %s)</sub>", dplyr::last(RecruitmentAge))

# # print(length(unique(big_data$AssessmentYear)))
# # print(length(unique(big_data_last_year$AssessmentYear)))
# palette_bw <- c( "#969696", "#737373", "#525252", "#252525","#de2d26") #"#bdbdbd",
#  fig1 <- plot_ly(
#      data = big_data,
#      x = ~Year,
#      y = ~SSB,
#      split = ~AssessmentYear,
#      type = "scatter",
#      mode = "lines+markers",
#     #  line = list(shape = "spline"),
#      connectgaps = FALSE,
#      color = ~AssessmentYear,
#      colors = palette_bw,
#      legendgroup = "A"
#  )
#  fig1 <- fig1 %>% add_trace(
#         data = big_data_last_year, ###select for last reference points last year
#         x = ~Year, 
#         y = ~Blim, 
#         name = "Blim", 
#         type = "scatter", 
#         mode = "lines",
#         line = list(color = "black", shape = "linear", dash = "dash", width = 2), 
#         showlegend = TRUE,
#         legendgroup = "B"
#     )
#     fig1 <- fig1 %>% add_trace(
#         data = big_data_last_year, 
#         x = ~Year, 
#         y = ~Bpa, 
#         name = "Bpa", 
#         type = "scatter", 
#         mode = "lines",
#         line = list(color = "black", shape = "linear", dash = "dot", width = 2), 
#         showlegend = TRUE,
#         legendgroup = "B"
#     )

#     fig1 <- fig1 %>% add_trace(
#         data = big_data_last_year, 
#         x = ~Year, 
#         y = ~MSYBtrigger, 
#         name = "MSYBtrigger", 
#         type = "scatter", 
#         mode = "lines",
#         line = list(color = "#689dff", shape = "linear", width = 1), 
#         showlegend = TRUE,
#         legendgroup = "B"
#     )
#  fig1 <- fig1 %>% layout(
#         # title = "SSB", 
#         legend = legend_format(),
#         paper_bgcolor = "rgb(246,250,251)", 
#         plot_bgcolor = "rgb(255,255,255)",
#         # images = watermark(),

#         xaxis = list(
#             title = "Years",
#             gridcolor = "rgb(235,235,235)",
#             showgrid = TRUE,
#             showline = TRUE,
#             showticklabels = TRUE,
#             tickcolor = "rgb(127,127,127)",
#             ticks = "outside",
#             zeroline = TRUE,
#             titlefont = titlefont_format(),
#             tickfont = tickfont_format()
#         ),
#         yaxis = list(
#             title = SSB_yaxis_label,
#             gridcolor = "rgb(235,235,235)",
#             showgrid = TRUE,
#             showline = TRUE,
#             showticklabels = TRUE,
#             tickcolor = "rgb(127,127,127)",
#             ticks = "outside",
#             zeroline = TRUE,
#             titlefont = titlefont_format(),
#             tickfont = tickfont_format()
#         )
#     )

#  fig2 <- plot_ly(
#      data = big_data,
#      x = ~Year,
#      y = ~F,
#      split = ~AssessmentYear,
#      type = "scatter",
#      mode = "lines+markers",
#     #  line = list(shape = "spline"),
#      connectgaps = FALSE,
#      color = ~AssessmentYear,
#      colors = palette_bw,
#      showlegend = FALSE,
#      legendgroup = "A"
#  )
#  ## Add horizontal lines
#     fig2 <- fig2 %>% add_trace(
#         data = big_data_last_year, 
#         x = ~Year, 
#         y = ~FLim, 
#         name = "FLim", 
#         type = "scatter", 
#         mode = "lines",
#         line = list(color = "#a1a1a1", shape = "linear", dash = "dash", width = 2), 
#         showlegend = TRUE,
#         legendgroup = "B"
#     )
#     fig2 <- fig2 %>% add_trace(
#         data = big_data_last_year, 
#         x = ~Year, 
#         y = ~Fpa, 
#         name = "Fpa", 
#         type = "scatter", 
#         mode = "lines",
#         line = list(color = "#a1a1a1", shape = "linear", dash = "dot", width = 2), 
#         showlegend = TRUE,
#         legendgroup = "B"
#     )

#     fig2 <- fig2 %>% add_trace(
#         data = big_data_last_year, 
#         x = ~Year, 
#         y = ~FMSY, 
#         name = "FMSY", 
#         type = "scatter", 
#         mode = "lines",
#         line = list(color = "#00AC67", shape = "linear", width = 1), 
#         showlegend = TRUE,
#         legendgroup = "B"
#     )
# fig2 <- fig2 %>% layout(
#         # title = "F", 
#         legend = legend_format(),
#         paper_bgcolor = "rgb(246,250,251)", 
#         plot_bgcolor = "rgb(255,255,255)",
#         # images = watermark(),

#         xaxis = list(
#             title = "Years",
#             gridcolor = "rgb(235,235,235)",
#             showgrid = TRUE,
#             showline = TRUE,
#             showticklabels = TRUE,
#             tickcolor = "rgb(127,127,127)",
#             ticks = "outside",
#             zeroline = TRUE,
#             titlefont = titlefont_format(),
#             tickfont = tickfont_format()
#         ),
#         yaxis = list(
#             title = F_yaxis_label,
#             gridcolor = "rgb(235,235,235)",
#             showgrid = TRUE,
#             showline = TRUE,
#             showticklabels = TRUE,
#             tickcolor = "rgb(127,127,127)",
#             ticks = "outside",
#             zeroline = TRUE,
#             rangemode = "tozero",
#             titlefont = titlefont_format(),
#             tickfont = tickfont_format()
#         )
#     )

#  fig3 <- plot_ly(
#      data = big_data,
#      x = ~Year,
#      y = ~recruitment,
#      split = ~AssessmentYear,
#      type = "scatter",
#      mode = "lines+markers",
#     #  line = list(shape = "spline"),
#      connectgaps = FALSE,
#      color = ~AssessmentYear,
#      colors = palette_bw,
#      showlegend = FALSE,
#      legendgroup = "A"
#  )
# fig3 <- fig3 %>% layout(
#         # title = "R", 
#         legend = legend_format(),
#         paper_bgcolor = "rgb(246,250,251)", 
#         plot_bgcolor = "rgb(255,255,255)",
#         # images = watermark(),


#         xaxis = list(
#             title = "Years",
#             gridcolor = "rgb(235,235,235)",
#             showgrid = TRUE,
#             showline = TRUE,
#             showticklabels = TRUE,
#             tickcolor = "rgb(127,127,127)",
#             ticks = "outside",
#             zeroline = TRUE,
#             titlefont = titlefont_format(),
#             tickfont = tickfont_format()
#         ),
#         yaxis = list(
#             title = R_yaxis_label,
#             gridcolor = "rgb(235,235,235)",
#             showgrid = TRUE,
#             showline = TRUE,
#             showticklabels = TRUE,
#             tickcolor = "rgb(127,127,127)",
#             ticks = "outside",
#             zeroline = TRUE,
#             titlefont = titlefont_format(),
#             tickfont = tickfont_format()
#         )
#     )

#  fig_qass <- subplot(fig1, fig2, fig3, 
#  nrows = 3, shareX = TRUE, titleX = TRUE, titleY = TRUE,  heights = c(0.33, 0.33, 0.33), margin = c(0.05,0.05,0.01,0.01))#, ,
# #  nrows = 1,
# #    widths = NULL,
# #    heights = NULL,
# #    margin = 0.02,
# #    shareX = TRUE,
# #    shareY = FALSE,
# #    titleX = shareX)
# #    titleY = shareY,
# #    which_layout = "merge")
#  fig_qass
# }

# quality_assessment_plots(big_data, big_data_last_year)


#' Returns ....
#'
#' Downloads ...
#'
#' @param stock_name
#'
#' @return 
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' 
#' }
#'
#' @references
#'
#' 
#'
#' @export
#' 
# figure_1_plots <- function(data1, data2, data3, data4, 
#                             years, 
#                             catches, landings, discards, units, stock_name, AssessmentYear,
#                             recruitment, low_recruitment, high_recruitment, recruitment_age, 
#                             low_F, F, high_F, FLim, Fpa, FMSY,Fage, fishingPressureDescription,
#                             low_SSB, SSB, high_SSB, Blim, Bpa, MSYBtrigger, stockSizeDescription, stockSizeUnits) {
#     if (all(is.na(data1[, "landings"]))) {
#         data1$landings <- data1$catches
#     }

#     ## Labels for axes and annotation for the plots, taken from SAG
#     catches_yaxis_label <- sprintf("Catches (%s)", dplyr::last(units))
#     R_yaxis_label <- sprintf("Recruitment <sub>(age %s)</sub>", dplyr::last(recruitment_age))
#     F_yaxis_label <- sprintf("%s <sub>(ages %s)</sub>",dplyr::last(fishingPressureDescription), dplyr::last(Fage))
#     SSB_yaxis_label<- sprintf("%s (%s)", dplyr::last(stockSizeDescription), dplyr::last(stockSizeUnits))
    
#     Stockcode_year_annotation_1 <- list( showarrow = FALSE,
#                                         text = sprintf("%s, %s", dplyr::last(stock_name), dplyr::last(AssessmentYear)),
#                                         font = list(family = "Calibri, serif",size = 10, color = "black"),
#                                         yref = 'paper', y = 1, xref = "paper", x = 0.8
#                                         )
#     Stockcode_year_annotation_2 <- list( showarrow = FALSE,
#                                         text = sprintf("%s, %s", dplyr::last(stock_name), dplyr::last(AssessmentYear)),
#                                         font = list(family = "Calibri, serif",size = 10, color = "black"),
#                                         yref = 'paper', y = 1, xref = "paper", x = 0.95
#                                         )
#     Stockcode_year_annotation_3 <- list( showarrow = FALSE,
#                                             text = sprintf("%s, %s", dplyr::last(stock_name), dplyr::last(AssessmentYear)),
#                                             font = list(family = "Calibri, serif",size = 10, color = "black"),
#                                             yref = 'paper', y = 0.97, xref = "paper", x = 0.8
#                                             )
#     Stockcode_year_annotation_4 <- list( showarrow = FALSE,
#                                         text = sprintf("%s, %s", dplyr::last(stock_name), dplyr::last(AssessmentYear)),
#                                         font = list(family = "Calibri, serif",size = 10, color = "black"),
#                                         yref = 'paper', y = 0.97, xref = "paper", x = 0.95
#                                         )

    

#     # Start the plot
#     fig1 <- plot_ly(
#         data = data1,
#         x = ~years,
#         y = ~landings,
#         name = "Landings",
#         type = "bar",
#         hoverinfo = "text",
#         text = ~ paste("Year:", Year, "<br>Landings:", landings),
#         marker = list(
#             color = "#002b5f", # BMSlandings #047c6c
#             line = list(
#                 color = "#d0d1d6",
#                 width = 0.5
#             )
#         ),
#         showlegend = TRUE,
#         legendgroup = "A"
#     )

#     fig1 <- fig1 %>% add_trace(
#         data = data1,
#         x = ~years,
#         y = ~discards,
#         name = "Discards",
#         type = "bar",
#         hoverinfo = "text",
#         text = ~ paste("Year:", Year, "<br>Discards:", discards),
#         marker = list(
#             color = "#fda500",
#             line = list(
#                 color = "#d0d1d6",
#                 width = 0.5
#             )
#         ),
#         showlegend = TRUE,
#         legendgroup = "A"
#     )

#     fig1 <- fig1 %>% layout(
#         # title = "Catches",
#         paper_bgcolor = "rgb(246,250,251)",
#         plot_bgcolor = "rgb(255,255,255)",
#         # images = watermark(),

#         xaxis = list(
#             title = "Years",
#             gridcolor = "rgb(235,235,235)",
#             showgrid = TRUE,
#             showline = TRUE,
#             tickcolor = "rgb(127,127,127)",
#             titlefont = titlefont_format(),
#             tickfont = tickfont_format(),
#             showticklabels = TRUE
#         ),
#         barmode = "stack",
#         legend = legend_format(),
#         yaxis = list(
#             title = catches_yaxis_label,#"Catches",
#             gridcolor = "rgb(235,235,235)",
#             showgrid = TRUE,
#             showline = TRUE,
#             tickcolor = "rgb(127,127,127)",
#             titlefont = titlefont_format(),
#             tickfont = tickfont_format(),
#             showticklabels = TRUE
#         ),
#         annotations = list(Stockcode_year_annotation_1)
#     )

#     fig2 <- plot_ly(
#         data = data2,
#         x = ~years,
#         y = ~recruitment,
#         name = "Recruitment",
#         type = "bar",
#         hoverinfo = "text",
#         text = ~ paste("Year:", years, "<br>Recruitment:", recruitment),
#         marker = list(
#             color = "#28b3e8", #last yesr #92defb
#             line = list(
#                 color = "#d0d1d6",
#                 width = 0.5
#             )
#         ),
#         error_y = list(
#             type = "data",
#             symmetric = FALSE,
#             arrayminus = ~low_recruitment,
#             array = ~high_recruitment,
#             color = "rgba(169,169,169,0.5)"
#         ),
#         legendgroup = "A"
#     )

#     fig2 <- fig2 %>% layout(
#         # title = "Recruitment",
#         paper_bgcolor = "rgb(246,250,251)",
#         plot_bgcolor = "rgb(255,255,255)",
#         # images = watermark(),

#         xaxis = list(
#             title = "Years",
#             gridcolor = "rgb(235,235,235)",
#             showgrid = TRUE,
#             showline = TRUE,
#             tickcolor = "rgb(127,127,127)",
#             titlefont = titlefont_format(),
#             tickfont = tickfont_format(),
#             showticklabels = TRUE
#         ),
#         yaxis = list(
#             title = R_yaxis_label,#"Recruitment",
#             gridcolor = "rgb(235,235,235)",
#             showgrid = TRUE,
#             showline = TRUE,
#             tickcolor = "rgb(127,127,127)",
#             titlefont = titlefont_format(),
#             tickfont = tickfont_format(),
#             showticklabels = TRUE
#         ),
#         annotations = list(Stockcode_year_annotation_2)
#     )

#     fig3 <- plot_ly(
#         data = data3,
#         x = ~years,
#         y = ~high_F,
#         type = "scatter",
#         mode = "lines",
#         line = list(color = "transparent", shape = "linear"), #
#         showlegend = FALSE,
#         name = "DATA",
#         legendgroup = "A"
#     )
#     fig3 <- fig3 %>% add_trace(
#         data = data3,
#         y = ~low_F,
#         type = "scatter",
#         mode = "lines",
#         fill = "tonexty",
#         name = "95 %",
#         fillcolor = "#f2a497", # "rgba(0,100,80,0.2)"rgba(255,71,26,0.2)
#         line = list(color = "transparent", shape = "linear"),
#         showlegend = TRUE,
#         name = "low_F",
#         legendgroup = "A"
#     )
#     fig3 <- fig3 %>% add_trace(
#         data = data3,
#         x = ~years,
#         y = ~F,
#         type = "scatter",
#         mode = "lines+markers",
#         line = list(color = "#ed5f26", shape = "linear"), #"rgb(255,71,26)"
#         name = "F",
#         marker = list(size = 1, color = "#ed5f26"),
#         showlegend = TRUE,
#         legendgroup = "A"
#     )

#     ## Add horizontal lines
#     fig3 <- fig3 %>% add_trace(
#         data = data3,
#         x = ~years,
#         y = ~FLim,
#         name = "FLim",
#         type = "scatter",
#         mode = "lines",
#         line = list(color = "#a1a1a1", shape = "linear", dash = "dash", width = 2), #black
#         showlegend = TRUE,
#         legendgroup = "B"
#     )
#     fig3 <- fig3 %>% add_trace(
#         data = data3,
#         x = ~years,
#         y = ~Fpa,
#         name = "Fpa",
#         type = "scatter",
#         mode = "lines",
#         line = list(color = "#a1a1a1", shape = "linear", dash = "dot", width = 2), #7e7e7e
#         showlegend = TRUE,
#         legendgroup = "B"
#     )
#     fig3 <- fig3 %>% add_trace(
#         data = data3,
#         x = ~years,
#         y = ~FMSY,
#         name = "FMSY",
#         type = "scatter",
#         mode = "lines",
#         line = list(color = "#00AC67", shape = "linear", width = 1),#, dash = "dash"), #679dfe old blue like the one MSYBtrigger  af1111
#         showlegend = TRUE,
#         legendgroup = "B"
#     )

#     fig3 <- fig3 %>% layout(
#         # title = "F",
#         legend = legend_format(),
#         paper_bgcolor = "rgb(246,250,251)",
#         plot_bgcolor = "rgb(255,255,255)",
#         # images = watermark(),

#         xaxis = list(
#             title = "Years",
#             gridcolor = "rgb(235,235,235)",
#             showgrid = TRUE,
#             showline = TRUE,
#             showticklabels = TRUE,
#             tickcolor = "rgb(127,127,127)",
#             ticks = "outside",
#             zeroline = TRUE,
#             titlefont = titlefont_format(),
#             tickfont = tickfont_format()
#         ),
#         yaxis = list(
#             title = F_yaxis_label, #"F",
#             gridcolor = "rgb(235,235,235)",
#             showgrid = TRUE,
#             showline = TRUE,
#             showticklabels = TRUE,
#             tickcolor = "rgb(127,127,127)",
#             ticks = "outside",
#             zeroline = TRUE,
#             rangemode = "tozero",
#             titlefont = titlefont_format(),
#             tickfont = tickfont_format()
#         ),
#         annotations = list(Stockcode_year_annotation_3)
#     )
#     fig4 <- plot_ly(
#         data = data4,
#         x = ~years,
#         y = ~high_SSB,
#         type = "scatter",
#         mode = "lines",
#         line = list(color = "transparent", shape = "linear"),
#         showlegend = FALSE,
#         name = "high_SSB"
#     )
#     fig4 <- fig4 %>% add_trace(
#         data = data4,
#         x = ~years,
#         y = ~low_SSB,
#         type = "scatter",
#         mode = "lines",
#         fill = "tonexty",
#         name = "95 %",
#         fillcolor = "#94b0a9", #rgba(0,100,80,0.2)
#         line = list(color = "transparent", shape = "linear"),
#         showlegend = TRUE,
#         name = "low_SSB",
#         legendgroup = "A"
#     )
#     fig4 <- fig4 %>% add_trace(
#         data = data4,
#         x = ~years,
#         y = ~SSB,
#         type = "scatter",
#         mode = "lines+markers",
#         line = list(color = "#047c6c", shape = "linear"), #rgb(0,100,80)
#         name = "SSB",
#         marker = list(size = 1, color = "#047c6c"),
#         showlegend = TRUE,
#         legendgroup = "A"
#     )

#     ## Add horizontal lines
#     fig4 <- fig4 %>% add_trace(
#         data = data4,
#         x = ~years,
#         y = ~Blim,
#         name = "Blim",
#         type = "scatter",
#         mode = "lines",
#         line = list(color = "black", shape = "linear", dash = "dash", width = 2),
#         showlegend = TRUE,
#         legendgroup = "B"
#     )
#     fig4 <- fig4 %>% add_trace(
#         data = data4,
#         x = ~years,
#         y = ~Bpa,
#         name = "Bpa",
#         type = "scatter",
#         mode = "lines",
#         line = list(color = "black", shape = "linear", dash = "dot", width = 2),
#         showlegend = TRUE,
#         legendgroup = "B"
#     )
#     fig4 <- fig4 %>% add_trace(
#         data = data4,
#         x = ~years,
#         y = ~MSYBtrigger,
#         name = "MSYBtrigger",
#         type = "scatter",
#         mode = "lines",
#         line = list(color = "#689dff", shape = "linear", width = 1),#, dash = "dash"), 
#         showlegend = TRUE,
#         legendgroup = "B"
#     )

#     fig4 <- fig4 %>% layout(
#         # title = "SSB",
#         legend = legend_format(),
#         paper_bgcolor = "rgb(246,250,251)",
#         plot_bgcolor = "rgb(255,255,255)",
#         # images = watermark(),

#         xaxis = list(
#             title = "Years",
#             gridcolor = "rgb(235,235,235)",
#             showgrid = TRUE,
#             showline = TRUE,
#             showticklabels = TRUE,
#             tickcolor = "rgb(127,127,127)",
#             ticks = "outside",
#             zeroline = TRUE,
#             titlefont = titlefont_format(),
#             tickfont = tickfont_format()
#         ),
#         yaxis = list(
#             title = SSB_yaxis_label,#"SSB",
#             gridcolor = "rgb(235,235,235)",
#             showgrid = TRUE,
#             showline = TRUE,
#             showticklabels = TRUE,
#             tickcolor = "rgb(127,127,127)",
#             ticks = "outside",
#             zeroline = TRUE,
#             titlefont = titlefont_format(),
#             tickfont = tickfont_format()
#         ),
#         annotations = list(Stockcode_year_annotation_4)
#     )

#     fig <- subplot(fig1, fig2, fig3, fig4,
#         nrows = 2, shareX = TRUE, titleX = TRUE, titleY = TRUE, widths = c(0.5, 0.5), heights = c(0.5, 0.5), margin = c(0.06,0.06,0.02,0.02)
#     ) #

#     # RefPoints_annotation <- list( showarrow = FALSE,
#     #                                     text = "Reference points",
#     #                                     font = list(family = "Calibri, serif",size = 20, color = "black"),
#     #                                     yref = 'paper', y =0.6, xref = "paper", x = 1.2
#     #                                   )

#     # fig <- fig %>%layout(annotations =  RefPoints_annotation)                                 
#     fig
# }



#' Returns ....
#'
#' Downloads ...
#'
#' @param stock_name
#'
#' @return 
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' 
#' }
#'
#' @references
#'
#' 
#'
#' @export
#' 
####### plots 1 catch scenarios
radial_plot <- function(tmp, catch_scenarios) {
    
    not_all_na <- function(x) any(!is.na(x))
    tmp <- tmp %>% select(where(not_all_na))
    
    rescale_function <- function(x) rescale(x, to = c(0, 1), from = range(c(min(x), max(x))))
    tmp <- tmp %>% select(-c(Year)) %>% na.omit() %>% mutate_if(is.numeric, rescale_function)

    zz <- ggplotly(
        ggradar(tmp %>% select(-cS_Purpose) %>% filter(cat %in% catch_scenarios),
            base.size = 8,
            font.radar = "sans",
            values.radar = c("-100%", "0%","100%"),
            # axis.labels = colnames(catch_tab_stand_scaled)[-1],
            # grid.min = 0,
            # grid.mid = 0.5,
            # grid.max = 1,
            # centre.y = grid.min - ((1 / 9) * (grid.max - grid.min)),
            # plot.extent.x.sf = 1,
            # plot.extent.y.sf = 1.2,
            # x.centre.range = 0.02 * (grid.max - centre.y),
            # label.centre.y = FALSE,
            # grid.line.width = 0.5,
            gridline.min.linetype = "longdash",
            gridline.mid.linetype = "longdash",
            gridline.max.linetype = "longdash",
            gridline.min.colour = "grey",
            gridline.mid.colour = "#007A87",
            gridline.max.colour = "grey",
            grid.label.size = 6,
            # gridline.label.offset = -0.1 * (grid.max - centre.y),
            label.gridline.min = TRUE,
            label.gridline.mid = TRUE,
            label.gridline.max = TRUE,
            axis.label.offset = 1.20,
            axis.label.size = 8,
            axis.line.colour = "grey",
            group.line.width = 1.5,
            group.point.size = 6,
            group.colours = NULL,
            background.circle.colour = "#D7D6D1",
            background.circle.transparency = 0.2,
            plot.legend = TRUE, # if (nrow(catch_tab_stand_scaled) > 1) TRUE else FALSE,
            legend.title = "Scenarios:",
            plot.title = "",
            legend.text.size = 8,
            legend.position = "right"
        )#,
        # height = 600, width=600
    )
    # zz <- zz %>% layout(autosize = T, margin = list(l = 0, r = 100, b = 0, t = 0, pad = 4))

    zz
    
}

# catch_scenarios_plot1(catch_scen_table_perc)

# catch_scenarios_plot2 <- function(tmp) {
#     tmp$Year <- 2022

#     tmp2 <- tmp %>% select(Year, cS_Label, `Ftotal (2020)`, `SSB (2021)`, `Total catch (2020)`, `% TAC change (2020)`, `% Advice change (2020)`, `% SSB change (2021)`)

#     colnames(tmp2) <- c("Year", "cat", "F", "SSB", "TotCatch", "TACchange", "ADVICEchange", "SSBchange")
#     tmp2 <- tmp2 %>% do(bind_rows(., data.frame(Year = 2022, cat = "ref", F = 0, SSB = 0, TotCatch = 0, TACchange = 0, ADVICEchange = 0, SSBchange = 0)))

#     sc <- head(tmp2$cat)


#     fig_F <- plot_ly(arrange(tmp2, F),
#         x = ~TotCatch, y = ~F, mode = "lines+markers", #text = ~cat,
#         marker = list(size = 20)
#     )
#     fig_F <- fig_F %>% add_annotations(
#         x = ~TotCatch, y = ~F,
#         text = ~cat,
#         textfont = list(color = "#000000", size = 30),
#         xref = "x",
#         yref = "y",
#         showarrow = TRUE,
#         arrowhead = 4,
#         arrowsize = .5,
#         ax = c(20, -20),
#         ay = c(-80, 40, 80)
#     )
#      fig_F <- fig_F %>% layout(
#         xaxis = list(
#              title = "Total Catch"),
#         yaxis= list(
#              title = "F")
#      )



#     fig_SSB <- plot_ly(arrange(tmp2, F),
#         x = ~TotCatch, y = ~SSB, mode = "lines+markers", #text = ~cat,
#         marker = list(size = 20)
#     )
#     fig_SSB <- fig_SSB %>% add_annotations(
#         x = ~TotCatch, y = ~SSB,
#         text = ~cat,
#         textfont = list(color = "#000000", size = 30),
#         xref = "x",
#         yref = "y",
#         showarrow = TRUE,
#         arrowhead = 4,
#         arrowsize = .5,
#         ax = c(20, -20),
#         ay = c(-80, 40, 80)
#     )
#     fig_SSB <- fig_SSB %>% layout(
#         xaxis = list(
#              title = "Total Catch"),
#         yaxis= list(
#              title = "SSB")
#      )

#     fig <- subplot(fig_F, fig_SSB,
#         nrows = 1, shareX = TRUE, titleX = TRUE, titleY = TRUE#, heights = c(1, 1)
#     ) # widths = c(0.5, 0.5), heights = c(0.5, 0.5), margin = c(0.06,0.06,0.02,0.02)
#     # ))

#     fig
# }

#' Returns ....
#'
#' Downloads ...
#'
#' @param stock_name
#'
#' @return 
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' 
#' }
#'
#' @references
#'
#' 
#'
#' @export
#' 
# catch_scenarios_plot2 <- function(tmp, Fage, fishingPressureDescription, stockSizeDescription, stockSizeUnits, units) {
catch_scenarios_plot2 <- function(tmp, df) {   
    F_yaxis_label <- sprintf("%s <sub>(ages %s)</sub>",dplyr::last(df$fishingPressureDescription), dplyr::last(df$Fage))
    SSB_yaxis_label<- sprintf("%s (%s)", dplyr::last(df$stockSizeDescription), dplyr::last(df$stockSizeUnits))
    catches_yaxis_label <- sprintf("Catches (%s)", dplyr::last(df$units))
    # sc <- head(tmp2$cat)
    tmp <- arrange(tmp, F)

    labels <- sprintf(
            "Catch Scenario: %s", tmp$cat
        ) %>% lapply(htmltools::HTML)
    
    # F0 <- tmp[tmp$cat == "F = 0", ] taking this out because spmetimes F0 is not present
    Basis <- tmp[tmp$cS_Purpose == "Basis Of Advice",]

    fig_catch <- plot_ly(tmp, source = "ranking") %>%  
        add_trace(
            x = ~ TotCatch,
            y = ~ F,
            type = "scatter",
            mode = "lines+markers",
            text = labels,
            marker = list(size = 15),
            name = "F"
        )
    ay <- list(
        # tickfont = list(color = "#000000", size = 20),
        overlaying = "y",
        side = "right",
        title = SSB_yaxis_label,
        # titlefont = list(color = "#ff7300", size = 30),
        titlefont = titlefont_format(),
        tickfont = tickfont_format()
    )
    fig_catch <- fig_catch %>% add_trace(
        x = ~ TotCatch,
        y = ~ SSB,
        type = "scatter",
        mode = "lines+markers",
        text = labels,
        marker = list(size = 15, color = "#ff7300"),
        name = "SSB",
        yaxis = "y2"
    )
    
    # a <- list(
    #     x = F0$TotCatch,
    #     y = F0$F,
    #     text = F0$cat,
    #     xref = "x",
    #     yref = "y",
    #     showarrow = TRUE,
    #     arrowhead = 15,
    #     ax = 10,
    #     ay = -100,
    #     font = list(
    #         color = "#000000",
    #         family = "sans serif",
    #         size = 25
    #     )
    # )
    b <- list(
        x = Basis$TotCatch,
        y = Basis$F,
        text = Basis$cS_Purpose,
        xref = "x",
        yref = "y",
        showarrow = TRUE,
        arrowcolor = "#999999",
        arrowhead = 15,
        ax = 7,
        ay = -50, font = list(
            color = "#999999",
            family = "sans serif",
            size = 20
        )
    )

    # c <- list(
    #   x = F0$TotCatch,
    #   y = F0$SSB,
    #   text = F0$cat,
    #   xref = "x",
    #   yref = "y",
    #   showarrow = TRUE,
    #   arrowhead = 15,
    #   ax = 200,
    #   ay = 50,
    #   font = list(color = '#000000',
    #                               family = 'sans serif',
    #                               size = 30)
    # )
    # d <- list(
    #   x = Basis$TotCatch,
    #   y = Basis$SSB,
    #   text = Basis$cat,
    #   xref = "x",
    #   yref = "y",
    #   showarrow = TRUE,
    #   arrowhead = 15,
    #   ax = 200,
    #   ay = 50,
    #   font = list(color = '#000000',
    #                               family = 'sans serif',
    #                               size = 30)
    # )

    fig_catch <- fig_catch %>% layout(
        # title = "Catches",
        paper_bgcolor = "rgb(255,255,255)",
        plot_bgcolor = "rgb(255,255,255)",
        hovermode = "x",
        yaxis2 = ay,
        annotations = b,
        legend = list(
            font = list(size = 20, 
            color = "black"),
            bgcolor = "rgba(255,255,255, 0.2)",
            x = 0.1,
            y = 0.5
        ),
        autosize = T,
        margin = list(l = 120, r = 120, b = 120, t = 50, pad = 8),
        xaxis = list(
            title = catches_yaxis_label,
            gridcolor = "rgb(235,235,235)",
            showgrid = TRUE,
            showline = TRUE,
            tickcolor = "rgb(127,127,127)",
            titlefont = titlefont_format(),
            tickfont = tickfont_format(),
            showticklabels = TRUE
        ),
        yaxis = list(
            title = F_yaxis_label, # "SSB",
            gridcolor = "rgb(235,235,235)",
            showgrid = TRUE,
            showline = TRUE,
            showticklabels = TRUE,
            tickcolor = "rgb(127,127,127)",
            ticks = "outside",
            zeroline = TRUE,
            rangemode = "tozero",
            titlefont = titlefont_format(),
            tickfont = tickfont_format()
        )
    )

    
}

#' Returns ....
#'
#' Downloads ...
#'
#' @param stock_name
#'
#' @return 
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' 
#' }
#'
#' @references
#'
#' 
#'
#' @export
#' 
TAC_timeline <- function(final_df, catch_scenarios, df) {
    # palette <- brewer.pal(length(unique(final_df$cat)), "Paired")
    # colourCount <- length(unique(catch_scenarios))
    # getPalette <- colorRampPalette(brewer.pal(12, "Paired"))
    # print(array(getPalette(colourCount)))
    catches_yaxis_label <- sprintf("Catches (%s)", dplyr::last(df$units))

    catch_time <- plot_ly(final_df,
        x = ~Year,
        y = ~TotCatch
    ) %>%
        filter(cat %in% catch_scenarios) %>%
        group_by(cat) %>%
        add_trace(
            x = ~Year,
            y = ~TotCatch,
            type = "scatter",
            mode = "lines+markers",
            color = ~cat
            # color = array(getPalette(colourCount))
        )
    # catch_time <- catch_time %>% layout(
    #     xaxis = list(
    #         title = "<b>Years</b>",
    #         titlefont = list(size = 25),
    #         tickfont = list(size = 20)
    #     ),
    
    catch_time <- catch_time %>% layout(
        # title = "Catches",
        paper_bgcolor = "rgb(255,255,255)",
        plot_bgcolor = "rgb(255,255,255)",
        # images = watermark(),
        legend = list(
            orientation = "h",
            y = -.6,
            yanchor = "bottom",
            x = 0.5,
            xanchor = "center",
            title = list(text = "Scenarios")
        ),

        xaxis = list(
            title = "Years",
            gridcolor = "rgb(235,235,235)",
            showgrid = TRUE,
            showline = TRUE,
            tickcolor = "rgb(127,127,127)",
            titlefont = titlefont_format(),
            tickfont = tickfont_format(),
            showticklabels = TRUE
        ),
    # )
        yaxis = list(
            title = catches_yaxis_label, # "SSB",
            gridcolor = "rgb(235,235,235)",
            showgrid = TRUE,
            showline = TRUE,
            showticklabels = TRUE,
            tickcolor = "rgb(127,127,127)",
            ticks = "outside",
            zeroline = TRUE,
            titlefont = titlefont_format(),
            tickfont = tickfont_format()
        )
    )
}

#' Returns ....
#'
#' Downloads ...
#'
#' @param stock_name
#'
#' @return 
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' 
#' }
#'
#' @references
#'
#' 
#'
#' @export
#' 
# get_advice_timeline <- function(stock_code, tbl_sid, tbl_rows_selected) {
#     ## this gets the initial dates from the advice view
#     timeL <- get_Advice_View_info(stock_code)

#     release_date <- timeL[timeL["advice View"] == "adviceReleasedDate", 2]
#     applicable_from <- timeL[timeL["advice View"] == "adviceApplicableFrom", 2]
#     applicable_until <- timeL[timeL["advice View"] == "adviceApplicableUntil", 2]

#     ## This block formats the dates from dd/mm/yyy to Yyyy-mm-dd
#     release_date <- strptime(as.character(release_date), "%d/%m/%Y")
#     release_date <- format(release_date, "%Y-%m-%d")
#     applicable_from <- strptime(as.character(applicable_from), "%d/%m/%Y")
#     applicable_from <- format(applicable_from, "%Y-%m-%d")
#     applicable_until <- strptime(as.character(applicable_until), "%d/%m/%Y")
#     applicable_until <- format(applicable_until, "%Y-%m-%d")


#     ## This block gets the name of the working group from the currently selected row
#     filtered_row <- tbl_sid[tbl_rows_selected, ]
#     WG <- filtered_row$ExpertGroupUrl
#     WG <- str_match(WG, "\\>\\s*(.*?)\\s*\\<\\/a>")[,2]

#     ## This block scrapes the meeting-calendar webpage to find the dates of the upcoming WG meeting
#     page <- read_html(paste0("https://www.ices.dk/news-and-events/meeting-calendar/Pages/ICES-CalendarSearch.aspx?k=", WG))
    
#     start_date <- page %>%
#         html_nodes("td") %>%
#         html_text()

#     title_meeting <- start_date[1]
#     ## This block extracts and formats the dates as above
#     start_WG <- strapplyc(start_date[2], "\\d+/\\d+/\\d+", simplify = TRUE)
#     end_WG <- strapplyc(start_date[3], "\\d+/\\d+/\\d+", simplify = TRUE)
#     start_WG <- strptime(as.character(start_WG), "%d/%m/%Y")
#     start_WG <- format(start_WG, "%Y-%m-%d")
#     end_WG <- strptime(as.character(end_WG), "%d/%m/%Y")
#     end_WG <- format(end_WG, "%Y-%m-%d")

#     ## This blocks create the df that timevis will display
#     data <- data.frame(
#         id      = 1:3,
#         content = c("Advice Release Date", "Advice Applicable Between", title_meeting),
#         start   = c(release_date, applicable_from, start_WG),
#         end     = c(NA, applicable_until, end_WG)
#     )
    

#     return(data)
# }







########################################################## NEW SAG PLOTS ########################################################



##### ICES Theme function
#' Returns ....
#'
#' Downloads ...
#'
#' @param stock_name
#'
#' @return 
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' 
#' }
#'
#' @references
#'
#' 
#'
#' @export
#' 
theme_ICES_plots <- function(type = c("catches", "recruitment", "F", "SSB", "quality_SSB", "quality_F", "quality_R"), df) {
    font <- "Calibri, sans-serif" # assign font family up front

    # scale_color_manual(values = mycolors)
    tmp <- theme_minimal() %+replace% # replace elements we want to change

        theme(
            axis.title = element_text( # axis titles
                family = font, # font family
                size = 20,
                colour = "darkgrey",
                vjust = -2
            ),
            axis.text = element_text( # axis titles
                family = font, # font family
                size = 15,
                colour = "black"
            ),
            axis.title.x = element_blank(),
            panel.grid.major.y = element_line(
                colour = "grey",
                size = 1,
                linetype = "solid",
            ),
            plot.title = element_text( # title
                family = font, # set font family
                size = 23, # set font size
                face = "bold", # bold typeface
                hjust = 0, # left align
                vjust = 1,
                margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
                if (type == "catches") {
                    color <- "#002b5f"
                } else if (type == "recruitment" | type == "quality_R") {
                    color <- "#28b3e8"
                } else if (type == "F" | type == "quality_F") {
                    color <- "#ed5f26"
                } else if (type == "SSB" | type == "quality_SSB") {
                    color <- "#047c6c"
                }
            ), # raise slightly
            # grid elements
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.border = element_rect(
                colour = "black",
                fill = NA,
                size = 0.5
            ),
            # legend
            legend.text = element_text(
                family = "sans-serif",
                size = 15,
                color = "black"
                
            ),
            legend.title = element_blank(),
            legend.position = "bottom"
            
        )

    #   axis.text.x = element_text(            #margin for axis text
    #                 margin=margin(5, b = 10))
    if (type == "catches") {
        # mycolors <- c("#fda500", "#002b5f")
        theme_ICES_plots <- list(
            tmp,
            labs(
                title = "Catches",
                y = sprintf("Catches in 1000 %s", dplyr::last(df$units))
            ),
            scale_fill_manual(values = c(
                "landings" = "#002b5f",
                "discards" = "#fda500"
            )),
            scale_y_continuous(
                expand = expansion(mult = c(0, 0.1)),
                labels = function(l) {
                    trans <- l / 1000
                }
            )
        )
    } else if (type == "recruitment") {
        # mycolors <- c("recruitment" = "#28b3e8")
        theme_ICES_plots <- list(
            tmp,
            labs(
                title = sprintf("Recruitment <sub>(age %s)</sub>", dplyr::last(df$recruitment_age)),
                y = "Recruitment in billions" # sprintf("Catches in 1000 %s", dplyr::last(df$units))
            ),
            scale_fill_manual(values = c("recruitment" = "#28b3e8")),
            scale_y_continuous(
                expand = expansion(mult = c(0, 0.1)),
                labels = function(l) {
                    trans <- l / 1000000
                }
            )
        )
    } else if (type == "F") {
        # mycolors <- c("#ed5f26")#, "#f2a497")
        theme_ICES_plots <- list(
            tmp,
            labs(
                title = "Fishing pressure", # sprintf("Recruitment <sub>(age %s)</sub>", dplyr::last(df$recruitment_age)),
                y = sprintf("%s <sub>(ages %s)</sub>", dplyr::last(df$fishingPressureDescription), dplyr::last(df$Fage)), # sprintf("Catches in 1000 %s", dplyr::last(df$units))
                x = "Year"
            ),
            scale_color_manual(values = c(
                "F" = "#ed5f26",
                "F<sub>MSY</sub>" = "#00AC67",
                "F<sub>Lim</sub>" = "#a1a1a1",
                "F<sub>pa</sub>" = "#a1a1a1"
            )),
            scale_linetype_manual(values = c(
                "F" = "solid",
                "F<sub>Lim</sub>" = "dashed",
                "F<sub>pa</sub>" = "dotted",
                "F<sub>MSY</sub>" = "solid"
            )),
            scale_size_manual(values = c(
                "F" = 1.5,
                "F<sub>Lim</sub>" = .8,
                "F<sub>pa</sub>" = 1,
                "F<sub>MSY</sub>" = .5
            )),
            scale_fill_manual(values = c("#f2a497")),
            expand_limits(y = 0),
            scale_y_continuous(
                expand = expansion(mult = c(0, 0.1)) # ,
                # labels = function(l) {
                #     trans <- l / 1000000
                # }
            )
        )
    } else if (type == "SSB") {
        # mycolors <- c("#ed5f26")#, "#f2a497")
        theme_ICES_plots <- list(
            tmp,
            labs(
                title = "Spawning Stock Biomass", # sprintf("Recruitment <sub>(age %s)</sub>", dplyr::last(df$recruitment_age)),
                y = sprintf("%s in millions %s", dplyr::last(df$stockSizeDescription), dplyr::last(df$stockSizeUnits)),
                x = "Year"
            ),
            scale_color_manual(values = c(
                "SSB" = "#047c6c",
                "MSY B<sub>trigger</sub>" = "#689dff",
                "B<sub>Lim</sub>" = "#a1a1a1",
                "B<sub>pa</sub>" = "#a1a1a1"
            )),
            scale_linetype_manual(values = c(
                "SSB" = "solid",
                "B<sub>Lim</sub>" = "dashed",
                "B<sub>pa</sub>" = "dotted",
                "MSY B<sub>trigger</sub>" = "solid"
            )),
            scale_size_manual(values = c(
                "SSB" = 1.5,
                "B<sub>Lim</sub>" = .8,
                "B<sub>pa</sub>" = 1,
                "MSY B<sub>trigger</sub>" = .5
            )),
            scale_fill_manual(values = c("#94b0a9")),


            # scale_color_manual(values = c("#047c6c")),
            # scale_fill_manual(values = c("#94b0a9")),
            expand_limits(y = 0),
            scale_y_continuous(
                expand = expansion(mult = c(0, 0.1)),
                labels = function(l) {
                    trans <- l / 1000000
                }
            )
        )
    } else if (type == "quality_SSB") {
        theme_ICES_plots <- list(
            tmp,
            labs(
                title = sprintf("%s in 1000 %s", dplyr::last(df$stockSizeDescription), dplyr::last(df$stockSizeUnits)),
                y = "",
                x = ""
            ),
            scale_color_manual(values = c(
                "2021" = "#047c6c",
                "2020" = "#252525",
                "2019" = "#525252",
                "2018" = "#737373",
                "2017" = "#969696",
                "MSY B<sub>trigger</sub>" = "#689dff",
                "B<sub>Lim</sub>" = "#a1a1a1",
                "B<sub>pa</sub>" = "#a1a1a1"
            )),
            scale_linetype_manual(values = c(
                "2021" = "solid",
                "2020" = "solid",
                "2019" = "solid",
                "2018" = "solid",
                "2017" = "solid",
                "B<sub>Lim</sub>" = "dashed",
                "B<sub>pa</sub>" = "dotted",
                "MSY B<sub>trigger</sub>" = "solid"
            )),
            scale_size_manual(values = c(
                "2021" = 1,
                "2020" = 1,
                "2019" = 1,
                "2018" = 1,
                "2017" = 1,
                "B<sub>Lim</sub>" = .8,
                "B<sub>pa</sub>" = 1,
                "MSY B<sub>trigger</sub>" = .5
            )),
            # scale_fill_manual(values = c("#94b0a9")),


            # scale_color_manual(values = c("#047c6c")),
            # scale_fill_manual(values = c("#94b0a9")),
            expand_limits(y = 0),
            scale_y_continuous(
                expand = expansion(mult = c(0, 0.1)),
                labels = function(l) {
                    trans <- l / 1000
                }
            )
        )
    } else if (type == "quality_F") {
        theme_ICES_plots <- list(
            tmp,
            labs(
                title = sprintf("%s <sub>(ages %s)</sub>", dplyr::last(df$fishingPressureDescription), dplyr::last(df$Fage)),
                y = "",
                x = "Year"
            ),
            scale_color_manual(values = c(
                "2021" = "#ed5f26",
                "2020" = "#252525",
                "2019" = "#525252",
                "2018" = "#737373",
                "2017" = "#969696",
                "F<sub>MSY</sub>" = "#00AC67",
                "F<sub>Lim</sub>" = "#a1a1a1",
                "F<sub>pa</sub>" = "#a1a1a1"
            )),
            scale_linetype_manual(values = c(
                "2021" = "solid",
                "2020" = "solid",
                "2019" = "solid",
                "2018" = "solid",
                "2017" = "solid",
                "F<sub>Lim</sub>" = "dashed",
                "F<sub>pa</sub>" = "dotted",
                "F<sub>MSY</sub>" = "solid"
            )),
            scale_size_manual(values = c(
                "2021" = 1,
                "2020" = 1,
                "2019" = 1,
                "2018" = 1,
                "2017" = 1,
                "F<sub>Lim</sub>" = .8,
                "F<sub>pa</sub>" = 1,
                "F<sub>MSY</sub>" = .5
            )),
            # scale_fill_manual(values = c("#94b0a9")),


            # scale_color_manual(values = c("#047c6c")),
            # scale_fill_manual(values = c("#94b0a9")),
            expand_limits(y = 0),
            scale_y_continuous(
                expand = expansion(mult = c(0, 0.1))
                
            )
        )
    } else if (type == "quality_R") {
        theme_ICES_plots <- list(
            tmp,
            labs(
                title = sprintf("Rec <sub>(age %s)</sub> (Billions)", dplyr::last(df$RecruitmentAge)),
                y = "",
                x = ""
            ),
            scale_color_manual(values = c(
                "2021" = "#28b3e8",
                "2020" = "#252525",
                "2019" = "#525252",
                "2018" = "#737373",
                "2017" = "#969696"                
            )),
            scale_linetype_manual(values = c(
                "2021" = "solid",
                "2020" = "solid",
                "2019" = "solid",
                "2018" = "solid",
                "2017" = "solid"                
            )),
            scale_size_manual(values = c(
                "2021" = 1,
                "2020" = 1,
                "2019" = 1,
                "2018" = 1,
                "2017" = 1                
            )),
            # scale_fill_manual(values = c("#94b0a9")),


            # scale_color_manual(values = c("#047c6c")),
            # scale_fill_manual(values = c("#94b0a9")),
            expand_limits(y = 0),
            scale_y_continuous(
                expand = expansion(mult = c(0, 0.1)),
                labels = function(l) {
                    trans <- l / 1000000
                }
            )
        )
    }

    return(theme_ICES_plots)
}


#### SAG stamp
# get_SAG_stamp <- function(df) {
#     # list_stocks <- getListStocks(year)
#     # stock <- list_stocks %>% filter(AssessmentKey == assessmentkey)
#     SAGstamp <- list(
#         showarrow = FALSE,
#         text = tail(df$SAGStamp),
#         font = list(family = "Calibri, serif", size = 10, color = "black"),
#         yref = "paper", y = 1, xref = "paper", x = 1,
#         yanchor = "right", xanchor = "right"
#     )
#     return(SAGstamp)
# }

####### Plots 

ICES_plot_1 <- function(df) {
    p1 <- df %>% filter(Purpose == "Advice") %>%
        select(Year, landings, discards, units, SAGStamp) %>%  
        gather(type, count, discards:landings) %>%
        ggplot(., aes(
            x = Year,
            y = count,
            fill = type,
            text = map(
                paste0(
                    "<b>Year: </b>", Year,
                    "<br>",
                    "<b>", type, ": </b>", count
                ), HTML
            )
        )) +
        geom_bar(position = "stack", stat = "identity") +
        theme_ICES_plots(type = "catches", df) 


    # converting
    fig1 <- ggplotly(p1, tooltip = "text") %>%
        layout(
            legend = list(
                orientation = "h",
                y = -.3,
                yanchor = "bottom",
                x = 0.5,
                xanchor = "center",
                title = list(text = "")
            ),
            annotations = list(
                showarrow = FALSE,
                text = tail(df$SAGStamp,1),
                font = list(family = "Calibri, serif", size = 12, color = "#acacac"),
                yref = "paper", y = 1, xref = "paper", x = 1,
                yanchor = "right", xanchor = "right"
            )
        )
        # print(df %>% filter(Purpose == "Advice"))
    fig1
}
# ICES_plot_1(df, SAGstamp)
######################################recruitment###################################################
ICES_plot_2 <- function(df) {
    p2 <- df %>% filter(Purpose == "Advice") %>%
        select(Year, recruitment, low_recruitment, high_recruitment, recruitment_age, SAGStamp) %>%
        #    gather(type, count, discards:landings) %>%
        ggplot(., aes(
            x = Year,
            y = recruitment,
            fill = "recruitment",
            text = map(
                paste0(
                    "<b>Year: </b>", Year,
                    "<br>",
                    "<b>Recruitment: </b>", recruitment
                ), HTML
            )
        )) +
        geom_bar(stat = "identity") +
        geom_errorbar(aes(
            ymin = low_recruitment,
            ymax = high_recruitment,
            text = map(
                paste0(
                    "<b>Year: </b>", Year,
                    "<br>",
                    "<b>High recruitment: </b>", high_recruitment,
                    "<br>",
                    "<b>Low recruitment: </b>", low_recruitment
                ), HTML
            )
        ), # , color = "2*sd"
        width = .3
        ) +
        theme_ICES_plots(type = "recruitment",df)

    # p2
    # converting
    fig2 <- ggplotly(p2, tooltip = "text") %>%
        layout(
            legend = list(
                orientation = "h",
                y = -.3,
                yanchor = "bottom",
                x = 0.5,
                xanchor = "center",
                title = list(text = "")
            ),
            annotations = list(
                showarrow = FALSE,
                text = tail(df$SAGStamp,1),
                font = list(family = "Calibri, serif", size = 12, color = "#acacac"),
                yref = "paper", y = 1, xref = "paper", x = 1,
                yanchor = "right", xanchor = "right")
        )
    fig2
}
# ICES_plot_2(df, SAGstamp)

ICES_plot_3 <- function(df) {
p3 <- df %>% filter(Purpose == "Advice") %>%
    select(Year, F, low_F, high_F, FLim, Fpa, FMSY, Fage, fishingPressureDescription, SAGStamp) %>%
    drop_na(F) %>%
    #    gather(type, count, discards:landings) %>%
    ggplot(., aes(x = Year, y = F)) +
    # , alpha = 0.2


    geom_ribbon(aes(
        ymin = low_F,
        ymax = high_F,
        fill = "2*sd",
        text = map(
            paste0(
                "<b>Year: </b>", Year,
                "<br>",
                "<b>F: </b>", F,
                "<br>",
                "<b>High F: </b>", high_F,
                "<br>",
                "<b>Low F: </b>", low_F
            ), HTML
        )
    ), linetype = "blank", size = 0) +
    geom_line(aes(
        x = Year,
        y = F,
        color = "F",
        text = map(
            paste0(
                "<b>Year: </b>", Year,
                "<br>",
                "<b>F: </b>", F
            ), HTML
        )
    )) + #, size = 1.5
    geom_line(aes(
        x = Year,
        y = FMSY,
        colour = "F<sub>MSY</sub>",
        linetype = "F<sub>MSY</sub>",
        size = "F<sub>MSY</sub>",
        text = map(
            paste0(
                "<b>F<sub>MSY</sub>: </b>", tail(FMSY, 1)
            ), HTML
        )
    )) +
    geom_line(aes(
        x = Year,
        y = FLim,
        colour = "F<sub>Lim</sub>",
        linetype = "F<sub>Lim</sub>",
        size = "F<sub>Lim</sub>",
        text = map(
            paste0(
                "<b>F<sub>Lim</sub>: </b>", tail(FLim, 1)
            ), HTML
        )
    )) +
    geom_line(aes(
        x = Year,
        y = Fpa,
        colour = "F<sub>pa</sub>",
        linetype = "F<sub>pa</sub>",
        size = "F<sub>pa</sub>",
        text = map(
            paste0(
                "<b>F<sub>pa</sub>: </b>", tail(Fpa, 1)
            ), HTML
        )
    # geom_hline(aes(
    #     yintercept = tail(FMSY, 1),
    #     colour = "FMSY",
    #     linetype = "FMSY",
    #     size = "FMSY",
    #     text = map(
    #         paste0(
    #             "<b>FMSY: </b>", tail(FMSY, 1)
    #         ), HTML
    #     )
    # )) +
    # geom_hline(aes(
    #     yintercept = tail(FLim, 1),
    #     colour = "FLim",
    #     linetype = "FLim",
    #     size = "FLim",
    #     text = map(
    #         paste0(
    #             "<b>FLim: </b>", tail(FLim, 1)
    #         ), HTML
    #     )
    # )) +
    # geom_hline(aes(
    #     yintercept = tail(Fpa, 1),
    #     colour = "Fpa",
    #     linetype = "Fpa",
    #     size = "Fpa",
    #     text = map(
    #         paste0(
    #             "<b>Fpa: </b>", tail(Fpa, 1)
    #         ), HTML
    #     )
    )) +
    theme_ICES_plots(type = "F", df) 
    
   
# plot <- p + text_labels
# plot
# p3
#converting
fig3 <- ggplotly(p3, tooltip = "text") %>%
    layout(
        legend = list(
            orientation = "h",
            itemwidth = 20,
            itemsizing= "trace",
            y = -.3, yanchor = "bottom",
            x = 0.5, xanchor = "center",
            title = list(text = "")
        ),
        xaxis = list(zeroline = TRUE),
        annotations = list(
            showarrow = FALSE,
                text = tail(df$SAGStamp,1),
                font = list(family = "Calibri, serif", size = 12, color = "#acacac"),
                yref = "paper", y = 1, xref = "paper", x = 1,
                yanchor = "right", xanchor = "right")
    ) 
for (i in 1:length(fig3$x$data)) {
    if (!is.null(fig3$x$data[[i]]$name)) {
        fig3$x$data[[i]]$name <- gsub("\\(", "", str_split(fig3$x$data[[i]]$name, ",")[[1]][1])
    }
}
fig3
}
# ICES_plot_3(df, SAGstamp)

ICES_plot_4 <- function(df) {
p4 <- df %>% filter(Purpose == "Advice") %>%
    select(Year, low_SSB, SSB, high_SSB, Blim, Bpa, MSYBtrigger, stockSizeDescription, stockSizeUnits, SAGStamp) %>%
    {if (is.na(.[nrow(.),2:4]) == c(TRUE,FALSE,TRUE)) head(., -1) else .} %>% 
    # fill(c(high_SSB,low_SSB), .direction = "down") %>% 
    # {if(is.na(tail(high_SSB,1))) head(df, -1) else .} %>%
    # drop_na(SSB, high_SSB) %>%
    #    gather(type, count, discards:landings) %>%
    ggplot(., aes(x = Year, y = SSB)) +
    geom_ribbon(aes(
        ymin = low_SSB,
        ymax = high_SSB,
        fill = "2*sd",
        text = map(
            paste0(
                "<b>Year: </b>", Year,
                "<br>",
                "<b>SSB: </b>", SSB,
                "<br>",
                "<b>High SSB: </b>", high_SSB,
                "<br>",
                "<b>Low SSB: </b>", low_SSB
            ), HTML
        )
    ),
    linetype = "blank",
    size = 0
    ) +
    geom_line(aes(
        x = Year,
        y = SSB,
        color = "SSB",
        text = map(
            paste0(
                "<b>Year: </b>", Year,
                "<br>",
                "<b>SSB: </b>", SSB
            ), HTML
        )
    )
    # size = 1.5
    ) +
    geom_line(aes(
        x = Year,
        y = Blim,
        linetype = "B<sub>Lim</sub>",
        colour = "B<sub>Lim</sub>",
        size = "B<sub>Lim</sub>",
        text = map(
            paste0(
                "<b>B<sub>Lim</sub>: </b>", tail(Blim, 1)
            ), HTML
        )
    )) +
    geom_line(aes(
        x = Year,
        y = Bpa,
        linetype = "B<sub>pa</sub>",
        colour = "B<sub>pa</sub>",
        size = "B<sub>pa</sub>",
        text = map(
            paste0(
                "<b>B<sub>pa</sub>: </b>", tail(Bpa, 1)
            ), HTML
        )
    )) +
    geom_line(aes(
        x = Year,
        y = MSYBtrigger,
        linetype = "MSY B<sub>trigger</sub>",
        colour = "MSY B<sub>trigger</sub>",
        size = "MSY B<sub>trigger</sub>",
        text = map(
            paste0(
                "<b>MSY B<sub>trigger</sub>: </b>", tail(MSYBtrigger, 1)
            ), HTML
        )
    )) +
    theme_ICES_plots(type = "SSB", df)
   
# plot <- p + text_labels
# plot
# p4
#converting
fig4 <- ggplotly(p4, tooltip = "text") %>%
    layout(
        autosize = T,
        legend = list(
            itemsizing = "trace",
            orientation = "h",
            y = -.3,
            yanchor = "bottom",
            x = 0.5,
            xanchor = "center",
            itemwidth = 20,
            itemsizing= "trace",
            title = list(text = "")
        ),
        xaxis = list(zeroline = TRUE),
        annotations = list(
            showarrow = FALSE,
                text = tail(df$SAGStamp,1),
                font = list(family = "Calibri, serif", size = 12, color = "#acacac"),
                yref = "paper", y = 1, xref = "paper", x = 1,
                yanchor = "right", xanchor = "right")
    ) # nolint

for (i in 1:length(fig4$x$data)){
    if (!is.null(fig4$x$data[[i]]$name)){
        fig4$x$data[[i]]$name =  gsub("\\(","",str_split(fig4$x$data[[i]]$name,",")[[1]][1])
    }
}

# print(df %>% filter(Purpose == "Advice") %>% select(Year, low_SSB, SSB, high_SSB, Blim, Bpa, MSYBtrigger, stockSizeDescription, stockSizeUnits, SAGStamp)) #%>% 
# {if (is.na(df[nrow(df),2:4]) == c(TRUE,FALSE,TRUE)) head(df, -1)})
# { if(is.na(tail(df$high_SSB,1))) filter(head(df,-1)) } )
#     # {if(is.na(tail(high_SSB,1))) head(df, -1)  else .})
fig4
}

# ICES_plot_4(df, SAGstamp)

ICES_plot_5 <- function(df) {
    p5 <- df %>% filter(Purpose == "Advice") %>%
        select(Year, AssessmentYear, SSB, Blim, Bpa, MSYBtrigger, stockSizeDescription, stockSizeUnits, SAGStamp) %>%
        # drop_na(SSB, high_SSB) %>%
        #    gather(type, count, discards:landings) %>%
        ggplot(., aes(x = Year, y = SSB, color = AssessmentYear)) +
        geom_line(
            aes(
                x = Year,
                y = SSB,
                color = AssessmentYear,
                size = "SSB",
                linetype = "SSB",
                text = map(
                    paste0(
                        "<b>Year: </b>", Year,
                        "<br>",
                        "<b>Assessment year: </b>", AssessmentYear,
                        "<br>",
                        "<b>", stockSizeDescription, ": </b>", SSB, " ", stockSizeUnits
                    ), HTML
                )
            ) # ,
            # size = 1,
            # linetype = "solid",
        ) +
        geom_hline(aes(
            yintercept = tail(Blim, 1),
            linetype = "B<sub>Lim</sub>",
            colour = "B<sub>Lim</sub>",
            size = "B<sub>Lim</sub>",
            text = map(
                paste0(
                    "<b>B<sub>Lim</sub>: </b>", tail(Blim, 1)
                ), HTML
            )
        )) +
        geom_hline(aes(
            yintercept = tail(Bpa, 1),
            linetype = "B<sub>pa</sub>",
            colour = "B<sub>pa</sub>",
            size = "B<sub>pa</sub>",
            text = map(
                paste0(
                    "<b>B<sub>pa</sub>: </b>", tail(Bpa, 1)
                ), HTML
            )
        )) +
        geom_hline(aes(
            yintercept = tail(MSYBtrigger, 1),
            linetype = "MSY B<sub>trigger</sub>",
            colour = "MSY B<sub>trigger</sub>",
            size = "MSY B<sub>trigger</sub>",
            text = map(
                paste0(
                    "<b>MSY B<sub>trigger</sub>: </b>", tail(MSYBtrigger, 1)
                ), HTML
            )
        )) +
        theme_ICES_plots(type = "quality_SSB", df)
    # theme(legend.position = "none")

    # plot <- p + text_labels
    # plot
    # p5
    # converting
    fig5 <- ggplotly(p5, tooltip = "text") %>%
        layout(
            legend = list(
                orientation = "h",
                y = -.4,
                yanchor = "bottom",
                x = 0.5,
                xanchor = "center",
                title = list(text = "")
            ),
            xaxis = list(zeroline = TRUE),
            annotations = list(
                showarrow = FALSE,
                text = tail(df$SAGStamp,1),
                font = list(family = "Calibri, serif", size = 12, color = "#acacac"),
                yref = "paper", y = 1, xref = "paper", x = 1,
                yanchor = "right", xanchor = "right")
        ) # nolint

    for (i in 1:length(fig5$x$data)) {
        if (!is.null(fig5$x$data[[i]]$name)) {
            fig5$x$data[[i]]$name <- gsub("\\(", "", str_split(fig5$x$data[[i]]$name, ",")[[1]][1])
        }
    }
    fig5
}

# ICES_plot_5(df_qual[[1]], SAGstamp)

#F
ICES_plot_6 <- function(df) {
    p6 <- df %>% filter(Purpose == "Advice") %>%
        select(Year, F, FLim, Fpa, FMSY, Fage, fishingPressureDescription, AssessmentYear, SAGStamp) %>%
        # drop_na(SSB, high_SSB) %>%
        #    gather(type, count, discards:landings) %>%
        ggplot(., aes(x = Year, y = F, color = AssessmentYear)) +
        geom_line(
            aes(
                x = Year,
                y = F,
                color = AssessmentYear,
                size = "F",
                linetype = "F",
                text = map(
                    paste0(
                        "<b>Year: </b>", Year,
                        "<br>",
                        "<b>Assessment year: </b>", AssessmentYear,
                        "<br>",
                        "<b>", fishingPressureDescription, ": </b>", F
                    ), HTML
                )
            ) # ,
            # size = 1,
            # linetype = "solid",
        ) +
        geom_hline(aes(
            yintercept = tail(FLim, 1),
            linetype = "F<sub>Lim</sub>",
            colour = "F<sub>Lim</sub>",
            size = "F<sub>Lim</sub>",
            text = map(
                paste0(
                    "<b>F<sub>Lim</sub>: </b>", tail(FLim, 1)
                ), HTML
            )
        )) +
        geom_hline(aes(
            yintercept = tail(Fpa, 1),
            linetype = "F<sub>pa</sub>",
            colour = "F<sub>pa</sub>",
            size = "F<sub>pa</sub>",
            text = map(
                paste0(
                    "<b>F<sub>pa</sub>: </b>", tail(Fpa, 1)
                ), HTML
            )
        )) +
        geom_hline(aes(
            yintercept = tail(FMSY, 1),
            linetype = "F<sub>MSY</sub>",
            colour = "F<sub>MSY</sub>",
            size = "F<sub>MSY</sub>",
            text = map(
                paste0(
                    "<b>F<sub>MSY</sub>: </b>", tail(FMSY, 1)
                ), HTML
            )
        )) +
        theme_ICES_plots(type = "quality_F", df)
    # theme(legend.position = "none")

    # plot <- p + text_labels
    # plot
    # p6
    # converting
    fig6 <- ggplotly(p6, tooltip = "text") %>%
        layout(
            legend = list(
                orientation = "h",
                y = -.4,
                yanchor = "bottom",
                x = 0.5,
                xanchor = "center",
                title = list(text = "")
            ),
            xaxis = list(zeroline = TRUE),
            annotations = list(
                showarrow = FALSE,
                text = tail(df$SAGStamp,1),
                font = list(family = "Calibri, serif", size = 12, color = "#acacac"),
                yref = "paper", y = 1, xref = "paper", x = 1,
                yanchor = "right", xanchor = "right")
        ) # nolint

    for (i in 1:length(fig6$x$data)) {
        if (!is.null(fig6$x$data[[i]]$name)) {
            fig6$x$data[[i]]$name <- gsub("\\(", "", str_split(fig6$x$data[[i]]$name, ",")[[1]][1])
        }
    }
    fig6
}
# ICES_plot_6(df_qual[[1]], SAGstamp)
#Rec
ICES_plot_7 <- function(df) {
    p7 <- df %>% filter(Purpose == "Advice") %>%
        select(Year, recruitment, RecruitmentAge, AssessmentYear, SAGStamp) %>%
        drop_na(recruitment) %>%
        #    gather(type, count, discards:landings) %>%
        ggplot(., aes(x = Year, y = recruitment, color = AssessmentYear)) +
        geom_line(
            aes(
                x = Year,
                y = recruitment,
                color = AssessmentYear,
                size = "recruitment",
                linetype = "recruitment",
                text = map(
                    paste0(
                        "<b>Year: </b>", Year,
                        "<br>",
                        "<b>Assessment year: </b>", AssessmentYear,
                        "<br>",
                        "<b>Recruitment: </b>", recruitment
                    ), HTML
                )
            ) # ,
            # size = 1,
            # linetype = "solid",
        ) +
        theme_ICES_plots(type = "quality_R", df)
    # theme(legend.position = "none")

    # plot <- p + text_labels
    # plot
    # p7
    # converting
    fig7 <- ggplotly(p7, tooltip = "text") %>%
        layout(
            legend = list(
                orientation = "h",
                y = -.4,
                yanchor = "bottom",
                x = 0.5,
                xanchor = "center",
                title = list(text = "")
            ),
            xaxis = list(zeroline = TRUE),
            annotations = list(
                showarrow = FALSE,
                text = tail(df$SAGStamp,1),
                font = list(family = "Calibri, serif", size = 12, color = "#acacac"),
                yref = "paper", y = 1, xref = "paper", x = 1,
                yanchor = "right", xanchor = "right")
        ) # nolint

    for (i in 1:length(fig7$x$data)) {
        if (!is.null(fig7$x$data[[i]]$name)) {
            fig7$x$data[[i]]$name <- gsub("\\(", "", str_split(fig7$x$data[[i]]$name, ",")[[1]][1])
        }
    }
    fig7
}
# ICES_plot_7(df_qual[[1]], SAGstamp)