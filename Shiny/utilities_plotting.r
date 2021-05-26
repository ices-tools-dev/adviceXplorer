### Plotting utilities

## Libraries
library(htmlwidgets)
library(dplyr)
library(ggplot2)
library(dygraphs)
library(htmltools)
library(widgetframe)
library(icesSAG)
library(plotly)


## Formatting axis title (titlefont)
titlefont_format <- function() {
    f1 <- list(
        family = "Arial, sans-serif",
        size = 30,
        color = "darkgrey")
}
## Formatting tick axis font (tickfont)
tickfont_format <- function() {
    f2 <- list(
        family = "Arial, serif",
        size = 25,
        color = "black")
}
## Formatting legend
legend_format <- function() {
    leg <- list(
        font = list(
        family = "sans-serif",
        size = 25,
        color = "#000"),
        bgcolor = "#E2E2E2",
        bordercolor = "#FFFFFF",
        borderwidth = 2)
}

################## Plot 1 - Catches ################
figure_1_catches <- function(data, years, catches, landings, discards) {
    #Make sure that if the column landings is empy, the column catches is plotted
    if (all(is.na(data[, "landings"]))) {
        data$landings <- data$catches
    }
    # Start the plot
    fig1 <- plot_ly(
        data = data,
        x = ~years,
        y = ~landings,
        name = "Landings",
        type = "bar",
        hoverinfo = 'text',
        text = ~paste('Year:', Year, '<br>Landings:', landings),
        marker = list(color = '#66a4cd',
                      line = list(color = 'black',
                                  width = 0.5)),
        showlegend = TRUE)

    fig1 <- fig1 %>% add_trace(
        data = data,
        x = ~years,
        y = ~discards,
        name = "Discards",
        type = "bar",
        hoverinfo = 'text',
        text = ~paste('Year:', Year, '<br>Discards:', discards),
        marker = list(color = '#a00130',
                        line = list(color = 'black',
                                    width = 0.5)),
        showlegend = TRUE)

    fig1 <- fig1 %>% layout(title = "Catches",
            xaxis = list(title = "Years",
            titlefont = titlefont_format(),
            tickfont = tickfont_format(),
            showticklabels = TRUE),
            barmode = "stack",
            legend = legend_format(),
            yaxis = list(title = "Catches",
            titlefont = titlefont_format(),
            tickfont = tickfont_format(),
            showticklabels = TRUE))
    fig1

}

# figure_1_catches(catches, catches$Year, catches$landings, catches$discards)


################## Plot 2 - Recruitment ################
figure_2_recruitment <- function(data, years, recruitment, low_recruitment, high_recruitment){
    fig2 <- plot_ly(
            data = data,
            x = ~years,
            y = ~recruitment,
            name = "recruitment",
            type = "bar",
            hoverinfo = "text",
            text = ~ paste("Year:", years, "<br>recruitment:", recruitment),
            marker = list(
                color = "#cd6666",
                line = list(
                    color = "black",
                    width = 0.5
                )
            ),
            error_y = list(
                type = "data",
                symmetric = FALSE,
                arrayminus = ~low_recruitment,
                array = ~ high_recruitment,
                color = "#000000")
                #orientation = "v",
                #type = "bar"
            #)
            # error_y = list(
            #     array = ~err,
            #     type = "data",
            #     color = "#000000"
            # )
        )

        fig2 <- fig2 %>% layout(
            title = "Recruitment",
            xaxis = list(
                title = "Years",
                titlefont = titlefont_format(),
                tickfont = tickfont_format(),
                showticklabels = TRUE
            ),
            yaxis = list(
                title = "Recruitment",
                titlefont = titlefont_format(),
                tickfont = tickfont_format(),
                showticklabels = TRUE
            )
        )

    fig2

}
# figure_2_recruitment(R, R$Year, R$recruitment,R$low_recruitment,R$high_recruitment)


################## Plot 3 - Fish Mortality ################
figure_3_fish_mortality <- function(data, years, low_F, F, high_F, FLim, Fpa, FMSY){
    fig3 <- plot_ly(
        data = data, 
        x = ~years, 
        y = ~high_F, 
        type = "scatter", 
        mode = "lines",
        line = list(color = "transparent", shape = "linear"),#
        showlegend = FALSE, 
        name = "high_F"
    )
    fig3 <- fig3 %>% add_trace(
        data = data, 
        y = ~low_F, 
        type = "scatter", 
        mode = "lines",
        fill = "tonexty", 
        fillcolor = "rgba(255,71,26,0.2)", #"rgba(0,100,80,0.2)"
        line = list(color = "transparent", shape = "linear"),
        showlegend = FALSE, 
        name = "low_F"
    )
    fig3 <- fig3 %>% add_trace(
        data = data, 
        x = ~years, 
        y = ~F, 
        type = "scatter", 
        mode = "lines+markers",
        line = list(color = "rgb(255,71,26)", shape = "linear"), 
        name = "F",
        marker = list(size = 10, color = "rgb(255,71,26)"), 
        showlegend = TRUE
    )

    ## Add horizontal lines
    fig3 <- fig3 %>% add_trace(
        data = data, 
        x = ~years, 
        y = ~FLim, 
        name = "FLim", 
        type = "scatter", 
        mode = "lines",
        line = list(color = "black", shape = "linear", dash = "dash"), 
        showlegend = TRUE
    )
    fig3 <- fig3 %>% add_trace(
        data = data, 
        x = ~years, 
        y = ~Fpa, 
        name = "Fpa", 
        type = "scatter", 
        mode = "lines",
        line = list(color = "black", shape = "linear", dash = "dot"), 
        showlegend = TRUE
    )
    fig3 <- fig3 %>% add_trace(
        data = data, 
        x = ~years, 
        y = ~FMSY, 
        name = "FMSY", 
        type = "scatter", 
        mode = "lines",
        line = list(color = "orange", shape = "linear", dash = "dash"), 
        showlegend = TRUE
    )

    fig3 <- fig3 %>% layout(
        title = "F", 
        legend = legend_format(),
        paper_bgcolor = "rgb(255,255,255)", 
        plot_bgcolor = "rgb(229,229,229)",
        xaxis = list(
            title = "Years",
            gridcolor = "rgb(255,255,255)",
            showgrid = TRUE,
            showline = TRUE,
            showticklabels = TRUE,
            tickcolor = "rgb(127,127,127)",
            ticks = "outside",
            zeroline = TRUE,
            titlefont = titlefont_format(),
            tickfont = tickfont_format()
        ),
        yaxis = list(
            title = "F",
            gridcolor = "rgb(255,255,255)",
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

    fig3
    }

# figure_3_fish_mortality(f, f$Year, f$low_F, f$F, f$high_F, f$FLim, f$Fpa, f$FMSY)

################## Plot 4 - SBB ################
figure_4_SSB <- function(data, years, low_SSB, SSB, high_SSB, Blim, Bpa, MSYBtrigger){
    fig4 <- plot_ly(
        data = data, 
        x = ~years, 
        y = ~high_SSB, 
        type = "scatter", 
        mode = "lines",
        line = list(color = "transparent", shape = "linear"),
        showlegend = FALSE, 
        name = "high_SSB"
    )
    fig4 <- fig4 %>% add_trace(
        data = data, 
         x = ~years,
        y = ~low_SSB, 
        type = "scatter", 
        mode = "lines",
        fill = "tonexty", 
        fillcolor = "rgba(0,100,80,0.2)",
        line = list(color = "transparent", shape = "linear"),
        showlegend = FALSE, 
        name = "low_SSB"
    )
    fig4 <- fig4 %>% add_trace(
        data = data, 
        x = ~years, 
        y = ~SSB, 
        type = "scatter", 
        mode = "lines+markers",
        line = list(color = "rgb(0,100,80)", shape = "linear"), 
        name = "SSB",
        marker = list(size = 10), 
        showlegend = TRUE
    )

    ## Add horizontal lines
    fig4 <- fig4 %>% add_trace(
        data = data, 
        x = ~years, 
        y = ~Blim, 
        name = "Blim", 
        type = "scatter", 
        mode = "lines",
        line = list(color = "black", shape = "linear", dash = "dash"), 
        showlegend = TRUE
    )
    fig4 <- fig4 %>% add_trace(
        data = data, 
        x = ~years, 
        y = ~Bpa, 
        name = "Bpa", 
        type = "scatter", 
        mode = "lines",
        line = list(color = "black", shape = "linear", dash = "dot"), 
        showlegend = TRUE
    )
    fig4 <- fig4 %>% add_trace(
        data = data, 
        x = ~years, 
        y = ~MSYBtrigger, 
        name = "MSYBtrigger", 
        type = "scatter", 
        mode = "lines",
        line = list(color = "orange", shape = "linear", dash = "dash"), 
        showlegend = TRUE
    )

    fig4 <- fig4 %>% layout(
        title = "SSB", 
        legend = legend_format(),
        paper_bgcolor = "rgb(255,255,255)", 
        plot_bgcolor = "rgb(229,229,229)",
        xaxis = list(
            title = "Years",
            gridcolor = "rgb(255,255,255)",
            showgrid = TRUE,
            showline = TRUE,
            showticklabels = TRUE,
            tickcolor = "rgb(127,127,127)",
            ticks = "outside",
            zeroline = TRUE,
            titlefont = titlefont_format(),
            tickfont = tickfont_format()
        ),
        yaxis = list(
            title = "SSB",
            gridcolor = "rgb(255,255,255)",
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


    fig4
}

#figure_4_SSB(SSB, SSB$Year, SSB$low_SSB, SSB$SSB, SSB$high_SSB, SSB$Blim, SSB$Bpa, SSB$MSYBtrigger)



#####################Subplots quality of assessment
quality_assessment_plots <- function(big_data, big_data_last_year) {
# pal <- c("red", "blue", "green")
 fig1 <- plot_ly(
     data = big_data,
     x = ~Year,
     y = ~SSB,
     split = ~AssessmentYear,
     type = "scatter",
     mode = "lines+markers",
    #  line = list(shape = "spline"),
     connectgaps = FALSE,
     color = ~AssessmentYear
 )
 fig1 <- fig1 %>% add_trace(
        data = big_data_last_year, ###select for last reference points last year
        x = ~Year, 
        y = ~Blim, 
        name = "Blim", 
        type = "scatter", 
        mode = "lines",
        line = list(color = "black", shape = "linear", dash = "dash"), 
        showlegend = TRUE
    )
    fig1 <- fig1 %>% add_trace(
        data = big_data_last_year, 
        x = ~Year, 
        y = ~Bpa, 
        name = "Bpa", 
        type = "scatter", 
        mode = "lines",
        line = list(color = "black", shape = "linear", dash = "dot"), 
        showlegend = TRUE
    )

    fig1 <- fig1 %>% add_trace(
        data = big_data_last_year, 
        x = ~Year, 
        y = ~MSYBtrigger, 
        name = "MSYBtrigger", 
        type = "scatter", 
        mode = "lines",
        line = list(color = "orange", shape = "linear", dash = "dash"), 
        showlegend = TRUE
    )
 fig1 <- fig1 %>% layout(
        # title = "SSB", 
        legend = legend_format(),
        paper_bgcolor = "rgb(255,255,255)", 
        plot_bgcolor = "rgb(229,229,229)",
        xaxis = list(
            title = "Years",
            gridcolor = "rgb(255,255,255)",
            showgrid = TRUE,
            showline = TRUE,
            showticklabels = TRUE,
            tickcolor = "rgb(127,127,127)",
            ticks = "outside",
            zeroline = TRUE,
            titlefont = titlefont_format(),
            tickfont = tickfont_format()
        ),
        yaxis = list(
            title = "SSB",
            gridcolor = "rgb(255,255,255)",
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

 fig2 <- plot_ly(
     data = big_data,
     x = ~Year,
     y = ~F,
     split = ~AssessmentYear,
     type = "scatter",
     mode = "lines+markers",
    #  line = list(shape = "spline"),
     connectgaps = FALSE,
     color = ~AssessmentYear,
     showlegend = FALSE
 )
 ## Add horizontal lines
    fig2 <- fig2 %>% add_trace(
        data = big_data_last_year, 
        x = ~Year, 
        y = ~FLim, 
        name = "FLim", 
        type = "scatter", 
        mode = "lines",
        line = list(color = "black", shape = "linear", dash = "dash"), 
        showlegend = TRUE
    )
    fig2 <- fig2 %>% add_trace(
        data = big_data_last_year, 
        x = ~Year, 
        y = ~Fpa, 
        name = "Fpa", 
        type = "scatter", 
        mode = "lines",
        line = list(color = "black", shape = "linear", dash = "dot"), 
        showlegend = TRUE
    )

    fig2 <- fig2 %>% add_trace(
        data = big_data_last_year, 
        x = ~Year, 
        y = ~FMSY, 
        name = "FMSY", 
        type = "scatter", 
        mode = "lines",
        line = list(color = "#ff5100", shape = "linear", dash = "dash"), 
        showlegend = TRUE
    )
fig2 <- fig2 %>% layout(
        # title = "F", 
        legend = legend_format(),
        paper_bgcolor = "rgb(255,255,255)", 
        plot_bgcolor = "rgb(229,229,229)",
        xaxis = list(
            title = "Years",
            gridcolor = "rgb(255,255,255)",
            showgrid = TRUE,
            showline = TRUE,
            showticklabels = TRUE,
            tickcolor = "rgb(127,127,127)",
            ticks = "outside",
            zeroline = TRUE,
            titlefont = titlefont_format(),
            tickfont = tickfont_format()
        ),
        yaxis = list(
            title = "F",
            gridcolor = "rgb(255,255,255)",
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

 fig3 <- plot_ly(
     data = big_data,
     x = ~Year,
     y = ~recruitment,
     split = ~AssessmentYear,
     type = "scatter",
     mode = "lines+markers",
    #  line = list(shape = "spline"),
     connectgaps = FALSE,
     color = ~AssessmentYear,
     showlegend = FALSE
 )
fig3 <- fig3 %>% layout(
        # title = "R", 
        legend = legend_format(),
        paper_bgcolor = "rgb(255,255,255)", 
        plot_bgcolor = "rgb(229,229,229)",
        xaxis = list(
            title = "Years",
            gridcolor = "rgb(255,255,255)",
            showgrid = TRUE,
            showline = TRUE,
            showticklabels = TRUE,
            tickcolor = "rgb(127,127,127)",
            ticks = "outside",
            zeroline = TRUE,
            titlefont = titlefont_format(),
            tickfont = tickfont_format()
        ),
        yaxis = list(
            title = "R",
            gridcolor = "rgb(255,255,255)",
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

 fig <- subplot(fig1, fig2, fig3, 
 nrows = 1, shareX = TRUE, titleX = TRUE,titleY = TRUE,  widths = c(0.33, 0.33, 0.33), margin = 0.05)#, ,
#  nrows = 1,
#    widths = NULL,
#    heights = NULL,
#    margin = 0.02,
#    shareX = TRUE,
#    shareY = FALSE,
#    titleX = shareX)
#    titleY = shareY,
#    which_layout = "merge")
 fig
}

# quality_assessment_plots(big_data, big_data_last_year)
