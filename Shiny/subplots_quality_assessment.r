######
data <- access_sag_data("cod.27.47d20", 2020)
data
# data %>% tibble()
years <- c(2021, 2020, 2019, 2018, 2017)
datalist = list()
for (i in years) {
    print(i)
    data_temp <- try(access_sag_data("cod.27.6a", i))

    ###############
    if(isTRUE(class(data_temp) == "try-error")) { next } else { 
    # 
    data_temp <- filter(data_temp, between(Year,2005, 2021))
    data_temp <- data_temp %>% select(Year,recruitment,SSB,F, Bpa,Blim,MSYBtrigger, FLim, Fpa, FMSY,  RecruitmentAge,AssessmentYear)
    datalist[[i]] <- data_temp
    # }
    } 
} 

# big_data = do.call(rbind, datalist)
big_data <- dplyr::bind_rows(datalist)

# data_temp <- filter(data, between(Year,2005, 2021))
# data_temp <- data_temp %>% select(Year,recruitment,SSB,F, Bpa,Blim, FLim, Fpa,RecruitmentAge,AssessmentYear)
# data_temp_
big_data$AssessmentYear <- as.factor(big_data$AssessmentYear)
# pal <- c("red", "blue", "green")
 fig1 <- plot_ly(
     data = big_data,
     x = ~Year, 
        y = ~SSB,
        split = ~AssessmentYear,
        type = 'scatter',
         mode = 'lines+markers',
         line = list(shape = "spline"),
         connectgaps=FALSE
         )

fig2 <- plot_ly(
     data = big_data,
     x = ~Year, 
        y = ~F,
        split = ~AssessmentYear,
        type = 'scatter',
         mode = 'lines+markers',
         line = list(shape = "spline"),
         connectgaps=FALSE
         )

fig3 <- plot_ly(
     data = big_data,
     x = ~Year, 
        y = ~recruitment,
        split = ~AssessmentYear,
        type = 'scatter',
         mode = 'lines+markers',
         line = list(shape = "spline"),
         connectgaps=FALSE
         )
fig <- subplot(fig1, fig2, fig3)
    fig

####################This is the long version but probably I can use the one above


fig1 <- plot_ly(
        data = datalist[[2017]], 
        x = ~Year, 
        y = ~SSB, 
        type = "scatter", 
        mode = "lines+markers",
        line = list(color = "red", shape = "spline"),
        # # showlegend = FALSE, 
        name = as.character(datalist[[2017]]$AssessmentYear[1])
        # color = ~AssessmentYear,
        
    )

    fig1 <- fig1 %>% add_trace(
        data = datalist[[2018]], 
        x = ~Year, 
        y = ~SSB,  
        type = "scatter", 
        mode = "lines+markers",
        # fill = "tonexty", 
        # fillcolor = "rgba(0,100,80,0.2)",
        line = list(color = "blue", shape = "spline"),
        # showlegend = FALSE, 
        name = as.character(datalist[[2018]]$AssessmentYear[1])
    )
fig1 <- fig1 %>% add_trace(
        data = datalist[[2019]], 
        x = ~Year, 
        y = ~SSB,  
        type = "scatter", 
        mode = "lines+markers",
        # fill = "tonexty", 
        # fillcolor = "rgba(0,100,80,0.2)",
        line = list(color = "blue", shape = "spline"),
        # showlegend = FALSE, 
        name = as.character(datalist[[2019]]$AssessmentYear[1])
    )
    fig1 <- fig1 %>% add_trace(
        data = datalist[[2020]], 
        x = ~Year, 
        y = ~SSB,  
        type = "scatter", 
        mode = "lines+markers",
        # fill = "tonexty", 
        # fillcolor = "rgba(0,100,80,0.2)",
        line = list(color = "blue", shape = "spline"),
        # showlegend = FALSE, 
        name = as.character(datalist[[2020]]$AssessmentYear[1])
    )

    fig1 <- fig1 %>% add_trace(
        data =  datalist[[2021]], 
        x = ~Year, 
        y = ~SSB,
        type = "scatter", 
        mode = "lines+markers",
        line = list(color = "green", shape = "spline"),
        name = as.character(datalist[[2021]]$AssessmentYear[1])
        # marker = list(size = 10), 
        # showlegend = TRUE
    )

    ## Add horizontal lines
    fig1 <- fig1 %>% add_trace(
        data = datalist[[2021]], 
        x = ~Year, 
        y = ~Blim, 
        name = "Blim", 
        type = "scatter", 
        mode = "lines",
        line = list(color = "black", shape = "linear", dash = "dash"), 
        showlegend = TRUE
    )
    fig1 <- fig1 %>% add_trace(
        data = datalist[[2021]], 
        x = ~Year, 
        y = ~Bpa, 
        name = "Bpa", 
        type = "scatter", 
        mode = "lines",
        line = list(color = "black", shape = "linear", dash = "dot"), 
        showlegend = TRUE
    )

    fig1 <- fig1 %>% add_trace(
        data = datalist[[2021]], 
        x = ~Year, 
        y = ~MSYBtrigger, 
        name = "MSYBtrigger", 
        type = "scatter", 
        mode = "lines",
        line = list(color = "orange", shape = "linear", dash = "dash"), 
        showlegend = TRUE
    )
source("Shiny/utilities_plotting.r")
    fig1 <- fig1 %>% layout(
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


###########################


fig2 <- plot_ly(
        data = datalist[[2019]], 
        x = ~Year, 
        y = ~F, 
        type = "scatter", 
        mode = "lines+markers",
        line = list(color = "red", shape = "spline"),
        # # showlegend = FALSE, 
        name = as.character(datalist[[2019]]$AssessmentYear[1])
        # color = ~AssessmentYear,
        
    )

    fig2 <- fig2 %>% add_trace(
        data = datalist[[2020]], 
        x = ~Year, 
        y = ~F,  
        type = "scatter", 
        mode = "lines+markers",
        # fill = "tonexty", 
        # fillcolor = "rgba(0,100,80,0.2)",
        line = list(color = "blue", shape = "spline"),
        # showlegend = FALSE, 
        name = as.character(datalist[[2020]]$AssessmentYear[1])
    )
    fig2 <- fig2 %>% add_trace(
        data =  datalist[[2021]], 
        x = ~Year, 
        y = ~F,
        type = "scatter", 
        mode = "lines+markers",
        line = list(color = "green", shape = "spline"),
        name = as.character(datalist[[2021]]$AssessmentYear[1])
        # marker = list(size = 10), 
        # showlegend = TRUE
    )

    ## Add horizontal lines
    fig2 <- fig2 %>% add_trace(
        data = datalist[[2021]], 
        x = ~Year, 
        y = ~FLim, 
        name = "FLim", 
        type = "scatter", 
        mode = "lines",
        line = list(color = "black", shape = "linear", dash = "dash"), 
        showlegend = TRUE
    )
    fig2 <- fig2 %>% add_trace(
        data = datalist[[2021]], 
        x = ~Year, 
        y = ~Fpa, 
        name = "Fpa", 
        type = "scatter", 
        mode = "lines",
        line = list(color = "black", shape = "linear", dash = "dot"), 
        showlegend = TRUE
    )

    # fig2 <- fig2 %>% add_trace(
    #     data = datalist[[2021]], 
    #     x = ~Year, 
    #     y = ~MSYBtrigger, 
    #     name = "MSYBtrigger", 
    #     type = "scatter", 
    #     mode = "lines",
    #     line = list(color = "orange", shape = "linear", dash = "dash"), 
    #     showlegend = TRUE
    # )
source("Shiny/utilities_plotting.r")
    fig2 <- fig2 %>% layout(
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

######################################

fig3 <- plot_ly(
        data = datalist[[2019]], 
        x = ~Year, 
        y = ~recruitment, 
        type = "scatter", 
        mode = "lines+markers",
        line = list(color = "red", shape = "spline"),
        # # showlegend = FALSE, 
        name = as.character(datalist[[2019]]$AssessmentYear[1])
        # color = ~AssessmentYear,
        
    )

    fig3 <- fig3 %>% add_trace(
        data = datalist[[2020]], 
        x = ~Year, 
        y = ~recruitment,  
        type = "scatter", 
        mode = "lines+markers",
        # fill = "tonexty", 
        # fillcolor = "rgba(0,100,80,0.2)",
        line = list(color = "blue", shape = "spline"),
        # showlegend = FALSE, 
        name = as.character(datalist[[2020]]$AssessmentYear[1])
    )
    fig3 <- fig3 %>% add_trace(
        data =  datalist[[2021]], 
        x = ~Year, 
        y = ~recruitment,
        type = "scatter", 
        mode = "lines+markers",
        line = list(color = "green", shape = "spline"),
        name = as.character(datalist[[2021]]$AssessmentYear[1])
        # marker = list(size = 10), 
        # showlegend = TRUE
    )

    ## Add horizontal lines
    # fig3 <- fig3 %>% add_trace(
    #     data = datalist[[2021]], 
    #     x = ~Year, 
    #     y = ~FLim, 
    #     name = "FLim", 
    #     type = "scatter", 
    #     mode = "lines",
    #     line = list(color = "black", shape = "linear", dash = "dash"), 
    #     showlegend = TRUE
    # )
    # fig3 <- fig3 %>% add_trace(
    #     data = datalist[[2021]], 
    #     x = ~Year, 
    #     y = ~Fpa, 
    #     name = "Fpa", 
    #     type = "scatter", 
    #     mode = "lines",
    #     line = list(color = "black", shape = "linear", dash = "dot"), 
    #     showlegend = TRUE
    # )

    # fig2 <- fig2 %>% add_trace(
    #     data = datalist[[2021]], 
    #     x = ~Year, 
    #     y = ~MSYBtrigger, 
    #     name = "MSYBtrigger", 
    #     type = "scatter", 
    #     mode = "lines",
    #     line = list(color = "orange", shape = "linear", dash = "dash"), 
    #     showlegend = TRUE
    # )
source("Shiny/utilities_plotting.r")
    fig3 <- fig3 %>% layout(
        title = "recruitment", 
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
            title = "recruitment",
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

    fig <- subplot(fig1, fig2, fig3)
    fig