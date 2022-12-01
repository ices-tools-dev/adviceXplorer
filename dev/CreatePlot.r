
library(dplyr)
library(tidyr)
library(plotly)
library(ggplot2)
library(ggradar)
  

MSYBtrigger <- 97777 
Blim <- 69841

SSBInterm <- 83301
CatchIterm <- 35358

tmp <- get_catch_scenario_table(stock_name = "ple.27.7d") #ple.27.7d cod.27.47d20
tmp$Year <- 2022

tmp2 <- tmp %>% select(Year, cS_Label, `Ftotal (2020)`, `SSB (2021)`, `Total catch (2020)`, `% TAC change (2020)`, `% Advice change (2020)`, `% SSB change (2021)` ) 

colnames(tmp2) <- c("Year", "cat", "F", "SSB", "TotCatch", "TACchange", "ADVICEchange", "SSBchange")
tmp2 <- tmp2 %>% do(bind_rows(., data.frame(Year = 2022, cat = "ref", F = 0, SSB = 0, TotCatch = 0, TACchange = 0, ADVICEchange = 0, SSBchange = 0))) 

sc <- head(tmp2$cat)

tmp3 <- tmp2 %>% relocate("SSB", .before = "SSBchange")


### working fine with ggRadar but could not convert it in plotly

yy <-  ggRadar(data = tmp2 %>% 
                        select(-Year) %>%
                        filter(cat %in% c(sc, "ref")),
    aes(group = cat))

yy    
ggsave("radarPlot.png", plot=yy)

ggplotly(yy)



###### can produce a plotly version with ggradar but needs values between 0 and 1 and couldn't think of a way of plotting it

tmp2 <- tmp2 %>% mutate(
    F = F / max(F),
    SSB = SSB / max(SSB),
    TotCatch = TotCatch / max(TotCatch),
    TACchange = case_when(
        length(TACchange < 0) > 0  ~ TACchange - min(TACchange)
        ),
    ADVICEchange = case_when(
        length(ADVICEchange < 0) > 0  ~ ADVICEchange - min(ADVICEchange)
        ),
    SSBchange = case_when(
        length(ADVICEchange < 0) > 0  ~ ADVICEchange - min(ADVICEchange)
        ),
    TACchange = TACchange / max(TACchange),
    ADVICEchange = ADVICEchange / max(ADVICEchange),
    SSBchange = SSBchange / max(SSBchange)
    )

p <- ggradar(tmp2 %>% select(-Year)) 
zz <- ggplotly(
    ggradar(tmp2 %>% select(-Year))       
)
style(zz,visible="legendonly")


htmlwidgets::saveWidget(zz, "test.html", selfcontained=FALSE)






#### test TAC time series
TAC <- read.csv2(file = '../codTAC.csv', header = T, sep=',')

TAC <- gather(TAC, "cat", "TAC", -Year)   


tmp3 <- tmp2 %>% select("Year", "cat", "TotCatch")
tmp3 <- rename_columns(tmp3, TotCatch = TAC)

TAC <- rbind(TAC, tmp3)
                        



p <- ggplot(data=TAC %>% filter(!cat %in% c("Official.landings", "ICES.landings",
 "ICES.discards")),
 aes(x=Year, y=TAC, group=cat, fill=cat)) +
    geom_line() +
    geom_point()

fig <- ggplotly(p)

fig


htmlwidgets::saveWidget(fig, "TACtimeseries.html", selfcontained=FALSE)



##########################################################################################
############################## tests #####################################################
##########################################################################################


p <- radarchart(tmp%>% 
 select(`Total catch (2020)`, `% TAC change (2020)`,`Ftotal (2020)`))


toPlot <-tmp %>%
        select(
            cS_Label, `Total catch (2020)`, `% TAC change (2020)`,`Ftotal (2020)`, `% SSB change (2021)`, `% TAC change (2020)`, `% Advice change (2020)`
        ) %>%
        mutate(
            `Total catch (2020)` = `Total catch (2020)`/ max(`Total catch (2020)`),
            `% TAC change (2020)`=abs(`% TAC change (2020)`)/max(abs(`% TAC change (2020)`)),
            `Ftotal (2020)`=`Ftotal (2020)`/max(`Ftotal (2020)`)
        )




zz <- ggplotly(
    ggradar(toPlot)       
)


htmlwidgets::saveWidget(zz, "test.html", selfcontained=FALSE)


xx <- ggplot(tmp)+
    geom_point(aes(x =`Total catch (2020)`, 
    y =`SSB (2021)`, 
    group = cS_Label, 
    fill= cS_Label)) +
    geom_hline(yintercept=MSYBtrigger)+
    geom_rect(aes(xmin=0, xmax=max(`Total catch (2020)`),
     ymin=0, ymax=Blim)) +
    geom_point(aes(x=CatchIterm , y=SSBInterm,size=3 ))

ggplotly(xx)





##test
set.seed(4) 
    df <- data.frame(matrix(runif(30), ncol = 10)) 
    df[, 1] <- paste0("G", 1:3) 
    colnames(df) <- c("Group", paste("Var", 1:9))                                                                

library(ggradar) 
     
    ggradar(df)                                                                                                  

# r$> library(ggradar) 
     
    ggplotly(ggradar(df))                                                                                        



library(ggplot2)
library(ggradar)
suppressPackageStartupMessages(library(dplyr))
library(scales)

mtcars %>%
     add_rownames( var = "group" ) %>%
     mutate_each(funs(rescale), -group) %>%
     tail(4) %>% select(1:10) -> mtcars_radar

ggradar(mtcars_radar) 




library(fmsb)
set.seed(1)
df2 <- data.frame(rbind(rep(10, 8), rep(0, 8),
                       matrix(sample(0:10, 24,
                                     replace = TRUE),
                              nrow = 3)))
colnames(df2) <- paste("Var", 1:8)
radarchart(df2) 




require(ggplot2)
require(ggiraph)
require(plyr)
require(reshape2)
require(moonBook)
require(sjmisc)

library(ggiraphExtra)
ggRadar(data=iris,aes(group=Species))
ggRadar(data=mtcars,interactive=TRUE)
ggRadar(data=mtcars,aes(colour=am,facet=cyl),interactive=TRUE)
ggRadar(data=acs,aes(colour=Dx,facet=Dx))
ggRadar(iris,aes(x=c(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width)))




##########################Tests Luca##########################################################################
tmp4 <- tmp3 %>% mutate(
    F = rescale(F, to = c(0, 1), from = range(c(min(F),max(F)))),
    SSB = rescale(SSB, to = c(0, 1), from = range(c(min(SSB),max(SSB)))),
    TotCatch = rescale(TotCatch, to = c(0, 1), from = range(c(min(TotCatch),max(TotCatch)))),
    TACchange = rescale(TACchange, to = c(0, 1), from = range(c(-100,100))),
    ADVICEchange = rescale(ADVICEchange, to = c(0, 1), from = range(c(-100,100))),
    SSBchange = rescale(SSBchange, to = c(0, 1), from = range(c(-100,100))),

)


# tmp2 <- tmp2 %>% mutate(
#     F = F / max(F),
#     SSB = SSB / max(SSB),
#     TotCatch = TotCatch / max(TotCatch),
#     TACchange = case_when(
#         length(TACchange < 0) > 0  ~ TACchange - min(TACchange)
#         ),
#     ADVICEchange = case_when(
#         length(ADVICEchange < 0) > 0  ~ ADVICEchange - min(ADVICEchange)
#         ),
#     SSBchange = case_when(
#         length(ADVICEchange < 0) > 0  ~ ADVICEchange - min(ADVICEchange)
#         ),
#     TACchange = TACchange / max(TACchange),
#     ADVICEchange = ADVICEchange / max(ADVICEchange),
#     SSBchange = SSBchange / max(SSBchange)
#     )


zz <- ggplotly(
    ggradar(tmp4 %>% select(-Year), axis.label.size = 10, axis.line.colour = "grey", legend.title = "Catch Scenarios:"
    # plot.extent.x.sf = 1, plot.extent.y.sf = 1)    
)
)
zz

zz <- ggradar(tmp4 %>% select(-Year), axis.label.size = 10, axis.line.colour = "grey", legend.title = "Catch Scenarios:")
ggplotly(zz) %>% style(hovertext = tmp4[2:4,"cat"])



htmlwidgets::saveWidget(zz, "test_luca.html", selfcontained=FALSE)




###############new plot


fig_F <- plot_ly(arrange(tmp2, F),
    x = ~TotCatch,
    y = ~F,
    type = "scatter",
    mode = "lines+markers",
    text = ~cat,
    hoverinfo = "text",
    showlegend = F,
    marker = list(size = 20),
    hovertemplate = paste('<i>Catch Scenario</i>: ', ~cat)
    
)
fig_F <- fig_F %>% add_annotations(x = ~TotCatch, y = ~F,
                  text = ~cat,
                  textfont = list(color = '#000000', size = 30),
                  xref = "x",
                  yref = "y",
                  showarrow = TRUE,
                  arrowhead = 4,
                  arrowsize = .5,
                  ax = c(20,-20),
                  ay = c(-80,40,80)
                  )



fig_SSB <- plot_ly(arrange(tmp2,F), x = ~TotCatch, y = ~SSB,   type = "scatter", mode = 'lines+markers', text = ~cat,  hoverinfo = 'text',showlegend = F,
        marker = list(size = 20))
fig_SSB <- fig_SSB %>% add_annotations(x = ~TotCatch, y = ~SSB,
                  text = ~cat,
                  textfont = list(color = '#000000', size = 30),
                  xref = "x",
                  yref = "y",
                  showarrow = TRUE,
                  arrowhead = 4,
                  arrowsize = .5,
                  ax = c(20,-20),
                  ay = c(-80,40,80)
                  )

fig <- subplot(fig_F, fig_SSB,
nrows = 1, shareX = TRUE, titleX = TRUE)#widths = c(0.5, 0.5), heights = c(0.5, 0.5), margin = c(0.06,0.06,0.02,0.02)
    #))

fig

##################################


labels <- sprintf(
        "Catch Scenario: %s", tmp2$cat
    ) %>% lapply(htmltools::HTML)


F0 <- tmp2[tmp2$cat == "F = 0", ]
Basis <- tmp2[tmp2$cat == "MSY approach: SSB (2021) = Blim",]

# data <- tmp2[tmp2$cat == "F = 0" & tmp2$cat == "MSY approach: SSB (2021) = Blim",]
# labels <- ~paste('</br> Catch Scenario: ', Species,
#                       '</br> Petal Length: ', Petal.Length,
#                       '</br> Petal Width: ', Petal.Width))

catch_scenarios_plot2 <- function(tmp) {
    tmp$Year <- 2022

    tmp2 <- tmp %>% select(Year, cS_Label, `Ftotal (2020)`, `SSB (2021)`, `Total catch (2020)`, `% TAC change (2020)`, `% Advice change (2020)`, `% SSB change (2021)`)

    colnames(tmp2) <- c("Year", "cat", "F", "SSB", "TotCatch", "TACchange", "ADVICEchange", "SSBchange")
    tmp2 <- tmp2 %>% do(bind_rows(., data.frame(Year = 2022, cat = "ref", F = 0, SSB = 0, TotCatch = 0, TACchange = 0, ADVICEchange = 0, SSBchange = 0)))

    sc <- head(tmp2$cat)


    fig_catch <- plot_ly(arrange(tmp2, F)) %>%
        add_trace(
            x = ~ jitter(TotCatch, 1, amount = NULL),
            y = ~ jitter(F, 1, amount = NULL),
            type = "scatter",
            mode = "lines+markers",
            text = labels,
            marker = list(size = 20),
            name = "F"
        )
    ay <- list(
        tickfont = list(color = "red"),
        overlaying = "y",
        side = "right",
        title = "<b>SSB</b>"
    )
    fig_catch <- fig_catch %>% add_trace(
        x = ~ jitter(TotCatch, 1, amount = NULL),
        y = ~ jitter(SSB, 1, amount = NULL),
        type = "scatter",
        mode = "lines+markers",
        text = labels,
        marker = list(size = 20),
        name = "SSB",
        yaxis = "y2"
    )

    a <- list(
        x = F0$TotCatch,
        y = F0$F,
        text = F0$cat,
        xref = "x",
        yref = "y",
        showarrow = TRUE,
        arrowhead = 15,
        ax = 200,
        ay = 50,
        font = list(
            color = "#000000",
            family = "sans serif",
            size = 30
        )
    )
    b <- list(
        x = Basis$TotCatch,
        y = Basis$F,
        text = Basis$cat,
        xref = "x",
        yref = "y",
        showarrow = TRUE,
        arrowhead = 15,
        ax = 200,
        ay = 50, font = list(
            color = "#000000",
            family = "sans serif",
            size = 30
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
        yaxis2 = ay,
        xaxis = list(title = "<b>Total Catch</b>"),
        yaxis = list(title = "<b>F</b>") # ,
        #   annotations = a
    )
    fig_catch <- fig_catch %>% layout(
        annotations = a
    )
    fig_catch <- fig_catch %>% layout(
        annotations = b
    )
    # fig_catch <- fig_catch %>% layout(
    #     annotations = d
    # )

    fig_catch
}



        # hoverinfo = ~cat,
        # hovertemplate = paste('<br><strong> Catch Scenario:</strong><br/>',  ~cat)
                        

    # fig_F <- fig_F %>% add_annotations(
    #     x = ~TotCatch, y = ~F,
    #     text = ~cat,
    #     textfont = list(color = "#000000", size = 50),
    #     xref = "x",
    #     yref = "y",
    #     showarrow = TRUE,
    #     arrowhead = 10,
    #     arrowsize = 1,
    #     ax = c(-80),
    #     ay = c(0)
    # )
    #  fig_F <- fig_F %>% layout(
    #     xaxis = list(
    #          title = "Total Catch"),
    #     yaxis= list(
    #          title = "F")
    #  )



    # fig_SSB <- plot_ly(arrange(tmp2, F),
    #     x = ~TotCatch, y = ~SSB, mode = "lines+markers", #text = ~cat,
    #     marker = list(size = 20)
    # )
    # fig_SSB <- fig_SSB %>% add_annotations(
    #     x = ~TotCatch, y = ~SSB,
    #     text = ~cat,
    #     textfont = list(color = "#000000", size = 50),
    #     xref = "x",
    #     yref = "y",
    #     showarrow = TRUE,
    #     arrowhead = 10,
    #     arrowsize = 1,
    #     ax = c(-90,-80,-70),
    #     ay = c(-10,0,10)
    # )
    # fig_SSB <- fig_SSB %>% layout(
    #     xaxis = list(
    #          title = "Total Catch"),
    #     yaxis= list(
    #          title = "SSB")
    #  )

    # fig <- subplot(fig_F, fig_SSB,
    #     nrows = 1, shareX = TRUE, titleX = TRUE, titleY = TRUE#, heights = c(1, 1)
    # ) # widths = c(0.5, 0.5), heights = c(0.5, 0.5), margin = c(0.06,0.06,0.02,0.02)
    # # ))

    # fig
# }


###################################################
# tmp5 <- tmp3 %>% select(-Year)

# library(plotly)

# fig <- plot_ly(
#     type = 'scatterpolar',
#     fill = 'none',
#     mode = "markers+lines"
#   ) 
# fig <- fig %>%
#   add_trace(
#     r = c(tmp6[tmp6$cat == "F = 0",2:dim(tmp6)[2]]),
#     theta = c(names(tmp6[2:dim(tmp6)[2]])),
#     name = "F = 0"
#   ) 
# fig

# fig <- fig %>%
#   add_trace(
#     r = c(1.5, 10, 39, 31, 15, 1.5),
#     theta = c(names(tmp6)),
#     name = 'Group B'
#   ) 
# fig <- fig %>%
#   layout(
#     polar = list(
#       radialaxis = list(
#         visible = T,
#         range = c(0,1)
#       )
#     )
#   )

# fig




tmp <- get_catch_scenario_table(stock_name = "wit.27.3a47d") #ple.27.7d cod.27.47d20 pok.27.1-2 ple.27.420 wit.27.3a47d



standardize_catch_scenario_table <- function(tmp){
tmp$Year <- 2022
###################################### code tests to try to accept as many catch scen tables headings
# patterns <- c("cS_Label", "Ftotal", "Total catch","TAC","% advice change","% SSB change")
tmp_unified <- data.frame()
#Year
pattern <- c("Year")
subset <- grepl(paste(pattern, collapse="|"), names(tmp))

tmp_unified <- tmp[,c(subset)]
# tmp_unified <-unlist(tmp[,c(subset)],use.names = FALSE)
# cS_Label"
pattern <- c("cS_Label")
subset <- grepl(paste(pattern, collapse="|"), names(tmp))
# tmp_unified$cat <- tmp[,c(subset)]
tmp_unified <- tmp_unified %>% add_column(tmp[,c(subset)])

# Ftotal"
pattern <- c("Ftotal","F_total","F total")
subset <- grepl(paste(pattern, collapse="|"), names(tmp))
# tmp_unified$F <- tmp[,c(subset)]
tmp_unified <- tmp_unified %>% add_column(tmp[,c(subset)])

# Total catch"
pattern <- c("Total catch")
subset <- grepl(paste(pattern, collapse="|"), names(tmp))
# tmp_unified$TotCatch <- tmp[,c(subset)]
tmp_unified <- tmp_unified %>% add_column(tmp[,c(subset)])

# % TAC change"
pattern <- c("% TAC change", "TAC", "TAC change", "% TAC")
subset <- grepl(paste(pattern, collapse="|"), names(tmp))
# tmp_unified$TACchange <- tmp[,c(subset)]
tmp_unified <- tmp_unified %>% add_column(tmp[,c(subset)])

# % Advice change"
pattern <- c("% Advice change", "Advice change")
subset <- grepl(paste(pattern, collapse="|"), names(tmp))
# tmp_unified$ADVICEchange <- tmp[,c(subset)]
tmp_unified <- tmp_unified %>% add_column(tmp[,c(subset)])

# SSB"
pattern <- c("SSB (2021)")
subset <- which(names(tmp) == pattern) 
if (length(subset) == 0){
    pattern <- c("SSB (2020)")
    subset <- which(names(tmp)== pattern) 
}
# 
# subset <- grepl(paste(pattern, collapse="|"), names(tmp))
# tmp_unified$SSB <- tmp[,c(subset)]
tmp_unified <- tmp_unified %>% add_column(tmp[,c(subset)])


# % SSB change "
pattern <- c("% SSB change", "SSB change")
subset <- grepl(paste(pattern, collapse="|"), names(tmp))
# tmp_unified$SSBchange <- tmp[,c(subset)]
tmp_unified <- tmp_unified %>% add_column(tmp[,c(subset)])

colnames(tmp_unified) <- c("Year", "cat", "F",  "TotCatch", "TACchange", "ADVICEchange","SSB", "SSBchange")
# rename_all(tmp_unified,c("Year", "cat", "F",  "TotCatch", "TACchange", "ADVICEchange", "SSBchange","SSB"))
tmp_unified <- tmp_unified %>% do(bind_rows(., data.frame(Year = 2022, cat = "ref", F = 0, TotCatch = 0, TACchange = 0, ADVICEchange = 0, SSBchange = 0,  SSB = 0)))
# result <- filter(tmp, grepl(paste(patterns, collapse="|"), names(tmp)))
# subset <- grepl(paste(patterns, collapse="|"), names(tmp))
# result <- tmp[,c(subset)]
# grepl("Ftotal",names(tmp))
# grepl("SSB",names(tmp))
# grepl("Total catch",names(tmp)) 

########################################
# tmp2 <- tmp %>% select(Year, cS_Label, `Ftotal (2020)`, `SSB (2021)`, `Total catch (2020)`, `% TAC change (2020)`, `% Advice change (2020)`, `% SSB change (2021)` ) 

# colnames(tmp2) <- c("Year", "cat", "F", "SSB", "TotCatch", "TACchange", "ADVICEchange", "SSBchange")
# tmp2 <- tmp2 %>% do(bind_rows(., data.frame(Year = 2022, cat = "ref", F = 0, SSB = 0, TotCatch = 0, TACchange = 0, ADVICEchange = 0, SSBchange = 0))) 

# sc <- head(tmp_unified$cat)
return(tmp_unified)
# tmp3 <- tmp2 %>% relocate("SSB", .before = "SSBchange")
}
standardize_catch_scenario_table(tmp)
