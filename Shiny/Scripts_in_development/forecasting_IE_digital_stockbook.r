###############
  # Forecasting #
  ###############
  #setwd("H:/Stockbook/shiny/WIP")
  Forecasting=read.csv('ForecastingData.csv', header=TRUE, stringsAsFactors=FALSE)
  Forecasting$value <- as.numeric(as.character(Forecasting$value))
  # Djc Basis was brought in as a factor
  Forecasting$Basis <- as.character(Forecasting$Basis)
  # djc Forecasting$value <- as.numeric(Forecasting$value)
  Forecasting$Year <- as.numeric(Forecasting$Year)
  Forecasting = Forecasting[,2:6]
  
  output$ForecastOptionsSelector <- renderUI({
    # djc 9/11/21 Filtering was previously only done by area description! - Fixed to filter by species and area
    #sbl <- filter(Forecasting, 
    #              FishStock==paste0(ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"New"]))
    stockToFilter <- ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"New" ]
    if (length(stockToFilter) == 0){
      # djc 9/11/21 Leave the function if we can't find a match - stops error messages
      # when you move between species with different valid areas
      return(NULL)
    }
    sbl <- filter(Forecasting, FishStock==stockToFilter)
    #print(input$speciesbydiv)
    #print(paste0(ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"New"]))
    #print(unique(Forecasting$FishStock))
    ## djc
    #sbl <- filter(Forecasting, FishStock=='cod.27.6a')
    Options <- unique(sbl$Basis)
    Options <- Options[Options!= "Assessment"]
    Options <- Options[Options!= "ICES Advice"]
    Options <- Options[Options!= "TAC"]
    # djc 23/11/21 Sort the options alphabetically - shoudl match forecast table then
    Options <- sort(Options)
    checkboxGroupInput("forecastoptionselection", h3("Select Forecast Options"), as.list(Options) ,
                       inline = TRUE) #, selected = "F = F2017"
  })

  #sbl <-reactive({
  #  filter(Forecasting, FishStock==paste0(ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"New"]))
  #})

  # djc 23/11/21
  #mypalette<-primary.colors(length(factor(Forecasting$Basis)))
  mypalette<-primary.colors(length(unique(Forecasting$Basis)))
  #print(mypalette)
  
  
  #output$plotforecasting <- renderPlotly({
  output$plotSSB <- renderPlotly({
    # djc 9/11/21 Filtering was previously only done by area description! - Fixed to filter by species and area
    # sbl <- filter(Forecasting, FishStock==paste0(ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"New"]))
    stockToFilter <- ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"New" ]
    if (length(stockToFilter) == 0){
      # djc 9/11/21 Leave the function if we can't find a match - stops error messages
      # when you move between species with different valid areas
      return(NULL)
    }
    sbl <- filter(Forecasting, FishStock==stockToFilter)
    ## djc
    #sbl <- filter(Forecasting, FishStock=='cod.27.6a')
    ssb <- filter(sbl, var=="SSB")
    Blim <- filter(sbl, var=="Blim")[1,5]
    Bpa <- filter(sbl, var=="Bpa")[1,5]
    ssb1 <- filter(ssb, Basis %in% c("Assessment", "ICES Advice"))
    #print(ssb1)
    #ssb2018= data.frame(FishStock=ssb1[1,1], Year=2018, Basis="Assessment", 
    #                    var="SSB", value=ssb1[ssb1$Year==2018 & ssb1$Basis=="ICES Advice",][,5])
    ssb2 <- filter(ssb, Basis %in% c(input$forecastoptionselection))
    ## djc
    #ssb2 <- filter(ssb, Basis %in% c('F=FMSY'))
    ssb3 <- rbind(ssb1, ssb2)#, ssb2018
    
    if (length(ssb3[ssb3$Year==2021 & ssb3$Basis=="Assessment",][,5])>0){
      ssb3[ssb3$Year==2021 & ssb3$Basis=="Assessment",][,5] <- head(ssb3[ssb3$Year==2021 & ssb3$Basis=="ICES Advice",][,5],1)
    }
    # SM Oct2021: Changed 2020 to 2021
    # SM Nov2020: Changed 2019 to 2020
    # SM this is the 2019 line 
    #ssb3[ssb3$Year==2019 & ssb3$Basis=="Assessment",][,5] <- head(ssb3[ssb3$Year==2019 & ssb3$Basis=="ICES Advice",][,5],1)
    # DJC ssb3[ssb3$Year==2019 & ssb3$Basis=="Assessment",][,5] <- ssb3[ssb3$Year==2019 & ssb3$Basis=="ICES Advice",][,5]
    # DJC ssb3[ssb3$Year==2018 & ssb3$Basis=="Assessment",][,5] <- ssb3[ssb3$Year==2018 & ssb3$Basis=="ICES Advice",][,5]
    
    # djc 23/11/21 Use a defined ordering for the plots to stop the lines changing color
    myCustomOrder <- unlist(lapply(sbl$Basis, FUN = function(x) switch(x, "Assessment" = 1, "ICES Advice" = 2, "TAC" = 3, 4)))
    myPlotFactorOrder <- unique(sbl[order(myCustomOrder,sbl$Basis),"Basis"])
    ssb3$Basis <- factor(ssb3$Basis, levels = myPlotFactorOrder)
    
    p1 <- plot_ly(ssb3, x = ~Year, y = ~value, type = 'scatter', mode = 'lines', showlegend = F, #linetype = ~Basis,
            color = ~Basis, colors=mypalette, height=375) %>% 
      layout(hovermode="FALSE", #showlegend = FALSE,
             xaxis = list(title = 'Year', range= c(min(ssb3$Year), max(ssb3$Year)+1)),
             yaxis = list (title = 'SSB', range = c(0, max(ssb3$value, na.rm = T)*1.05)),
             shapes = list(
               list(type = "rect", fillcolor = "green", opacity = 0.2, 
                    line = list(color = "green", opacity=0.2), x0 = min(sbl$Year), x1 = max(sbl$Year)+1, 
                    xref = "Year", y0 = Bpa, y1 = max(ssb$value, na.rm=TRUE)*1.05, yref = "value"),
               list(type = "rect", fillcolor = "orange", opacity = 0.2, 
                    line = list(color = "orange", opacity=0.2), x0 = min(sbl$Year), x1 = max(sbl$Year)+1, 
                    xref = "Year", y0 = Blim, y1 = Bpa, yref = "value"),
               list(type = "rect", fillcolor = "red", opacity = 0.2, 
                    line = list(color = "red", opacity=0.2), x0 = min(sbl$Year), x1 = max(sbl$Year)+1, 
                    xref = "Year", y0 = 0, y1 = Blim, yref = "value")))
    p1$elementId <- NULL
    p1
  })
  
  output$plotF <- renderPlotly({
    # djc 9/11/21 Filtering was previously only done by area description! - Fixed to filter by species and area
    #sbl <- filter(Forecasting, FishStock==paste0(ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"New"]))
    stockToFilter <- ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"New" ]
    if (length(stockToFilter) == 0){
      # djc 9/11/21 Leave the function if we can't find a match - stops error messages
      # when you move between species with different valid areas
      return(NULL)
    }
    sbl <- filter(Forecasting, FishStock==stockToFilter)
    f <- filter(sbl, var=="F")
    #if(dim(f[f$Year==2018 & f$Basis=="Assessment",])[1]==0){
    #      f2018= data.frame(FishStock=f[1,1], Year=2018, Basis="Assessment", 
    #                    var="F", value=f[f$Year==2018 & f$Basis=="ICES Advice",][,5])
    #      f=rbind(f, f2018)
    #}else{
    
    if (length(f[f$Year==2021 & f$Basis=="Assessment",][,5])>0){
      f[f$Year==2021 & f$Basis=="Assessment",][,5] <- head(f[f$Year==2021 & f$Basis=="ICES Advice",][,5],1)
    }
    # SM Oct2021: Changed 2020 to 2021
    # SM Nov2020: Changed 2019 to 2020
    # DJC f[f$Year==2019 & f$Basis=="Assessment",][,5] <- f[f$Year==2019 & f$Basis=="ICES Advice",][,5]
    # DJC f[f$Year==2018 & f$Basis=="Assessment",][,5] <- f[f$Year==2018 & f$Basis=="ICES Advice",][,5]
    #}
    Fmsy <- filter(sbl, var=="Fmsy")[1,5]
    Fpa <- filter(sbl, var=="Fpa")[1,5]
    f1 <- filter(f, Basis %in% c("Assessment", "ICES Advice"))
    f2 <- filter(f, Basis %in% c(input$forecastoptionselection))
    f3 <- rbind(f1,f2) 
    
    # djc 23/11/21 Use a defined ordering for the plots to stop the lines changing color
    myCustomOrder <- unlist(lapply(sbl$Basis, FUN = function(x) switch(x, "Assessment" = 1, "ICES Advice" = 2, "TAC" = 3, 4)))
    myPlotFactorOrder <- unique(sbl[order(myCustomOrder,sbl$Basis),"Basis"])
    f3$Basis <- factor(f3$Basis, levels = myPlotFactorOrder)
    
    p2 <- plot_ly(f3, x = ~Year, y = ~value, type = 'scatter', mode = 'lines', showlegend = F,# linetype = ~Basis,
            color = ~Basis, colors=mypalette, height=375) %>% 
      layout(hovermode="FALSE", #showlegend = TRUE,
             xaxis = list(title = 'Year', range= c(min(f3$Year), max(f3$Year)+1)),
             yaxis = list (title = 'F', range = c(0, max(f3$value, na.rm = T)*1.05)),
             #margin = list(l = 25, r = 25, b = 25, t = 25, pad = 4),
             shapes = list(
               list(type = "rect", fillcolor = "green", opacity = 0.2, 
                    line = list(color = "green", opacity=0.2), x0 = min(sbl$Year), x1 = max(sbl$Year)+1, 
                    xref = "Year", y0 = 0, y1 = Fmsy, yref = "value"),
               list(type = "rect", fillcolor = "orange", opacity = 0.2, 
                    line = list(color = "orange", opacity=0.2), x0 = min(sbl$Year), x1 = max(sbl$Year)+1, 
                    xref = "Year", y0 = Fmsy, y1 = Fpa, yref = "value"),
               list(type = "rect", fillcolor = "red", opacity = 0.2, 
                    line = list(color = "red", opacity=0.2), x0 = min(sbl$Year), x1 = max(sbl$Year)+1, 
                    xref = "Year", y0 = Fpa, y1 =max(f3$value, na.rm=TRUE)*1.05, yref = "value")))
    p2$elementId <- NULL
    p2
  })
  
  output$plotLandings <- renderPlotly({
    # djc 9/11/21 Filtering was previously only done by area description! - Fixed to filter by species and area
    #sbl <- filter(Forecasting, FishStock==paste0(ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"New"]))
    stockToFilter <- ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"New" ]
    if (length(stockToFilter) == 0){
      # djc 9/11/21 Leave the function if we can't find a match - stops error messages
      # when you move between species with different valid areas
      return(NULL)
    }
    sbl <- filter(Forecasting, FishStock==stockToFilter)
    la <- filter(sbl, var %in% c("Landings", "TAC"))
    #print(la)
    yaxislabel="Landings"
    if(dim(la[la$Year==2021 & la$Basis=="Assessment",])[1]==0){
    # SM Oct2021: Changed 2020 to 2021
    # SM Nov2020: Changed 2019 to 2020
    # DJC if(dim(la[la$Year==2018 & la$Basis=="Assessment",])[1]==0){
      la2021= data.frame(FishStock=la[1,1], Year=2021, Basis="Assessment", 
                         var="Landings", value=la[la$Year==2021 & la$Basis=="ICES Advice",][,5])
      la=rbind(la, la2021)
      # SM Oct2021: Changed 2020 to 2021
      # SM Nov2020: Changed 2019 to 2020
      # DJC la2018= data.frame(FishStock=la[1,1], Year=2018, Basis="Assessment", 
      # DJC                   var="Landings", value=la[la$Year==2018 & la$Basis=="ICES Advice",][,5])
      # DJC la=rbind(la, la2018)
    }else{
      la[la$Year==2021 & la$Basis=="Assessment" & la$var =="Landings",][,5] <- la[la$Year==2021 & la$Basis=="F=F2021" & la$var =="Landings",][,5]
      # SM Oct2021: Changed 2020 to 2021
      # SM Nov2020: Changed 2019 to 2020
      # DJC la[la$Year==2018 & la$Basis=="Assessment" & la$var =="Landings",][,5] <- la[la$Year==2018 & la$Basis=="F = F2018" & la$var =="Landings",][,5]
    }
    if(is.na(la[which(la$Basis=="ICES Advice"),"value"])[1]){
      la <- filter(sbl, var %in% c("Catch", "TAC"))
      yaxislabel="Total Catch"
      la[la$Year==2021 & la$Basis=="Assessment" & la$var =="Catch",][,5] <- la[la$Year==2021 & la$Basis=="F=F2021" & la$var =="Catch",][,5]
      # SM Oct2021: Changed 2020 to 2021
      # SM Nov2020: Changed 2019 to 2020
      # DJC la[la$Year==2018 & la$Basis=="Assessment" & la$var =="Catch",][,5] <- la[la$Year==2018 & la$Basis=="F = F2018" & la$var =="Catch",][,5]
    }
    la1 <- filter(la, Basis %in% c("Assessment", "ICES Advice", "TAC"))
    la2 <- filter(la, Basis %in% c(input$forecastoptionselection))
    la3 <- rbind(la1, la2)
    
    # djc 23/11/21 Use a defined ordering for the plots to stop the lines changing color
    myCustomOrder <- unlist(lapply(sbl$Basis, FUN = function(x) switch(x, "Assessment" = 1, "ICES Advice" = 2, "TAC" = 3, 4)))
    myPlotFactorOrder <- unique(sbl[order(myCustomOrder,sbl$Basis),"Basis"])
    la3$Basis <- factor(la3$Basis, levels = myPlotFactorOrder)
    
    p3 <- plot_ly(la3, x = ~Year, y = ~value, type = 'scatter', mode = 'lines', showlegend = T, #linetype = ~factor(var), 
            color = ~Basis, colors=mypalette, height=375) %>% 
      layout(hovermode="FALSE", #showlegend = TRUE,
             xaxis = list(title = 'Year', range= c(min(la3$Year), max(la3$Year)+1)),
             yaxis = list (title = yaxislabel, range = c(0, max(la3$value, na.rm = T)*1.05)))
    p3$elementId <- NULL
    p3
    #subplot(p1, p2, p3)
  })