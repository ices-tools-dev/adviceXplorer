get_catch_scenario_table <- function(stock_name) {
  catch_scenario_list <- jsonlite::fromJSON(
    URLencode(
      # "https://sg.ices.dk/adviceview/API/getAdviceViewRecord?year=2020"
      sprintf("https://sg.ices.dk/adviceview/API/getAdviceViewRecord?stockcode=%s", stock_name)
    )
  )

  catch_scenario_list <- catch_scenario_list %>% filter(adviceViewPublished == TRUE)

  catch_scenario_table <- jsonlite::fromJSON(
    URLencode(
      sprintf("https://sg.ices.dk/adviceview/API/getCatchScenariosTable/%s", catch_scenario_list$adviceKey) # )
    )
  )
  catch_scenario_table <- catch_scenario_table %>%
    pivot_wider(
      names_from = c(aK_ID, aK_Label, yearLabel, unit, stockDataType),
      names_glue = "{aK_Label} ({yearLabel})",
      values_from = value
    ) %>%
    select(-adviceKey,  -aR_ID) #%>%-cS_Basis,
    # by(
    #   .$cS_Purpose,
    #   function(x) {
    #     select(x, -cS_Purpose)
    #   }
    # ) %>%
    # unclass()
  return(catch_scenario_table)
}

standardize_catch_scenario_table <- function(tmp) {
  tmp$Year <- 2022
  ###################################### code tests to try to accept as many catch scen tables headings

  tmp_unified <- data.frame()
  # Year
  pattern <- c("Year")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  tmp_unified <- tmp[, c(subset)]

  # tmp_unified <-unlist(tmp[,c(subset)],use.names = FALSE)
  
  # cS_Label"
  pattern <- c("cS_Label")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  # tmp_unified$cat <- tmp[,c(subset)]
  tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)])

  # cS_Purpose"
  pattern <- c("cS_Purpose")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  # tmp_unified$cat <- tmp[,c(subset)]
  tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)])

  # Ftotal"
  pattern <- c("Ftotal", "F_total", "F total", "Total F", "F age")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  # tmp_unified$F <- tmp[,c(subset)]
  if (!any(subset)) {
      tmp_unified <- tmp_unified %>% add_column(F = NA)
  } else {
      tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)])
  }
  

  # Total catch"
  pattern <- c("Total catch")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  # tmp_unified$TotCatch <- tmp[,c(subset)]
  if (!any(subset)) {
      tmp_unified <- tmp_unified %>% add_column(TotCatch = NA)
  } else {
      tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)])
  }
  

  # % TAC change"
  pattern <- c("% TAC ", "TAC", "TAC ", "% TAC")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  # tmp_unified$TACchange <- tmp[,c(subset)]
  if (!any(subset)) {
      tmp_unified <- tmp_unified %>% add_column(TACchange = NA)
  } else {
      tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)])
  }

  # % Advice change"
  pattern <- c("% Advice change", "Advice change", "% advice change")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  # tmp_unified$ADVICEchange <- tmp[,c(subset)]
  if (!any(subset)) {
      tmp_unified <- tmp_unified %>% add_column(ADVICEchange = NA)
  } else {
      tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)])
  }
  

  # SSB"
  pattern <- c("SSB (2021)")
  subset <- which(names(tmp) == pattern)
  if (length(subset) == 0) {
    pattern <- c("SSB (2020)")
    subset <- which(names(tmp) == pattern)
  }

  if (!any(subset)) {
      tmp_unified <- tmp_unified %>% add_column(SSB = NA)
  } else {
      tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)])
  }
  


  # % SSB change "
  pattern <- c("% SSB change", "SSB change")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
      tmp_unified <- tmp_unified %>% add_column(SSBchange = NA)
  } else {
      tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)])
  }
  
# rename columns to standard names
  colnames(tmp_unified) <- c("Year", "cat", "cS_Purpose", "F", "TotCatch", "TACchange", "ADVICEchange", "SSB", "SSBchange")

#   tmp_unified <- tmp_unified %>% do(bind_rows(., data.frame(Year = 2022, cat = "ref", F = 0, TotCatch = 0, TACchange = 0, ADVICEchange = 0, SSBchange = 0, SSB = 0)))

  return(tmp_unified)
  # tmp3 <- tmp2 %>% relocate("SSB", .before = "SSBchange")
}

##############################################
stocks <- c(
  "cod.27.5a",
  "cod.21.1",
  "cod.27.47d20",
  "had.27.7a",
  "had.27.6b",
  "had.27.7b-k",
  "had.27.46a20",
  "had.27.1-2",
  "her.27.irls",
  "her.27.20-24",
  "her.27.nirs",
  "her.27.3a47d",
  "nop.27.3a4",
  "ple.27.420",
  "ple.27.7d",
  "ple.27.7a",
  "pok.27.3a46",
  "pok.27.1-2",
  "san.sa.1r",
  "san.sa.2r",
  "san.sa.3r",
  "san.sa.4",
  "sol.27.4",
  "spr.27.3a4",
  "tur.27.4",
  "whg.27.47d",
  "wit.27.3a47d"
)
catch_tab <- get_catch_scenario_table("cod.27.47d20")
tibble(catch_tab)
catch_tab_stand <- standardize_catch_scenario_table(catch_tab)
tibble(catch_tab_stand)
names(catch_tab)


not_all_na <- function(x) any(!is.na(x))
catch_tab_stand <- catch_tab_stand %>% select(where(not_all_na))
rescale_function <- function(x) rescale(x, to = c(0, 1), from = range(c(min(x), max(x))))
catch_tab_stand_scaled <- catch_tab_stand %>% select(-Year) %>% mutate_if(is.numeric, rescale_function)
catch_tab_stand_scaled <- catch_tab_stand_scaled %>% relocate("SSB", .before = "SSBchange")
zz <- ggplotly(
        ggradar(catch_tab_stand_scaled %>% select(-cS_Purpose), values.radar = c("0%", "50%", "100%"), axis.label.size = 10, axis.line.colour = "grey", legend.title = "Catch Scenarios:")
    )
    zz
### problem here, some catch tables have 1 or more NAs columns, we could use
    #not_all_na <- function(x) any(!is.na(x))
    #temp %>% select(where(not_all_na))
    # then we need to make the rescale function not variable-name dependent but general.
    # tmp <- catch_tab_stand
    # tmp3 <- tmp %>% mutate(
    #     F = rescale(F, to = c(0, 1), from = range(c(min(F), max(F)))),
    #     SSB = rescale(SSB, to = c(0, 1), from = range(c(min(SSB), max(SSB)))),
    #     TotCatch = rescale(TotCatch, to = c(0, 1), from = range(c(min(TotCatch), max(TotCatch)))),
    #     # TACchange = rescale(TACchange, to = c(0, 1), from = range(c(min(TACchange), max(TACchange)))),
    #     ADVICEchange = rescale(ADVICEchange, to = c(0, 1), from = range(c(min(ADVICEchange), max(ADVICEchange)))),
    #     SSBchange = rescale(SSBchange, to = c(0, 1), from = range(c(min(SSBchange), max(SSBchange)))),
    # )
    # tmp3 <- tmp3 %>% relocate("SSB", .before = "SSBchange")
    
    
    
    # zz <- ggplotly(
    #     ggradar(tmp3 %>% select(-Year), values.radar = c("0%", "50%", "100%"), axis.label.size = 10, axis.line.colour = "grey", legend.title = "Catch Scenarios:")
    # )
    # zz


catch_scenarios_plot2 <- function(tmp) {
    # tmp$Year <- 2022

    # tmp2 <- tmp %>% select(Year, cS_Label, `Ftotal (2020)`, `SSB (2021)`, `Total catch (2020)`, `% TAC change (2020)`, `% Advice change (2020)`, `% SSB change (2021)`)

    # colnames(tmp2) <- c("Year", "cat", "F", "SSB", "TotCatch", "TACchange", "ADVICEchange", "SSBchange")
    # tmp2 <- tmp2 %>% do(bind_rows(., data.frame(Year = 2022, cat = "ref", F = 0, SSB = 0, TotCatch = 0, TACchange = 0, ADVICEchange = 0, SSBchange = 0)))

    # sc <- head(tmp2$cat)
    tmp <- arrange(tmp, F)

    labels <- sprintf(
            "Catch Scenario: %s", tmp$cat
        ) %>% lapply(htmltools::HTML)
    
    F0 <- tmp[tmp$cat == "F = 0", ]
    Basis <- tmp[tmp$cS_Purpose == "BasisAdvice",]

    fig_catch <- plot_ly(tmp) %>%
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
        tickfont = list(color = "#ff7300", size = 20),
        overlaying = "y",
        side = "right",
        title = "<b>SSB</b>",
        titlefont = list(color = "#ff7300", size = 30),
        tickfont = list(size = 30)
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
    
    a <- list(
        x = F0$TotCatch,
        y = F0$F,
        text = F0$cat,
        xref = "x",
        yref = "y",
        showarrow = TRUE,
        arrowhead = 15,
        ax = 10,
        ay = -100,
        font = list(
            color = "#000000",
            family = "sans serif",
            size = 25
        )
    )
    b <- list(
        x = Basis$TotCatch,
        y = Basis$F,
        text = Basis$cS_Purpose,
        xref = "x",
        yref = "y",
        showarrow = TRUE,
        arrowhead = 15,
        ax = 10,
        ay = -100, font = list(
            color = "#000000",
            family = "sans serif",
            size = 25
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
        xaxis = list(title = "<b>Total Catch</b>", titlefont = list(size = 30), tickfont = list(size = 30)),
        yaxis = list(title = "<b>F</b>", titlefont = list(size = 30), tickfont = list(size = 30)) # ,tickfont = list(color = "red", size = 20)
        #   annotations = a
    )
    fig_catch <- fig_catch %>% layout(
        annotations = a
    )
    fig_catch <- fig_catch %>% layout(
        annotations = b
    )
    fig_catch <- fig_catch %>% layout(
      legend = list(font = list(size = 20, color = "#000"), bgcolor = "#ffffff", x = 0.5, y = 1)
    )
    # fig_catch <- fig_catch %>% layout(
    #     annotations = d
    # )
    fig_catch <- fig_catch %>% layout(autosize = T,  margin=list( l = 120, r = 120, b = 120, t = 50,  pad = 4))

    fig_catch
}
tmp <- catch_tab_stand
catch_scenarios_plot2(catch_tab_stand)


# pattern <- c("F = 0")
# subset <- grepl(pattern, tmp$cat)
#   # tmp_unified$F <- tmp[,c(subset)]
#   if (!any(subset)) {
#       tmp_unified <- tmp_unified %>% add_column(F = NA)
#   } else {
#       tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)])
#   }
  