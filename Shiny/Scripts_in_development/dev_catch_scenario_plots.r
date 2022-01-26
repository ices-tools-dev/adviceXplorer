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
    select(-adviceKey, -cS_Basis, -aR_ID) #%>%
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
  colnames(tmp_unified) <- c("Year", "cat", "F", "TotCatch", "TACchange", "ADVICEchange", "SSB", "SSBchange")

#   tmp_unified <- tmp_unified %>% do(bind_rows(., data.frame(Year = 2022, cat = "ref", F = 0, TotCatch = 0, TACchange = 0, ADVICEchange = 0, SSBchange = 0, SSB = 0)))

  return(tmp_unified)
  # tmp3 <- tmp2 %>% relocate("SSB", .before = "SSBchange")
}

##############################################
stocks <- c("her.27.irls", "cod.27.5a", "had.27.7a", "ple.27.7a", "had.27.6b", "had.27.7b-k", "cod.21.1", "pok.27.3a46",
"had.27.46a20", "whg.27.47d", "sol.27.4", "san.sa.3r", "her.27.20-24", "her.27.nirs", "her.27.3a47d", "cod.27.47d20", 
"san.sa.1r", "san.sa.2r", "san.sa.4", "spr.27.3a4", "wit.27.3a47d", "tur.27.4", "ple.27.420", "ple.27.7d", "nop.27.3a4",
"had.27.1-2", "pok.27.1-2")
catch_tab <- get_catch_scenario_table("pok.27.1-2")
tibble(catch_tab)
catch_tab_stand <- standardize_catch_scenario_table(catch_tab)
tibble(catch_tab_stand)
names(catch_tab)
