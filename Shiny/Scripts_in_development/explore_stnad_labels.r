settings <- icesSAG::getSAGTypeSettings(1)[-4]
graph <- icesSAG::getSAGTypeGraphs(14)
settings %>%
  filter(
    SAGChartKey == 3 & settingKey == 20
  ) %>%
  `[[`("settingDescription")

  assessmentKey <- findAssessmentKey("bss.27.4bc7ad-h", year = 2021)
sumtab <- getSummaryTable(assessmentKey)
customs <- getCustomColumns(assessmentKey)
customs

key <- findAssessmentKey("cod.27.47d20", 2021)
graphs <- getSAGGraphs(key[1])
plot(graphs)
