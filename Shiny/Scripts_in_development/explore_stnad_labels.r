settings <- icesSAG::getSAGTypeSettings(0)[-4]
graph <- icesSAG::getSAGTypeGraphs(4)
settings %>%
  filter(
    SAGChartKey == 3 & settingKey == 20
  ) %>%
  `[[`("settingDescription")

  assessmentKey <- findAssessmentKey("cod-2224", year = 2016)
sumtab <- getSummaryTable(assessmentKey)
