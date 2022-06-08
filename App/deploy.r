# library(rsconnect)
deployApp(
  # appDir = "App",
  appName = "online-single-stock-advice",
  appTitle = "Online single-stock advice",
  forceUpdate = getOption("rsconnect.force.update.apps", TRUE),
  launch.browser = FALSE
)
