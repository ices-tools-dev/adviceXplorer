# Libraries



library(shiny)







# ## If this code is run for the first time and the SAG data in not present on the local machine
# ## the following line will download the last 5 years of SAG data (summary and ref points).
# ## This process will take several minutes but, once the data is in the local folder, 
# ## the app will run much faster. 
# if (!file.exists("SAG_ 2021/SAG_summary.csv")) {
#     source("Shiny/update_SAG_data.r")
# }

# ui and server
# source("Shiny/ui_05052021.r")
# source("Shiny/server_18052021.r")

### run app
# shinyApp(server = server, ui = ui)


### runApp function (Colin way of running the app which shows the png images in folder www)
runApp()
