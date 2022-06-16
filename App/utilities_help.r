# help dataframe
help_datatable <- function() {
    data.table(
        tab = c(
            "help_tab1",# 1
            "help_tab1",#2
            "help_tab1", #3
            "help_tab1", # 4
            "help_tab1", # 5
            "help_tab1", # 6
            "help_tab1", # 7
            "help_tab1", # 8
            "help_tab1", # 9
            "help_tab1", # 10
            "help_tab1", # 11
            "help_tab2", # 12
            "help_tab3", # 13
            "help_tab3", # 14
            "help_tab3", # 15
            "help_tab3", # 16
            "help_tab3", # 17
            "help_tab3", # 18
            "help_tab3"  #19
        ),
        step = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19),
        element = c(
            "#help_tab1",
            "#tabset",
            "#map1",
            "#selected_locations + .selectize-control",
            "#selected_years + .selectize-control",
            "#my-filters-StockKeyLabel-label",
            "#my-filters-SpeciesCommonName-label",
            "#my-filters-ExpertGroup-label",
            "#my-filters-DataCategory-label",
            "#my-filters-YearOfLastAssessment-label",
            "#my-filters-AdviceCategory-label",
            "#tbl",
            # "#tbl + .tr.odd",
            "#Advice_Sentence2",
            "#catch_scenario_plot_3",
            "#catch_choice-label",
            "#TAC_timeline",
            "#preview",
            "#table",
            "#footnotes"
        ),
        intro = c(
            paste0("<center><img src= 'ICES_logo.png' width= '100%'/></br></br></center><b><font size='5'>Welcome to the beta-version of the single-stock Online Advice!</font></b></br></br><font size='4'>You can get a quick tour of each page by clicking this symbol</font></br></br>" ,"<center><img src= 'info.png' height= '50px'/></center>"),
            paste0("<ul><li><b>Data Filtering:</b> You can filter the data based on different criteria</li>","<li><b>Stock Selection:</b> Select a fish stock and get access to different ICES products</li>","<li><b>Stock Assessment Trends:</b> Check the present and past trends of the fish stock</li>","<li><b>Advice:</b> Check the future projections for the stock</li>","<li><b>Resources:</b> Give us feedback! Abd acess more ICES resources.</li>","</ul>"),
            paste0( "<b>","This map is interactive and connected to the Data filtering panel on the right. Click on one or more Ecoregions to start filtering the data", "<b/>","<br/>","<br/>","<img src= 'Animation.gif'", " height= '150px'/>"),
            "select ecoregion",
            "select year",
            "select stockcode",
            "common name",
            "Expert group",
            "data category",
            "Year of last assessment",
            "Advice category",
            paste0("<b>", "To select a stock, simply click on its row and move to one of the other tabs on the right", "<b/>","<img src= 'stock_selection.gif'", " height= '100px'/>"),
            "infos",
            "plot catch scen 3",
            "choose scenarios",
            "TAC plot",
            "check the timeline",
            "chatch scenario table",
            "these are the footnotes"
        ),
        position = c(
            "left", #1
            "bottom",#2
            "right",#3
            "left",#4
            "left",#5
            "left",#6
            "left",#7
            "left",#8
            "left",#9
            "left",#10
            "left",#11
            "left",#12
            "bottom",#13
            "right",#14
            "top",#15
            "right",#16
            "left",#17
            "left",#18
            "left"  #19
        )
    )
}