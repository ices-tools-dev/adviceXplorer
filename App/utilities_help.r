# help dataframe
help_datatable <- function() {
    data.table(
        tab = c(
            "help_tab1",
            "help_tab1", # 1
            "help_tab1", # 2
            "help_tab1", # 3
            "help_tab1", # 4
            "help_tab1", # 5
            "help_tab1", # 6
            "help_tab1", # 7
            "help_tab1", # 8
            "help_tab2", # 9
            # "help_tab2",
            "help_tab3", # 10
            "help_tab3", # 11
            "help_tab3", # 12
            "help_tab3", # 13
            "help_tab3", # 14
            "help_tab3", # 15
            "help_tab3"  #16
        ),
        step = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17),
        element = c(
            "#tabset",
            "#map1",
            "#selected_locations + .selectize-control",
            "#selected_years + .selectize-control",
            "#my-filters-StockKeyLabel-label",
            "#my-filters-SpeciesCommonName-label",
            "#my-filters-ExpertGroup-label", ##add data category
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
            "filter tab",
            paste0( "<b>","Click on one or more Ecoregions to start filtering the data", "<b/>","<br/>","<br/>","<img src= 'Animation.gif'", " height= '200px'/>"),
            "select ecoregion",
            "select year",
            "select stockcode",
            "common name",
            "Expert group",
            "Year of last assessment",
            "Advice category",
            paste0("<b>", "To select a stock, simply click on its row and move to one of the other tabs on the right", "<b/>", "<br/>", "<br/>","<img src= 'stock_selection.gif'", " height= '200px'/>"),
            "infos",
            "plot catch scen 3",
            "choose scenarios",
            "TAC plot",
            "check the timeline",
            "chatch scenario table",
            "these are the footnotes"
        )
    )
}