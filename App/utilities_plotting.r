#' This function is used to produce a standardised ICES theme for all SAG and quality of assessement plots.
#' The idea is to have a common base that then can be modified based on the plot formatting options specified in SAG.
#'
#' @param type (specify the type of plot)
#' @param df (SAG data)
#'
#' @return a list to be used in a ggplot function as a theme
#'
#' @note
#' in development
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' theme_ICES_plots(type = "Catches", df)
#' }
#'
#' @references
#'
#'
#'
#' @export
#'
theme_ICES_plots <-
  function(
    type = c("Catches", "Recruitment", "FishingPressure", "StockSize", "quality_SSB", "quality_F", "quality_R"), df,
    title = NULL, ylegend = NULL, ymax = NULL) {
    font <- "Gothic A1, sans-serif"#"Calibri, sans-serif" # assign font family up front
    tmp <- theme_minimal() %+replace% # replace elements we want to change

        theme(
            axis.title = element_text( # axis titles
                family = font, # font family
                size = 18,
                colour = "darkgrey",
                vjust = -2
            ),
            axis.text = element_text( # axis titles
                family = font, # font family
                size = 13,
                colour = "black"
            ),
            axis.title.x = element_blank(),
            
            panel.grid.major.y = element_line(
                colour = "grey",
                size = 1,
                linetype = "solid",
            ),
            plot.title = element_text( # title
                family = font, # set font family
                size = 21, # set font size
                face = "bold", # bold typeface
                hjust = 0, # left align
                vjust = 1,
                margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
                if (type == "Catches") {
                    color <- "#002b5f"
                } else if (type == "Recruitment" | type == "quality_R") {
                    color <- "#28b3e8"
                } else if (type == "FishingPressure" | type == "quality_F") {
                    color <- "#ed5f26"
                } else if (type == "StockSize" | type == "quality_SSB") {
                    color <- "#047c6c"
                }
            ),
            axis.line = element_line(size = 1, colour = "black"),
            axis.ticks = element_line(size = 1, color="black"),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.border = element_rect(
                colour = "black",
                fill = NA,
                size = 0.5
            ),
            legend.text = element_text(
                family = "sans-serif",
                size = 15,
                color = "black"
            ),
            legend.title = element_blank(),
            legend.position = "bottom"

        )

    if (type == "Catches") {

        if (is.null(title)) {
          title <- "Catches"
        }
        if (is.null(ylegend)) {
          ylegend <- sprintf("Catches in 1000 %s", dplyr::last(df$CatchesLandingsUnits))
        }

        if (is.null(ymax)) {
          limits <- expand_limits(y = 0)
        } else {
          limits <- expand_limits(y = c(0, ymax))
        }
        
        theme_ICES_plots <- list(
            tmp,
            labs(
                title = title,
                y = ylegend
            ),
            scale_fill_manual(values = c(
                "Landings" = "#002b5f",
                "Discards" = "#fda500",
                "Catches" = "#002b5f",
                "Industrial Bycatch" = "#00b29d",
                "Unallocated_Removals" = "#6eb200",
                "Down-weighted Catches" = "#6eb5d2"
            )),
            limits,
            scale_y_continuous(
                expand = expansion(mult = c(0, 0.1)),
                labels = function(l) {
                    trans <- l / 1000
                }
            ),
             scale_x_continuous(breaks = breaks_pretty())
        )
    } else if (type == "Recruitment") {

        if (is.null(title)) {
          title <- sprintf("Recruitment <sub>(age %s)</sub>", dplyr::last(df$RecruitmentAge))
        }
        if (is.null(ylegend)) {
          ylegend <- "Recruitment in billions"
        }

        theme_ICES_plots <- list(
            tmp,
            labs(
                title = title,
                y = ylegend
            ),
            scale_fill_manual(values = c("Recruitment" = "#28b3e8")),
            scale_y_continuous(
                expand = expansion(mult = c(0, 0.1)),
                labels = function(l) {
                    trans <- l / 1000000 #### need to work on this
                }
            ),
             scale_x_continuous(breaks = breaks_pretty())
        )
    } else if (type == "FishingPressure") {
        if (is.null(title)) {
          title <- "Fishing pressure"
        }
        if (is.null(ylegend)) {
          ylegend <- sprintf("%s <sub>(ages %s)</sub>", dplyr::last(df$FishingPressureDescription), dplyr::last(df$FAge))
        }

        theme_ICES_plots <- list(
            tmp,
            labs(
                title = title, #"Fishing pressure", # sprintf("Recruitment <sub>(age %s)</sub>", dplyr::last(df$RecruitmentAge)),
                y = ylegend, #sprintf("%s <sub>(ages %s)</sub>", dplyr::last(df$FishingPressureDescription), dplyr::last(df$FAge)), # sprintf("Catches in 1000 %s", dplyr::last(df$Units))
                x = "Year"
            ),
            scale_color_manual(values = c(
                "FishingPressure" = "#ed5f26",
                "F<sub>MSY</sub>" = "#00AC67",
                "F<sub>Lim</sub>" = "#000000",
                "F<sub>pa</sub>" = "#000000",
                "HR MSY<sub>proxy</sub>" = "#00AC67",
                "FMSY<sub>proxy</sub>" = "#00AC67",
                "F<sub>management</sub>" = "#00AC67"

            )),
            scale_linetype_manual(values = c(
                "FishingPressure" = "solid",
                "F<sub>Lim</sub>" = "dashed",
                "F<sub>pa</sub>" = "dotted",
                "F<sub>MSY</sub>" = "solid",
                "HR MSY<sub>proxy</sub>" = "dotdash",
                "FMSY<sub>proxy</sub>" = "dotdash",
                "F<sub>management</sub>" = "dotdash"
            )),
            scale_size_manual(values = c(
                "FishingPressure" = 1.5,
                "F<sub>Lim</sub>" = .8,
                "F<sub>pa</sub>" = 1,
                "F<sub>MSY</sub>" = .5,
                "HR MSY<sub>proxy</sub>" = .8,
                "FMSY<sub>proxy</sub>" = .8,
                "F<sub>management</sub>" = .8
            )),
            scale_fill_manual(values = c("#f2a497")),
            expand_limits(y = 0),
            scale_y_continuous(
                expand = expansion(mult = c(0, 0.1)) # ,
            ),
            scale_x_continuous(breaks = breaks_pretty())
        )
    } else if (type == "StockSize") {
        if (is.null(title)) {
          title <- "Spawning Stock Biomass"
        }
        if (is.null(ylegend)) {
          ylegend <- sprintf("%s in 1000 %s", dplyr::last(df$StockSizeDescription), dplyr::last(df$StockSizeUnits))
          ylabels_func <- function(l) {
            trans <- l / 1000 #1000000
          }
        } else {
          if (is.na(ylegend)) ylegend <- ""
          ylabels_func <- function(l) {
            trans <- l
          }
        }

        if (is.null(ymax)) {
          limits <- expand_limits(y = 0)
        } else {
          limits <- expand_limits(y = c(0, ymax))
        }

        theme_ICES_plots <- list(
            tmp,
            labs(
                title = title, 
                y = ylegend,
                x = "Year"
            ),
            scale_color_manual(values = c(
                "StockSize" = "#047c6c",
                "MSY B<sub>trigger</sub>" = "#689dff",
                "B<sub>Lim</sub>" = "#000000",
                "B<sub>pa</sub>" = "#000000",
                "Average" = "#ed5f26",
                "I<sub>trigger</sub>" = "#689dff",
                "BMGT<sub>lower</sub>" = "#000000",
                "BMGT<sub>upper</sub>" = "#689dff"
            )),
            scale_linetype_manual(values = c(
                "StockSize" = "solid",
                "B<sub>Lim</sub>" = "dashed",
                "B<sub>pa</sub>" = "dotted",
                "MSY B<sub>trigger</sub>" = "solid",
                "Average" = "solid",
                "I<sub>trigger</sub>" = "dotdash",
                "BMGT<sub>lower</sub>" = "dotted",
                "BMGT<sub>upper</sub>" = "dotdash"
            )),
            scale_size_manual(values = c(
                "StockSize" = 1.5,
                "B<sub>Lim</sub>" = .8,
                "B<sub>pa</sub>" = 1,
                "MSY B<sub>trigger</sub>" = .5,
                "Average" = .8,
                "I<sub>trigger</sub>" = .8,
                "BMGT<sub>lower</sub>" = .8,
                "BMGT<sub>upper</sub>" = .8                
            )),
            scale_fill_manual(values = c("#94b0a9")),
            limits,
            scale_y_continuous(
                expand = expansion(mult = c(0, 0.1)),
                labels = function(l) {
                    trans <- l / 1000
                }
            ),
             scale_x_continuous(breaks = breaks_pretty())
        )
    } else if (type == "quality_SSB") {

        if (is.null(title)) {
          title <- sprintf("%s in 1000 %s", dplyr::last(df$StockSizeDescription), dplyr::last(df$StockSizeUnits))
        }

        rfpt <- c( "B<sub>Lim</sub>", "B<sub>pa</sub>","MSY B<sub>trigger</sub>")

        line_color <- c("#969696","#737373","#525252","#252525","#047c6c") %>% tail(length(unique(df$AssessmentYear)))
        names(line_color) <- as.character(sort(unique(df$AssessmentYear)))
        line_color_rfpt <- c( "#000000","#000000", "#689dff")
        names(line_color_rfpt) <- rfpt
        line_color <- append(line_color, line_color_rfpt)

        line_type <- sapply(as.character(sort(unique(df$AssessmentYear))), function(x) "solid")
        line_type_rfpt <- c("dashed", "dotted","solid")
        names(line_type_rfpt) <- rfpt
        line_type <- append(line_type, line_type_rfpt)

        line_size <- sapply(as.character(sort(unique(df$AssessmentYear))), function(x) 1)
        line_size_rfpt <- c( .8, 1, .5)
        names(line_size_rfpt) <- rfpt
        line_size <- append(line_size, line_size_rfpt)
        

        theme_ICES_plots <- list(
            tmp,
            labs(
                title = title,
                y = "",
                x = ""
            ),
            scale_color_manual(values = line_color
            ),
            scale_linetype_manual(values = line_type
            ),
            scale_size_manual(values = line_size
            ),
            expand_limits(y = 0),
            scale_y_continuous(
                expand = expansion(mult = c(0, 0.1)),
                labels = function(l) {
                    trans <- l / 1000
                }
            ),
            scale_x_continuous(breaks= pretty_breaks())

        )
    } else if (type == "quality_F") {
        rfpt <- c( "F<sub>Lim</sub>","F<sub>pa</sub>", "F<sub>MSY</sub>")

        line_color <- c("#969696","#737373","#525252","#252525","#ed5f26") %>% tail(length(unique(df$AssessmentYear)))
        names(line_color) <- as.character(sort(unique(df$AssessmentYear)))
        line_color_rfpt <- c( "#000000","#000000", "#00AC67")
        names(line_color_rfpt) <- rfpt
        line_color <- append(line_color, line_color_rfpt)

        line_type <- sapply(as.character(sort(unique(df$AssessmentYear))), function(x) "solid")
        line_type_rfpt <- c("dashed", "dotted","solid")
        names(line_type_rfpt) <- rfpt
        line_type <- append(line_type, line_type_rfpt)

        line_size <- sapply(as.character(sort(unique(df$AssessmentYear))), function(x) 1)
        line_size_rfpt <- c( .8, 1, .5)
        names(line_size_rfpt) <- rfpt
        line_size <- append(line_size, line_size_rfpt)

        if (is.null(title)) {
          title <- sprintf("%s <sub>(ages %s)</sub>", dplyr::last(df$FishingPressureDescription), dplyr::last(df$FAge))
        }

        theme_ICES_plots <- list(
            tmp,
            labs(
                title = title,
                y = "",
                x = "Year"
            ),
            scale_color_manual(values = line_color
            ),
            scale_linetype_manual(values = line_type
            ),
            scale_size_manual(values = line_size
            ),
            expand_limits(y = 0),
            scale_y_continuous(
                expand = expansion(mult = c(0, 0.1))

            ),
            scale_x_continuous(breaks= pretty_breaks())
        )
    } else if (type == "quality_R") {
        line_type <- sapply(as.character(sort(unique(df$AssessmentYear))), function(x) "solid")
        line_size <- sapply(as.character(sort(unique(df$AssessmentYear))), function(x) 1)
        line_color <- c("#969696","#737373","#525252","#252525","#28b3e8") %>% tail(length(unique(df$AssessmentYear)))
        names(line_color) <- as.character(sort(unique(df$AssessmentYear)))
        
        if (is.null(title)) {
          title <- sprintf("Rec <sub>(age %s)</sub> (Billions)", dplyr::last(df$RecruitmentAge))
        }
        theme_ICES_plots <- list(
            tmp,
            labs(
                title = title,
                y = "",
                x = ""
            ),
            scale_color_manual(values = line_color
            ),
            scale_linetype_manual(values = line_type
            ),
            scale_size_manual(values = line_size
            ),
            expand_limits(y = 0),
            scale_y_continuous(
                expand = expansion(mult = c(0, 0.1)),
                labels = function(l) {
                    trans <- l / 1000000
                }
            ),
            scale_x_continuous(breaks= pretty_breaks())
        )
    }

    return(theme_ICES_plots)
}

#' Function to create a data download button for the plotly options' bar
#'
#'
#' @return 
#'
#' @note
#'
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' }
#'
#' @references
#'
#'
#' @export
#' 
#' 
disclaimer <- " Find disclaimer at https://raw.githubusercontent.com/ices-tools-prod/disclaimers/master/Disclaimer_adviceXplorer.txt"
# data_download_button <- function(){

#     icon_svg_path = "M15.608,6.262h-2.338v0.935h2.338c0.516,0,0.934,0.418,0.934,0.935v8.879c0,0.517-0.418,0.935-0.934,0.935H4.392c-0.516,0-0.935-0.418-0.935-0.935V8.131c0-0.516,0.419-0.935,0.935-0.935h2.336V6.262H4.392c-1.032,0-1.869,0.837-1.869,1.869v8.879c0,1.031,0.837,1.869,1.869,1.869h11.216c1.031,0,1.869-0.838,1.869-1.869V8.131C17.478,7.099,16.64,6.262,15.608,6.262z M9.513,11.973c0.017,0.082,0.047,0.162,0.109,0.226c0.104,0.106,0.243,0.143,0.378,0.126c0.135,0.017,0.274-0.02,0.377-0.126c0.064-0.065,0.097-0.147,0.115-0.231l1.708-1.751c0.178-0.183,0.178-0.479,0-0.662c-0.178-0.182-0.467-0.182-0.645,0l-1.101,1.129V1.588c0-0.258-0.204-0.467-0.456-0.467c-0.252,0-0.456,0.209-0.456,0.467v9.094L8.443,9.553c-0.178-0.182-0.467-0.182-0.645,0c-0.178,0.184-0.178,0.479,0,0.662L9.513,11.973z"

#     dl_button <- list(
#         name = "Download data",
#         icon = list(
#             path = icon_svg_path,
#             transform = "scale(0.84) translate(-1, -1)"
#             ),
#         click = htmlwidgets::JS("
#             function(gd) {
#                 var text = '';
#                 for(var i = 0; i < gd.data.length; i++){
#                 text += gd.layout.xaxis.title.text + gd.data[i].name + ',' + gd.data[i].x + '\\n';
#                 text += gd.layout.yaxis.title.text + gd.data[i].name + ',' + gd.data[i].y + '\\n';
#                 };
#                 var blob = new Blob([text], {type: 'text/plain'});
#                 var a = document.createElement('a');
#                 const object_URL = URL.createObjectURL(blob);
#                 a.href = object_URL;
#                 a.download = 'data.csv';
#                 document.body.appendChild(a);
#                 a.click();
#                 URL.revokeObjectURL(object_URL);
#             }
#     ")
#     )
# return(dl_button)
# }
data_download_button <- function(disclaimer_text) {

    icon_svg_path = "M15.608,6.262h-2.338v0.935h2.338c0.516,0,0.934,0.418,0.934,0.935v8.879c0,0.517-0.418,0.935-0.934,0.935H4.392c-0.516,0-0.935-0.418-0.935-0.935V8.131c0-0.516,0.419-0.935,0.935-0.935h2.336V6.262H4.392c-1.032,0-1.869,0.837-1.869,1.869v8.879c0,1.031,0.837,1.869,1.869,1.869h11.216c1.031,0,1.869-0.838,1.869-1.869V8.131C17.478,7.099,16.64,6.262,15.608,6.262z M9.513,11.973c0.017,0.082,0.047,0.162,0.109,0.226c0.104,0.106,0.243,0.143,0.378,0.126c0.135,0.017,0.274-0.02,0.377-0.126c0.064-0.065,0.097-0.147,0.115-0.231l1.708-1.751c0.178-0.183,0.178-0.479,0-0.662c-0.178-0.182-0.467-0.182-0.645,0l-1.101,1.129V1.588c0-0.258-0.204-0.467-0.456-0.467c-0.252,0-0.456,0.209-0.456,0.467v9.094L8.443,9.553c-0.178-0.182-0.467-0.182-0.645,0c-0.178,0.184-0.178,0.479,0,0.662L9.513,11.973z"

    dl_button <- list(
        name = "Download data",
        icon = list(
            path = icon_svg_path,
            transform = "scale(0.84) translate(-1, -1)"
        ),
        click = htmlwidgets::JS(paste0("
            function(gd) {
                console.log(gd.data);
                var text = '';
                for(var i = 0; i < gd.data.length; i++) {
                    text += gd.layout.xaxis.title.text + gd.data[i].name + ',' + gd.data[i].x + '\\n';
                    text += gd.layout.yaxis.title.text + gd.data[i].name + ',' + gd.data[i].y + '\\n';
                };

                // Add the disclaimer to the text
                text += '\\nDisclaimer: ' + '", disclaimer_text, "' + '\\n';

                var blob = new Blob([text], {type: 'text/plain'});
                var a = document.createElement('a');
                const object_URL = URL.createObjectURL(blob);
                a.href = object_URL;
                a.download = 'data.csv';
                document.body.appendChild(a);
                a.click();
                URL.revokeObjectURL(object_URL);
            }
        "))
    )

    return(dl_button)
}



replace_subscript_symbols <- function(text) {
  # Check if text is empty or character(0)
  if (length(text) == 0 || is.na(text)) {
    return(text)
  }
  
  # Check if both "_{" and "}" are present in the string
  if (grepl("_\\{", text) && grepl("\\}", text)) {
    # Replace "_{" with "<sub>" and "}" with "</sub>"
    text <- gsub("_\\{", "<sub>", text)
    text <- gsub("\\}", "</sub>", text)
  }
  return(text)
}
#' Function to plot Landings ans Discards
#'
#' @param df (SAG data)
#'
#' @return a ggplotly object
#'
#' @note
#'
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' ICES_plot_1(df)
#' }
#'
#' @references
#'https://www.ices.dk/data/assessment-tools/Pages/stock-assessment-graphs.aspx
#'
#'
#' @export
#'
ICES_plot_1 <- function(df, sagSettings) {
                # function(df, sagSettings, additional_LandingData) {
    sagSettings1 <- sagSettings %>% filter(SAGChartKey == 1)

    # df <- df %>% left_join(y = additional_LandingData, by = "Year")

    df1 <- df %>%
        filter(Purpose == "Advice") %>%
        select(Year, Landings, Catches, Discards, CatchesLandingsUnits, SAGStamp, IBC, Unallocated_Removals) %>%
        relocate(c(IBC, Unallocated_Removals), .after = Discards) %>%
        rename("Industrial Bycatch" = IBC) 

    shadeYears <- sagSettings1 %>%
        filter(settingKey == 14) %>%
        pull(settingValue) %>%
        str_split(pattern = ",", simplify = TRUE) %>%
        as.numeric()
    
    # Function to check if a column is made up of all NA values
    is_na_column <- function(dataframe, col_name) {
        return(all(is.na(dataframe[, ..col_name])))
    }

    if (is_na_column(df,"Landings")){
        # df1$Landings <- df1$Catches
        df1 <- df1 %>%
        gather(type, count, Catches:Unallocated_Removals)
    } else {
        df1 <- df1 %>%
        select(-Catches) %>% 
        gather(type, count, Landings:Unallocated_Removals)
    }
    

    p1 <- df1 %>%
        ggplot(., aes(
            x = Year,
            y = count,
            fill = type,
            text = map(
                paste0(
                    "<b>Year: </b>", Year,
                    "<br>",
                    "<b>", type, ": </b>", count
                ), HTML
            )
        )) +
        geom_bar(position = "stack", stat = "identity", data = df1 %>% filter(!Year %in% shadeYears))
    
    if (any(!is.na(shadeYears))) {
        p1 <- p1 + geom_bar(stat = "identity", 
                            data = df1 %>% filter(Year %in% shadeYears), 
                            aes(x = Year, 
                            y = count, 
                            fill = "Down-weighted Catches",
                            text = map(
                                    paste0(
                                        "<b>Year: </b>", Year,
                                        "<br>",
                                        "<b>Down-weighted or preliminary Catches: </b>", count
                                    ), HTML
                                )),
                            alpha = 0.5, 
                            show.legend = FALSE, 
                            inherit.aes = FALSE)
    }



    nullifempty <- function(x) if (length(x) == 0) NULL else x
    
    p1 <-
        p1 +
        theme_ICES_plots(
            type = "Catches", df,
            title = sagSettings1 %>% filter(settingKey == 1) %>% pull(settingValue) %>% nullifempty(),
            ylegend = sagSettings1 %>% filter(settingKey == 20) %>% pull(settingValue) %>% as.character() %>% nullifempty(),
        
        )


    # converting
    fig1 <- ggplotly(p1, tooltip = "text") %>%
        layout(
            legend = list(
                orientation = "h",
                y = -.3,
                yanchor = "bottom",
                x = 0.5,
                xanchor = "center",
                title = list(text = "")
            ),
            annotations = list(
                showarrow = FALSE,
                text = tail(df$SAGStamp, 1),
                font = list(family = "Calibri, serif", size = 12, color = "#acacac"),
                yref = "paper", y = 1, xref = "paper", x = 1,
                yanchor = "right", xanchor = "right"
            )
        ) #%>% 
        #config(modeBarButtonsToAdd = list(data_download_button(disclaimer)))
    fig1
}


#' Function to plot Recruitment
#'
#' @param df (SAG data)
#'
#' @return a ggplotly object
#'
#' @note
#'
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' ICES_plot_2(df)
#' }
#'
#' @references
#'https://www.ices.dk/data/assessment-tools/Pages/stock-assessment-graphs.aspx
#'
#'
#' @export
#'
ICES_plot_2 <- function(df, sagSettings) {
    df2 <- df %>%
        filter(Purpose == "Advice") %>%
        select(Year, Recruitment, Low_Recruitment, High_Recruitment, UnitOfRecruitment, RecruitmentAge, SAGStamp)

    sagSettings2 <- sagSettings %>% filter(SAGChartKey == 2)

    xmax <- sagSettings2 %>%
        filter(settingKey == 5) %>%
        pull(settingValue) %>%
        as.numeric()
    
    
    if (any(!is.na(xmax))) {
        df2 <- df2 %>%
            filter(Year != xmax + 1)
    }

    shadeYears <- sagSettings2 %>%
        filter(settingKey == 14) %>%
        pull(settingValue) %>%
        str_split(pattern = ",", simplify = TRUE) %>%
        as.numeric()


    p2 <- df2 %>%
        ggplot(., aes(
            x = Year, y = Recruitment,
            fill = "Recruitment",
            text = map(
                paste0(
                    "<b>Year: </b>", Year,
                    "<br>",
                    "<b>Recruitment: </b>", Recruitment
                ), HTML
            )
        )
        ) +
        geom_bar(stat = "identity", data = df2 %>% filter(!Year %in% shadeYears))


    if (any(!is.na(df2$High_Recruitment))) {
        p2 <- p2 +
            geom_errorbar(
                data = df2 %>% filter(!is.na(High_Recruitment)),
                aes(
                    ymin = Low_Recruitment,
                    ymax = High_Recruitment,
                    text = map(
                        paste0(
                            "<b>Year: </b>", Year,
                            "<br>",
                            "<b>High Recruitment: </b>", High_Recruitment,
                            "<br>",
                            "<b>Low Recruitment: </b>", Low_Recruitment
                        ), HTML
                    )
                ),
                width = .3
            )
    }

    if (any(!is.na(shadeYears))) {
        p2 <- p2 + geom_bar(stat = "identity", 
                            data = df2 %>% filter(Year %in% shadeYears), 
                            aes(x = Year, 
                            y = Recruitment, 
                            fill = "Recruitment",
                            text = map(
                                    paste0(
                                        "<b>Year: </b>", Year,
                                        "<br>",
                                        "<b>Assumed Recruitment: </b>", Recruitment
                                    ), HTML
                                )),
                            alpha = 0.5, 
                            show.legend = FALSE, 
                            inherit.aes = FALSE)
    }

    nullifempty <- function(x) if (length(x) == 0) NULL else x
    p2 <-
        p2 +
        theme_ICES_plots(
            type = "Recruitment", df,
            title = sagSettings2 %>% filter(settingKey == 1) %>% pull(settingValue) %>% nullifempty(),
            ylegend = sagSettings2 %>% filter(settingKey == 20) %>% pull(settingValue) %>% nullifempty()
        )

    fig2 <- ggplotly(p2, tooltip = "text") %>%
        layout(
            legend = list(
                orientation = "h",
                y = -.3,
                yanchor = "bottom",
                x = 0.5,
                xanchor = "center",
                title = list(text = "")
            ),
            annotations = list(
                showarrow = FALSE,
                text = tail(df$SAGStamp, 1),
                font = list(family = "Calibri, serif", size = 12, color = "#acacac"),
                yref = "paper", y = 1, xref = "paper", x = 1,
                yanchor = "right", xanchor = "right"
            )
        ) #%>%
        #config(modeBarButtonsToAdd = list(data_download_button(disclaimer)))
    fig2
}



#' Function to plot fishing pressure (F)
#'
#' @param df (SAG data)
#'
#' @return a ggplotly object
#'
#' @note
#'
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' ICES_plot_3(df)
#' }
#'
#' @references
#'https://www.ices.dk/data/assessment-tools/Pages/stock-assessment-graphs.aspx
#'
#'
#' @export
#'
ICES_plot_3 <- function(df, sagSettings) {
    sagSettings3 <- sagSettings %>% filter(SAGChartKey == 3)
    
    customRefPoint <-
        sagSettings3 %>%
        filter(settingKey == 51) %>%
        pull(settingValue) %>%
        standardiseRefPoints(.) %>%
        str_split(pattern = ",", simplify = TRUE)
        # as.numeric()
    
    df3 <- df %>%
        filter(Purpose == "Advice") %>%
        select(
            c(Year, FishingPressure, Low_FishingPressure, High_FishingPressure, Flim, Fpa, FMSY, FAge, Fmanagement, FishingPressureDescription, SAGStamp, ConfidenceIntervalDefinition, FMGT_lower, FMGT_upper),
            if (length(customRefPoint) != 0 && !all(customRefPoint %in% colnames(.))) c(paste0("CustomRefPointValue", customRefPoint), paste0("CustomRefPointName", customRefPoint))
        ) %>%
        mutate(segment = cumsum(is.na(FishingPressure)))

    
    
    # Filter out rows with NAs and create a segment identifier
    df_segments <- df3 %>%
        filter(!is.na(FishingPressure)) %>%
        group_by(segment) %>%
        mutate(start = first(Year), end = last(Year))


    p3 <- df_segments %>%
        ggplot(., aes(x = Year, y = FishingPressure))
    
    if (any(!is.na(df_segments$Low_FishingPressure))) {
        p3 <- p3 +
            geom_ribbon(
                aes(
                    ymin = Low_FishingPressure,
                    ymax = High_FishingPressure,
                    fill = ConfidenceIntervalDefinition,
                    group = segment,
                    text = map(
                        paste0(
                            "<b>Year: </b>", Year,
                            "<br>",
                            "<b>", FishingPressureDescription, ": </b>", FishingPressure,
                            "<br>",
                            "<b>High ", FishingPressureDescription," : </b>", High_FishingPressure,
                            "<br>",
                            "<b>Low ", FishingPressureDescription," : </b>", Low_FishingPressure
                        ), HTML
                    )
                ),
                linetype = "blank",
                size = 0
            )
    }

    p3 <- p3 +
        geom_line(aes(
            x = Year,
            y = FishingPressure,
            color = "FishingPressure",
            group = segment,
            text = map(
                paste0(
                    "<b>Year: </b>", Year,
                    "<br>",
                    "<b>", FishingPressureDescription, ": </b>", FishingPressure
                ), HTML
            )
        ))

    if (any(!is.na(df_segments$FMSY))) {
        p3 <- p3 +
            geom_line(aes(
                x = Year,
                y = FMSY,
                linetype = "F<sub>MSY</sub>",
                colour = "F<sub>MSY</sub>",
                size = "F<sub>MSY</sub>",
                text = map(
                    paste0(
                        "<b>F<sub>MSY</sub>: </b>", tail(FMSY, 1)
                    ), HTML
                )
            ))
    }

    if (any(!is.na(df_segments$Flim))) {
        p3 <- p3 +
            geom_line(aes(
                x = Year,
                y = Flim,
                linetype = "F<sub>Lim</sub>",
                colour = "F<sub>Lim</sub>",
                size = "F<sub>Lim</sub>",
                text = map(
                    paste0(
                        "<b>F<sub>Lim</sub>: </b>", tail(Flim, 1)
                    ), HTML
                )
            ))
    }

    if (any(!is.na(df_segments$Fpa))) {
        p3 <- p3 +
            geom_line(aes(
                x = Year,
                y = Fpa,
                linetype = "F<sub>pa</sub>",
                colour = "F<sub>pa</sub>",
                size = "F<sub>pa</sub>",
                text = map(
                    paste0(
                        "<b>F<sub>pa</sub>: </b>", tail(Fpa, 1)
                    ), HTML
                )
            ))
    }

    if (any(!is.na(df_segments$Fmanagement))) {
        p3 <- p3 +
            geom_line(aes(
                x = Year,
                y = Fmanagement,
                linetype = "F<sub>management</sub>",
                colour = "F<sub>management</sub>",
                size = "F<sub>management</sub>",
                text = map(
                    paste0(
                        "<b>F<sub>management</sub>: </b>", tail(Fmanagement, 1)
                    ), HTML
                )
            ))
    }

    #### custom reference points
    if (any(!is.na(df_segments[[paste0("CustomRefPointValue", customRefPoint)]])) && !customRefPoint %in% colnames(df)) {
        p3 <- p3 +
            geom_line(aes(
                x = Year,
                y = df_segments[[paste0("CustomRefPointValue", customRefPoint)]],
                linetype = df_segments[[paste0("CustomRefPointName", customRefPoint)]][1],
                colour = df_segments[[paste0("CustomRefPointName", customRefPoint)]][1],
                size = df_segments[[paste0("CustomRefPointName", customRefPoint)]][1],
                text = map(
                    paste0(
                        "<b>", df_segments[[paste0("CustomRefPointName", customRefPoint)]][1], ": </b>", tail(df_segments[[paste0("CustomRefPointValue", customRefPoint)]], 1)
                    ), HTML
                )
            ))
    }

    min_year <- min(df_segments$Year[which(!is.na(df_segments$FishingPressure))])
    nullifempty <- function(x) if (length(x) == 0) NULL else x

    p3 <-
        p3 +
        xlim(min_year, max(df_segments$Year)) +
        theme_ICES_plots(
            type = "FishingPressure", df_segments,
            title = sagSettings3 %>% filter(settingKey == 1) %>% pull(settingValue) %>% nullifempty(),
            ylegend = sagSettings3 %>% filter(settingKey == 20) %>% pull(settingValue) %>% replace_subscript_symbols(.)  %>% nullifempty()
        )



    fig3 <- ggplotly(p3, tooltip = "text") %>%
        layout(
            legend = list(
                orientation = "h",
                itemwidth = 20,
                itemsizing = "trace",
                y = -.3, yanchor = "bottom",
                x = 0.5, xanchor = "center",
                title = list(text = ""),
                yref = "container", xref = "container" # x=0.5, y=1.1, xanchor='center', yanchor='top'
            ),
            xaxis = list(zeroline = TRUE),
            annotations = list(
                showarrow = FALSE,
                text = tail(df$SAGStamp, 1),
                font = list(family = "Calibri, serif", size = 12, color = "#acacac"),
                yref = "paper", y = 1, xref = "paper", x = 1,
                yanchor = "right", xanchor = "right"
            )
        ) #%>% 
        #style(name = "test", name = list(name = "F")) 


    for (i in 1:length(fig3$x$data)) {
        if (!is.null(fig3$x$data[[i]]$name)) {
            fig3$x$data[[i]]$name <- gsub("\\(", "", str_split(fig3$x$data[[i]]$name, ",")[[1]][1])
            if (fig3$x$data[[i]]$name == "FishingPressure") {
                fig3$x$data[[i]]$name <- df_segments$FishingPressureDescription[1]
            }
        }
    }
    
    fig3
}

#' Function to plot spawning stock biomass (StockSize)
#'
#' @param df (SAG data)
#'
#' @return a ggplotly object
#'
#' @note
#'
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' ICES_plot_4(df)
#' }
#'
#' @references
#'https://www.ices.dk/data/assessment-tools/Pages/stock-assessment-graphs.aspx
#'
#'
#' @export
#'
ICES_plot_4 <- function(df, sagSettings) {
    sagSettings4 <- sagSettings %>% filter(SAGChartKey == 4)

    customRefPoint <-
        sagSettings4 %>%
        filter(settingKey == 51) %>%
        pull(settingValue) %>%
        str_split(pattern = ",", simplify = TRUE)
    # as.numeric()
    
    df4 <- df %>%
        filter(Purpose == "Advice") %>%
        select(
            c(Year, Low_StockSize, StockSize, High_StockSize, Blim, Bpa, MSYBtrigger, StockSizeDescription, StockSizeUnits, SAGStamp, ConfidenceIntervalDefinition, BMGT_lower, BMGT_upper),
            if (length(customRefPoint) != 0 && !all(customRefPoint %in% colnames(.))) c(paste0("CustomRefPointValue", customRefPoint), paste0("CustomRefPointName", customRefPoint))
        ) %>%
        mutate(segment = cumsum(is.na(StockSize)))


    # Filter out rows with NAs and create a segment identifier
    df_segments <- df4 %>%
        filter(!is.na(StockSize)) %>%
        group_by(segment) %>%
        mutate(start = first(Year), end = last(Year))


    p4 <- df_segments %>%
        ggplot(., aes(x = Year, y = StockSize))
    
    if (any(!is.na(df_segments$Low_StockSize))) {
        p4 <- p4 +
            geom_ribbon(
                data = df_segments %>% filter(!is.na(Low_StockSize) & !is.na(High_StockSize)), aes(
                    ymin = Low_StockSize,
                    ymax = High_StockSize ,
                    fill = ConfidenceIntervalDefinition,
                    group = segment,
                    text = map(
                        paste0(
                            "<b>Year: </b>", Year,
                            "<br>",
                            "<b>", StockSizeDescription,": </b>", StockSize,
                            "<br>",
                            "<b>High ", StockSizeDescription,": </b>", High_StockSize,
                            "<br>",
                            "<b>Low", StockSizeDescription,": </b>", Low_StockSize
                        ), HTML
                    )
                ),
                linetype = "blank",
                size = 0
            )
    }

    p4 <- p4 +
        geom_line(data = df_segments, aes(
            x = Year,
            y = StockSize,
            color = "StockSize",
            group = segment,
            text = map(
                paste0(
                    "<b>Year: </b>", Year,
                    "<br>",
                    "<b>", StockSizeDescription,": </b>", StockSize
                ), HTML
            )
        ))


    if (any(!is.na(df_segments$MSYBtrigger))) {
        p4 <- p4 +
            geom_line(aes(
                x = Year,
                y = MSYBtrigger,
                linetype = "MSY B<sub>trigger</sub>",
                colour = "MSY B<sub>trigger</sub>",
                size = "MSY B<sub>trigger</sub>",
                text = map(
                    paste0(
                        "<b>MSY B<sub>trigger</sub>: </b>", tail(MSYBtrigger, 1)
                    ), HTML
                )
            ))
    }

    if (any(!is.na(df_segments$Blim))) {
        p4 <- p4 +
            geom_line(aes(
                x = Year,
                y = Blim,
                linetype = "B<sub>Lim</sub>",
                colour = "B<sub>Lim</sub>",
                size = "B<sub>Lim</sub>",
                text = map(
                    paste0(
                        "<b>B<sub>Lim</sub>: </b>", tail(Blim, 1)
                    ), HTML
                )
            ))
    }

    if (any(!is.na(df_segments$Bpa))) {
        p4 <- p4 +
            geom_line(aes(
                x = Year,
                y = Bpa,
                linetype = "B<sub>pa</sub>",
                colour = "B<sub>pa</sub>",
                size = "B<sub>pa</sub>",
                text = map(
                    paste0(
                        "<b>B<sub>pa</sub>: </b>", tail(Bpa, 1)
                    ), HTML
                )
            ))
    }

    if (any(!is.na(df_segments$BMGT_lower))) {
        p4 <- p4 +
            geom_line(aes(
                x = Year,
                y = BMGT_lower,
                linetype = "BMGT<sub>lower</sub>",
                colour = "BMGT<sub>lower</sub>",
                size = "BMGT<sub>lower</sub>",
                text = map(
                    paste0(
                        "<b>BMGT<sub>lower</sub>: </b>", tail(BMGT_lower, 1)
                    ), HTML
                )
            ))
    }

    if (any(!is.na(df_segments$BMGT_upper))) {
        p4 <- p4 +
            geom_line(aes(
                x = Year,
                y = BMGT_upper,
                linetype = "BMGT<sub>upper</sub>",
                colour = "BMGT<sub>upper</sub>",
                size = "BMGT<sub>upper</sub>",
                text = map(
                    paste0(
                        "<b>BMGT<sub>upper</sub>: </b>", tail(BMGT_upper, 1)
                    ), HTML
                )
            ))
    }

    #### custom reference points
    if (any(!is.na(df_segments[[paste0("CustomRefPointValue", customRefPoint[1])]])) && !all(customRefPoint %in% colnames(df))) {
        p4 <- p4 +
            geom_line(aes(
                x = Year,
                y = df_segments[[paste0("CustomRefPointValue", customRefPoint)]],
                # linetype = as.factor("dotdash"),
                # colour = as.factor(df4[[paste0("CustomRefPointName", customRefPoint)]][1]),#"#69d371",
                # size = as.factor(.8),
                linetype = df_segments[[paste0("CustomRefPointName", customRefPoint)]][1],
                colour = df_segments[[paste0("CustomRefPointName", customRefPoint)]][1],
                size = df_segments[[paste0("CustomRefPointName", customRefPoint)]][1],
                text = map(
                    paste0(
                        "<b>", df_segments[[paste0("CustomRefPointName", customRefPoint)]][1], ": </b>", tail(df_segments[[paste0("CustomRefPointValue", customRefPoint)]], 1)
                    ), HTML
                )
            ))
    }


    diamondYears <-
        sagSettings4 %>%
        filter(settingKey == 14) %>%
        pull(settingValue) %>%
        str_split(pattern = ",", simplify = TRUE) %>%
        as.numeric()

    if (any(!is.na(diamondYears))) {
        p4 <- p4 + geom_point(
            data = df_segments %>% filter(Year %in% diamondYears),
            aes(
                x = Year,
                y = StockSize,
                text = map(
                    paste0(
                        "<b>Year: </b>", Year,
                        "<br>",
                        "<b>Forecast spawning-stock biomass (StockSize): </b>", StockSize
                    ), HTML
                )
            ),
            shape = 23,
            fill = "#cfcfcf",
            color = "#3aa6ff",
            size = 2.5,
            show.legend = FALSE,
            inherit.aes = FALSE
        )
    }


    # add average lines
    averageYears <-
        sagSettings4 %>%
        filter(settingKey == 46) %>%
        pull(settingValue) %>%
        str_split(",", simplify = TRUE) %>%
        as.numeric()

    if (length(averageYears)) {
        id1 <- nrow(df_segments) - 1:averageYears[1] + 1
        id2 <- nrow(df_segments) - 1:averageYears[2] - averageYears[1] + 1
        avedf1 <- data.frame(
            Year = range(df_segments$Year[id1]) + c(-0.5, 0.5),
            StockSize = mean(df_segments$StockSize[id1], na.rm = TRUE)
        )
        avedf2 <- data.frame(
            Year = range(df_segments$Year[id2]) + c(-0.5, 0.5),
            StockSize = mean(df_segments$StockSize[id2], na.rm = TRUE)
        )

        p4 <-
            p4 + geom_line(
                data = avedf1,
                aes(
                    x = Year,
                    y = StockSize,
                    linetype = "Average",
                    colour = "Average",
                    size = "Average",
                    text = map(
                        paste0(
                            "<b>Average: </b>", StockSize
                        ), HTML
                    )
                )
            ) +
            geom_line(
                data = avedf2,
                aes(
                    x = Year,
                    y = StockSize,
                    linetype = "Average",
                    colour = "Average",
                    size = "Average",
                    text = map(
                        paste0(
                            "<b>Average: </b>", StockSize
                        ), HTML
                    )
                )
            )
    }



    min_year <- min(df_segments$Year[which(!is.na(df_segments$StockSize))])
    nullifempty <- function(x) if (length(x) == 0) NULL else x

    p4 <-
        p4 +
        xlim(min_year, max(df_segments$Year)) +
        theme_ICES_plots(
            type = "StockSize", df_segments,
            title = sagSettings4 %>% filter(settingKey == 1) %>% pull(settingValue) %>% nullifempty(),
            ylegend = sagSettings4 %>% filter(settingKey == 20) %>% pull(settingValue) %>% as.character() %>% replace_subscript_symbols(.) %>% nullifempty(),
            ymax = sagSettings4 %>% filter(settingKey == 6) %>% pull(settingValue) %>% as.numeric() %>% nullifempty()
        )


    # converting
    fig4 <- ggplotly(p4, tooltip = "text") %>%
        layout(
            autosize = T,
            legend = list(
                itemsizing = "trace",
                orientation = "h",
                y = -.3,
                yanchor = "bottom",
                x = 0.5,
                xanchor = "center",
                itemwidth = 20,
                itemsizing = "trace",
                title = list(text = "")
            ),
            xaxis = list(zeroline = TRUE),
            annotations = list(
                showarrow = FALSE,
                text = tail(df$SAGStamp, 1),
                font = list(family = "Calibri, serif", size = 12, color = "#acacac"),
                yref = "paper", y = 1, xref = "paper", x = 1,
                yanchor = "right", xanchor = "right"
            )
        )  #%>% style(showlegend = FALSE, traces = 2)

    for (i in 1:length(fig4$x$data)) {
        if (!is.null(fig4$x$data[[i]]$name)) {
            fig4$x$data[[i]]$name <- gsub("\\(", "", str_split(fig4$x$data[[i]]$name, ",")[[1]][1])
            if (fig4$x$data[[i]]$name == "StockSize") {
                fig4$x$data[[i]]$name <- df_segments$StockSizeDescription[1]
            }
        }
    }


    fig4
}


#' Function to plot spawning stock biomass (StockSize) for the last 5 years (quality of assessement section)
#'
#' @param df (quality of assessement SAG data)
#'
#' @return a ggplotly object
#'
#' @note
#'
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' ICES_plot_5(df)
#' }
#'
#' @references
#'https://www.ices.dk/data/assessment-tools/Pages/stock-assessment-graphs.aspx
#'
#'
#' @export
#'
ICES_plot_5 <- function(df, sagSettings) {
    
    sagSettings4 <- sagSettings %>% filter(SAGChartKey == 4)
    
    df5 <- df %>%
        filter(Purpose == "Advice") %>%
        select(Year, AssessmentYear, StockSize, Blim, Bpa, MSYBtrigger, StockSizeDescription, StockSizeUnits, SAGStamp) #%>%
     
    p5 <- df5 %>%
        ggplot(., aes(x = Year, y = StockSize, color = AssessmentYear))
        
    p5 <- p5 +    
        geom_line(
            aes(
                x = Year,
                y = StockSize,
                color = AssessmentYear,
                size = "StockSize",
                linetype = "StockSize",
                text = map(
                    paste0(
                        "<b>Year: </b>", Year,
                        "<br>",
                        "<b>Assessment year: </b>", AssessmentYear,
                        "<br>",
                        "<b>", StockSizeDescription, ": </b>", StockSize, " ", StockSizeUnits
                    ), HTML
                )
            ) 
        )

        if (any(!is.na(df5$MSYBtrigger))) {
            p5 <- p5 +
                geom_hline(aes(
                    yintercept = tail(MSYBtrigger, 1),
                    linetype = "MSY B<sub>trigger</sub>",
                    colour = "MSY B<sub>trigger</sub>",
                    size = "MSY B<sub>trigger</sub>",
                    text = map(
                        paste0(
                            "<b>MSY B<sub>trigger</sub>: </b>", tail(MSYBtrigger, 1)
                        ), HTML
                    )
                ))
        }

        if (any(!is.na(df5$Blim))) {
            p5 <- p5 +
                geom_hline(aes(
                    yintercept = tail(Blim, 1),
                    linetype = "B<sub>Lim</sub>",
                    colour = "B<sub>Lim</sub>",
                    size = "B<sub>Lim</sub>",
                    text = map(
                        paste0(
                            "<b>B<sub>Lim</sub>: </b>", tail(Blim, 1)
                        ), HTML
                    )
                ))
        }

        if (any(!is.na(df5$Bpa))) {
            p5 <- p5 +
                geom_hline(aes(
                    yintercept = tail(Bpa, 1),
                    linetype = "B<sub>pa</sub>",
                    colour = "B<sub>pa</sub>",
                    size = "B<sub>pa</sub>",
                    text = map(
                        paste0(
                            "<b>B<sub>pa</sub>: </b>", tail(Bpa, 1)
                        ), HTML
                    )
                ))
        }

        
    
        nullifempty <- function(x) if (length(x) == 0) NULL else x
        
        title_temp <- sagSettings4 %>% filter(settingKey == 55) %>% pull(settingValue) %>% as.character() %>% nullifempty()
        if (is.null(title_temp)){
            title_temp <- sagSettings4 %>% filter(settingKey == 20) %>% pull(settingValue) %>% as.character() %>% nullifempty()
        }
        


        p5 <-
            p5 +
            theme_ICES_plots(
            type = "quality_SSB", df,
            title = title_temp
            )
    
    fig5 <- ggplotly(p5, tooltip = "text") %>%
        layout(
            legend = list(
                orientation = "h",
                y = -.4,
                yanchor = "bottom",
                x = 0.5,
                xanchor = "center",
                title = list(text = "")
            ),
            xaxis = list(zeroline = TRUE),
            annotations = list(
                showarrow = FALSE,
                text = tail(df$SAGStamp,1),
                font = list(family = "Calibri, serif", size = 12, color = "#acacac"),
                yref = "paper", y = 1, xref = "paper", x = 1,
                yanchor = "right", xanchor = "right")
        ) #%>% 
        #config(modeBarButtonsToAdd = list(data_download_button(disclaimer))) 

    for (i in 1:length(fig5$x$data)) {
        if (!is.null(fig5$x$data[[i]]$name)) {
            fig5$x$data[[i]]$name <- gsub("\\(", "", str_split(fig5$x$data[[i]]$name, ",")[[1]][1])
        }
    }
    fig5
}

#' Function to plot fishing pressure (F) for the last 5 years (quality of assessement section)
#'
#' @param df (quality of assessement SAG data)
#'
#' @return a ggplotly object
#'
#' @note
#'
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' ICES_plot_6(df)
#' }
#'
#' @references
#'https://www.ices.dk/data/assessment-tools/Pages/stock-assessment-graphs.aspx
#'
#'
#' @export
#'
ICES_plot_6 <- function(df, sagSettings) {
    sagSettings3 <- sagSettings %>% filter(SAGChartKey == 3)

    df6 <- df %>%
        filter(Purpose == "Advice") %>%
        select(Year, FishingPressure, Flim, Fpa, FMSY, FAge, FishingPressureDescription, AssessmentYear, SAGStamp) # %>%
       
    p6 <- df6 %>%
        ggplot(., aes(x = Year, y = FishingPressure, color = AssessmentYear)) 

    p6 <- p6 +    
        geom_line(
            aes(
                x = Year,
                y = FishingPressure,
                color = AssessmentYear,
                size = "FishingPressure",
                linetype = "FishingPressure",
                text = map(
                    paste0(
                        "<b>Year: </b>", Year,
                        "<br>",
                        "<b>Assessment year: </b>", AssessmentYear,
                        "<br>",
                        "<b>", FishingPressureDescription, ": </b>", FishingPressure
                    ), HTML
                )
            ) 
        ) 

        if (any(!is.na(df6$FMSY))) {
            p6 <- p6 +
                geom_hline(aes(
                    yintercept = tail(FMSY, 1),
                    linetype = "F<sub>MSY</sub>",
                    colour = "F<sub>MSY</sub>",
                    size = "F<sub>MSY</sub>",
                    text = map(
                        paste0(
                            "<b>F<sub>MSY</sub>: </b>", tail(FMSY, 1)
                        ), HTML
                    )
                ))
        }

        if (any(!is.na(df6$Flim))) {
            p6 <- p6 +
                geom_hline(aes(
                    yintercept = tail(Flim, 1),
                    linetype = "F<sub>Lim</sub>",
                    colour = "F<sub>Lim</sub>",
                    size = "F<sub>Lim</sub>",
                    text = map(
                        paste0(
                            "<b>F<sub>Lim</sub>: </b>", tail(Flim, 1)
                        ), HTML
                    )
                ))
        }

        if (any(!is.na(df6$Fpa))) {
            p6 <- p6 +
                geom_hline(aes(
                    yintercept = tail(Fpa, 1),
                    linetype = "F<sub>pa</sub>",
                    colour = "F<sub>pa</sub>",
                    size = "F<sub>pa</sub>",
                    text = map(
                        paste0(
                            "<b>F<sub>pa</sub>: </b>", tail(Fpa, 1)
                        ), HTML
                    )
                ))
        }

        
       
        nullifempty <- function(x) if (length(x) == 0) NULL else x

        title_temp <- sagSettings3 %>% filter(settingKey == 55) %>% pull(settingValue) %>% as.character() %>% nullifempty()
        if (is.null(title_temp)){
            title_temp <- sagSettings3 %>% filter(settingKey == 20) %>% pull(settingValue) %>% as.character() %>% nullifempty()
        }

        p6 <-
            p6 +
            theme_ICES_plots(
            type = "quality_F", df,
            title = title_temp
            )

    # converting
    fig6 <- ggplotly(p6, tooltip = "text") %>%
        layout(
            legend = list(
                orientation = "h",
                y = -.4,
                yanchor = "bottom",
                x = 0.5,
                xanchor = "center",
                title = list(text = "")
            ),
            xaxis = list(zeroline = TRUE),
            annotations = list(
                showarrow = FALSE,
                text = tail(df$SAGStamp,1),
                font = list(family = "Calibri, serif", size = 12, color = "#acacac"),
                yref = "paper", y = 1, xref = "paper", x = 1,
                yanchor = "right", xanchor = "right")
        ) #%>% 
        #config(modeBarButtonsToAdd = list(data_download_button(disclaimer)))
    
    for (i in 1:length(fig6$x$data)) {
        if (!is.null(fig6$x$data[[i]]$name)) {
            fig6$x$data[[i]]$name <- gsub("\\(", "", str_split(fig6$x$data[[i]]$name, ",")[[1]][1])
        }
    }
    fig6
}


#' Function to plot Recruitment (R) for the last 5 years (quality of assessement section)
#'
#' @param df (quality of assessement SAG data)
#'
#' @return a ggplotly object
#'
#' @note
#'
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' ICES_plot_7(df)
#' }
#'
#' @references
#'https://www.ices.dk/data/assessment-tools/Pages/stock-assessment-graphs.aspx
#'
#'
#' @export
#'
ICES_plot_7 <- function(df, sagSettings) {
    sagSettings2 <- sagSettings %>% filter(SAGChartKey == 2)

    p7 <- df %>% filter(Purpose == "Advice") %>%
        select(Year, Recruitment, RecruitmentAge, AssessmentYear, SAGStamp) %>%
        drop_na(Recruitment) %>%
        ggplot(., aes(x = Year, y = Recruitment, color = AssessmentYear)) +
        geom_line(
            aes(
                x = Year,
                y = Recruitment,
                color = AssessmentYear,
                size = "Recruitment",
                linetype = "Recruitment",
                text = map(
                    paste0(
                        "<b>Year: </b>", Year,
                        "<br>",
                        "<b>Assessment year: </b>", AssessmentYear,
                        "<br>",
                        "<b>Recruitment: </b>", Recruitment,
                        "<br>",
                        "<b>Recruitment age: </b>", RecruitmentAge
                    ), HTML
                )
            )
        )

        nullifempty <- function(x) if (length(x) == 0) NULL else x

        title_temp <- sagSettings2 %>% filter(settingKey == 55) %>% pull(settingValue) %>% as.character() %>% nullifempty()
        if (is.null(title_temp)){
            title_temp <- sagSettings2 %>% filter(settingKey == 20) %>% pull(settingValue) %>% as.character() %>% nullifempty()
        }

        p7 <- 
            p7  +
            theme_ICES_plots(
                type = "quality_R", df,
                title = title_temp)

    # converting
    fig7 <- ggplotly(p7, tooltip = "text") %>%
        layout(
            legend = list(
                orientation = "h",
                y = -.4,
                yanchor = "bottom",
                x = 0.5,
                xanchor = "center",
                title = list(text = "")
            ),
            xaxis = list(zeroline = TRUE),
            annotations = list(
                showarrow = FALSE,
                text = tail(df$SAGStamp,1),
                font = list(family = "Calibri, serif", size = 12, color = "#acacac"),
                yref = "paper", y = 1, xref = "paper", x = 1,
                yanchor = "right", xanchor = "right")
        ) #%>% 
        #config(modeBarButtonsToAdd = list(data_download_button(disclaimer)))

    for (i in 1:length(fig7$x$data)) {
        if (!is.null(fig7$x$data[[i]]$name)) {
            fig7$x$data[[i]]$name <- gsub("\\(", "", str_split(fig7$x$data[[i]]$name, ",")[[1]][1])
        }
    }
    fig7
}




### Plotting utilities

## Formatting axis title (titlefont)
titlefont_format <- function() {
    f1 <- list(
        family = "Calibri, sans-serif",
        size = 25,
        color = "darkgrey")
}
## Formatting tick axis font (tickfont)
tickfont_format <- function() {
    f2 <- list(
        family = "Calibri, serif",
        size = 20,
        color = "black")
}
## Formatting legend
legend_format <- function() {
    leg <- list(
        font = list(
        family = "sans-serif",
        size = 20,
        color = "#000"),
        grouptitlefont = list(
            color = "black",
            family = "sans-serif",
            size = 24
        ),
        bgcolor = "rgb(233,244,245)",
        bordercolor = "#FFFFFF",
        borderwidth = 2,
        orientation = 'v',
        tracegroupgap = 30,
        traceorder = "grouped")
       
}

#' simple function to shorten catch scenario tick labels if they are too long
#'
#' @param catch_scenarios_array (char array of catch scenarios)

#'
#' @return updated char array
#'
#' @note

#' @examples
#' \dontrun{
#' shorten_labels(tmp$cat)
#' }
#'
#' @references
#'
#'
#'
#' @export
#'
shorten_labels <- function(catch_scenarios_array) {
        
        for (i in 1:length(catch_scenarios_array)) {
            if (nchar(catch_scenarios_array[i]) > 14) {
                catch_scenarios_array[i] <- paste0(substr(catch_scenarios_array[i], 1, 14), "...")
            } else {
                catch_scenarios_array[i] <- catch_scenarios_array[i]
            }
        }
        return(catch_scenarios_array)
    }

#' Radial plot to compare the % of change of the different catch scenarios for a particular stock/year
#'
#' @param tmp (catch scenario table scaled in percentages)
#' @param catch_scenarios (one or more catch scenarios chosen by the user using selectizeInput)
#'
#' @return a ggradar plot embedded in a ggplotly container
#'
#' @note
#' The ggradar function works only if the values are scaled between 0 and 1, so the original
#' catch scenario table is first scaled based on the values of the previous year advice (% of change, see scale_catch_scenarios_for_radialPlot())
#' and then it is scaled here between 0 and 1 (see rescale_function())
#'
#' @examples
#' \dontrun{
#'radial_plot(df,"F = 0")
#' }
#'
#' @references
#'
#'
#'
#' @export
#'
####### plots 1 catch scenarios
radial_plot <- function(tmp, catch_scenarios) {
    
    not_all_na <- function(x) any(!is.na(x))
    tmp <- tmp %>% select(where(not_all_na))

    rescale_function <- function(x) rescale(x, to = c(0, 1), from = range(c(min(x), max(x))))
    tmp <- tmp %>% select(-c(Year)) %>% na.omit() %>% mutate_if(is.numeric, rescale_function)

    zz <- ggplotly(
        ggradar(tmp %>% select(-cS_Purpose) %>% filter(cat %in% catch_scenarios),
            base.size = 6,
            font.radar = "sans",
            values.radar = c("Min", "0", "Max"),
            gridline.min.linetype = "longdash",
            gridline.mid.linetype = "longdash",
            gridline.max.linetype = "longdash",
            gridline.min.colour = "grey",
            gridline.mid.colour = "#007A87",
            gridline.max.colour = "grey",
            grid.label.size = 6,
            label.gridline.min = TRUE,
            label.gridline.mid = TRUE,
            label.gridline.max = TRUE,
            axis.label.offset = 1.20,
            axis.label.size = 6,
            axis.line.colour = "grey",
            group.line.width = 1,
            group.point.size = 4,
            group.colours = NULL,
            background.circle.colour = "#D7D6D1",
            background.circle.transparency = 0.2,
            plot.legend = TRUE, # if (nrow(catch_tab_stand_scaled) > 1) TRUE else FALSE,
            legend.title = "Catch scenarios:",
            plot.title = "",
            legend.text.size = 8,
            legend.position = "right"
        ),
        tooltip = c("cat")
    )
    
    zz
}



#' Plot to visualise the effect of the different catch scenarios on F, StockSize and the resulting total Catches
#'
#' @param tmp (catch scenario table)
#' @param df (SAG data)
#'
#' @return a plotly object
#'
#' @note
#' source = "ranking" is used to link the hovering on the different catch scenarios with
#' the highlighting of the corresponding row in the table
#'
#'
#' @examples
#' \dontrun{
#' catch_scenario_plot_1(catch_scenario_table(), SAG_data_reactive())
#' }
#'
#' @references
#'
#'
#'
#' @export
#'

catch_scenario_plot_1 <- function(tmp, df, sagSettings) {
    nullifempty <- function(x) if (length(x) == 0) NULL else x
    
    F_yaxis_label <- sagSettings %>% filter(SAGChartKey == 3) %>% filter(settingKey == 20) %>% pull(settingValue) %>% as.character() %>% nullifempty()
    if (is.null(F_yaxis_label)) {
          F_yaxis_label <- sprintf("%s <sub>(ages %s)</sub>",dplyr::last(df$FishingPressureDescription), dplyr::last(df$FAge))
        }
    
    
    SSB_yaxis_label <- sagSettings %>% filter(SAGChartKey == 4) %>% filter(settingKey == 20) %>% pull(settingValue) %>% as.character() %>% nullifempty()
    if (is.null(SSB_yaxis_label)) {
    SSB_yaxis_label <- sprintf("%s (1000 %s)", dplyr::last(df$StockSizeDescription), dplyr::last(df$StockSizeUnits))
        }
    Discards_yaxis_label <- "Discards (tonnes)"
       
    Catches_yaxis_label <- sprintf("Catches (%s)", dplyr::last(df$CatchesLandingsUnits))
    
    tmp <- data.frame(tmp$table)
    
    
    tmp$fmsy <- tail(df$FMSY,1)
    tmp$blim <- tail(df$Blim,1)
    
    

    
    Basis <- tmp[tmp$cS_Purpose == "Basis Of Advice",]
    tmp <- tmp[tmp$cS_Purpose == "Other Scenarios",] %>% 
      dplyr::filter(TotCatch != Basis$TotCatch) %>% 
      dplyr::bind_rows(Basis)
    
    
    # Function to check if a column is made up of all NA values
    is_na_column <- function(dataframe, col_name) {
        return(all(is.na(dataframe[, col_name])))
    }
    
    if (is_na_column(tmp, "F")){
        tmp <- arrange(tmp, F_wanted)

        labels <- sprintf(
            "Catch Scenario: %s", tmp$cat
        ) %>% lapply(htmltools::HTML)

        fig_catch <- plot_ly(tmp) %>%
        add_trace(
            x = ~ TotCatch,
            y = ~ F_wanted,
            type = "scatter",
            mode = "lines+markers",
            text = labels,
            marker = list(color = "#ed5f26", size = 10),
            line = list(color = "#ed5f26", width = 2, dash = 'solid'),
            name = "F wanted"
        ) %>% 
        add_trace(
            x = ~ TotCatch,
            y = ~ fmsy,
            type = "scatter",
            mode = "lines",
            text = "FMSY",
            line = list(color = "#00AC67", width = .9, dash = 'solid'),
            name = "FMSY"
        ) %>% 
        add_markers(
            x = Basis$TotCatch,
            y = Basis$F_wanted,
            type = "scatter",
            mode = "markers",            
            marker = list(color = "#ed5f26", size = 15, symbol = "circle-open"),
            text = "Basis of advice",
            name = "Basis of advice"
        )

    if (is_na_column(tmp, "F_wanted")){
        tmp <- arrange(tmp, HR)

        labels <- sprintf(
            "Catch Scenario: %s", tmp$cat
        ) %>% lapply(htmltools::HTML)

        fig_catch <- plot_ly(tmp) %>%
        add_trace(
            x = ~ TotCatch,
            y = ~ HR,
            type = "scatter",
            mode = "lines+markers",
            text = labels,
            marker = list(color = "#ed5f26", size = 10),
            line = list(color = "#ed5f26", width = 2, dash = 'solid'),
            name = "HR"
        ) %>% 
        add_trace(
            x = ~ TotCatch,
            y = ~ fmsy,
            type = "scatter",
            mode = "lines",
            text = "FMSY",
            line = list(color = "#00AC67", width = .9, dash = 'solid'),
            name = "FMSY"
        ) %>% 
        add_markers(
            x = Basis$TotCatch,
            y = Basis$HR,
            type = "scatter",
            mode = "markers",            
            marker = list(color = "#ed5f26", size = 15, symbol = "circle-open"),
            text = "Basis of advice",
            name = "Basis of advice"
        )
    
    }
    } else {
        tmp <- arrange(tmp, F)

        labels <- sprintf(
            "Catch Scenario: %s", tmp$cat
        ) %>% lapply(htmltools::HTML)

        fig_catch <- plot_ly(tmp) %>%
        add_trace(
            x = ~ TotCatch,
            y = ~ F,
            type = "scatter",
            mode = "lines+markers",
            text = labels,            
            marker = list(color = "#ed5f26", size = 10),
            line = list(color = "#ed5f26", width = 2, dash = 'solid'),
            name = "F"
        ) %>% 
        add_trace(
            x = ~ TotCatch,
            y = ~ fmsy,
            type = "scatter",
            mode = "lines",
            text = "FMSY",
            line = list(color = "#00AC67", width = .9, dash = 'solid'),
            name = "FMSY"
        ) %>% 
        add_markers(
            x = Basis$TotCatch,
            y = Basis$F,
            type = "scatter",
            mode = "markers",            
            marker = list(color = "#ed5f26", size = 15, symbol = "circle-open"),
            text = "Basis of advice",
            name = "Basis of advice"
        )
    }

        ay <- list(
            overlaying = "y",
            side = "right",
            title = SSB_yaxis_label,
            titlefont = titlefont_format(),
            tickfont = tickfont_format()
        )

            fig_catch <- fig_catch %>%
                add_trace(
                    x = ~TotCatch,
                    y = ~ SSB / 1000,
                    type = "scatter",
                    mode = "lines+markers",
                    text = labels,
                    line = list(color = "#047c6c", width = 2, dash = "solid"),
                    marker = list(size = 10, color = "#047c6c"),
                    name = "SSB",
                    yaxis = "y2"
                ) %>%
                add_trace(
                    x = ~TotCatch,
                    y = ~ blim / 1000,
                    type = "scatter",
                    mode = "lines",
                    text = "BLim",
                    line = list(color = "black", width = .9, dash = "dash"),
                    name = "BLim",
                    yaxis = "y2"
                ) %>%
                add_markers(
                    x = Basis$TotCatch,
                    y = Basis$SSB / 1000,
                    type = "scatter",
                    mode = "markers",
                    marker = list(color = "#047c6c", size = 15, symbol = "circle-open"),
                    text = "Basis of advice",
                    name = "Basis of advice",
                    yaxis = "y2"
                )
   
    
    fig_catch <- fig_catch %>% layout(
        paper_bgcolor = "rgb(255,255,255)",
        plot_bgcolor = "rgb(255,255,255)",
        hovermode = "x",
        yaxis2 = ay,
        legend = list(
                orientation = "h",
                y = 1.05,
                yanchor = "bottom",
                x = 0.5,
                xanchor = "center",
                title = list(text = "")
            ),
        autosize = T,
        margin = list(l = 120, r = 120, b = 120, t = 50, pad = 8),
        xaxis = list(
            title = Catches_yaxis_label,
            gridcolor = "rgb(235,235,235)",
            showgrid = TRUE,
            showline = TRUE,
            tickcolor = "rgb(127,127,127)",
            titlefont = titlefont_format(),
            tickfont = tickfont_format(),
            showticklabels = TRUE
        ),
        yaxis = list(
            title = F_yaxis_label, 
            gridcolor = "rgb(235,235,235)",
            showgrid = TRUE,
            showline = TRUE,
            showticklabels = TRUE,
            tickcolor = "rgb(127,127,127)",
            ticks = "outside",
            zeroline = TRUE,
            rangemode = "tozero",
            titlefont = titlefont_format(),
            tickfont = tickfont_format()
        )
    ) #%>% 
        #config(modeBarButtonsToAdd = list(data_download_button(disclaimer)))


}


#' Plot to visualise the effect of the different catch scenarios on F, Discards and the resulting total Catches
#'
#' @param tmp (catch scenario table)
#' @param df (SAG data)
#' @param sagSettings (df of sag settings)
#'
#' @return a plotly object
#'
#' @note
#' 
#'
#'
#' @examples
#' \dontrun{
#' catch_scenario_plot_1(catch_scenario_table(), SAG_data_reactive())
#' }
#'
#' @references
#'
#'
#'
#' @export
#'
catch_scenario_plot_1_nephrops <- function(tmp, df, sagSettings) {
    Discards_yaxis_label <- sprintf("Catches (%s)", dplyr::last(df$CatchesLandingsUnits))
    nullifempty <- function(x) if (length(x) == 0) NULL else x

    F_yaxis_label <- sagSettings %>%
        filter(SAGChartKey == 3) %>%
        filter(settingKey == 20) %>%
        pull(settingValue) %>%
        as.character() %>%
        nullifempty()
    if (is.null(F_yaxis_label)) {
        F_yaxis_label <- sprintf("%s <sub>(ages %s)</sub>", dplyr::last(df$FishingPressureDescription), dplyr::last(df$FAge))
    }


    SSB_yaxis_label <- sagSettings %>%
        filter(SAGChartKey == 4) %>%
        filter(settingKey == 20) %>%
        pull(settingValue) %>%
        as.character() %>%
        nullifempty()
    if (is.null(SSB_yaxis_label)) {
        SSB_yaxis_label <- sprintf("%s (1000 %s)", dplyr::last(df$StockSizeDescription), dplyr::last(df$StockSizeUnits))
    }


    Catches_yaxis_label <- sprintf("Catches (%s)", dplyr::last(df$CatchesLandingsUnits))

    tmp <- data.frame(tmp$table)


    tmp$fmsy <- tail(df$FMSY, 1)
    tmp$blim <- tail(df$Blim, 1)
    # print(tmp)

    
    tmp$cat <- shorten_labels(tmp$cat)
    Basis <- tmp[tmp$cS_Purpose == "Basis Of Advice", ]
    tmp <- tmp[tmp$cS_Purpose == "Other Scenarios",] %>% 
      dplyr::filter(TotCatch != Basis$TotCatch) %>% 
      dplyr::bind_rows(Basis)
    
    # Function to check if a column is made up of all NA values
    is_na_column <- function(dataframe, col_name) {
        return(all(is.na(dataframe[, col_name])))
    }
    if (is_na_column(tmp, "F")) {
        tmp <- arrange(tmp, F_wanted)
        labels <- sprintf(
            "Catch Scenario: %s", tmp$cat
        ) %>% lapply(htmltools::HTML)
        fig_catch <- plot_ly(tmp) %>%
            add_trace(
                x = ~TotCatch,
                y = ~F_wanted,
                type = "scatter",
                mode = "lines+markers",
                text = labels,
                marker = list(color = "#ed5f26", size = 10),
                line = list(color = "#ed5f26", width = 2, dash = "solid"),
                name = "F wanted"
            ) %>%
            add_trace(
                x = ~TotCatch,
                y = ~fmsy,
                type = "scatter",
                mode = "lines",
                text = "FMSY",
                line = list(color = "#00AC67", width = .9, dash = "solid"),
                name = "FMSY"
            ) %>%
            add_markers(
                x = Basis$TotCatch,
                y = Basis$F_wanted,
                type = "scatter",
                mode = "markers",
                marker = list(color = "#ed5f26", size = 15, symbol = "circle-open"),
                text = "Basis of advice",
                name = "Basis of advice"
            )

        if (is_na_column(tmp, "F_wanted")) {
          
            tmp <- arrange(tmp, HR)
            labels <- sprintf(
              "Catch Scenario: %s", tmp$cat
            ) %>% lapply(htmltools::HTML)
            
            fig_catch <- plot_ly(tmp) %>%
                add_trace(
                    x = ~TotCatch,
                    y = ~HR,
                    type = "scatter",
                    mode = "lines+markers",
                    text = labels,
                    marker = list(color = "#ed5f26", size = 10),
                    line = list(color = "#ed5f26", width = 2, dash = "solid"),
                    name = "HR"
                ) %>%
                add_trace(
                    x = ~TotCatch,
                    y = ~fmsy,
                    type = "scatter",
                    mode = "lines",
                    text = "FMSY",
                    line = list(color = "#00AC67", width = .9, dash = "solid"),
                    name = "FMSY"
                ) %>%
                add_markers(
                    x = Basis$TotCatch,
                    y = Basis$HR,
                    type = "scatter",
                    mode = "markers",
                    marker = list(color = "#ed5f26", size = 15, symbol = "circle-open"),
                    text = "Basis of advice",
                    name = "Basis of advice"
                )
        }
    } else {
        tmp <- arrange(tmp, F)
        labels <- sprintf(
          "Catch Scenario: %s", tmp$cat
        ) %>% lapply(htmltools::HTML)
        fig_catch <- plot_ly(tmp) %>%
            add_trace(
                x = ~TotCatch,
                y = ~F,
                type = "scatter",
                mode = "lines+markers",
                text = labels,
                marker = list(color = "#ed5f26", size = 10),
                line = list(color = "#ed5f26", width = 2, dash = "solid"),
                name = "F"
            ) %>%
            add_trace(
                x = ~TotCatch,
                y = ~fmsy,
                type = "scatter",
                mode = "lines",
                text = "FMSY",
                line = list(color = "#00AC67", width = .9, dash = "solid"),
                name = "FMSY"
            ) %>%
            add_markers(
                x = Basis$TotCatch,
                y = Basis$FishingPressure,
                type = "scatter",
                mode = "markers",
                marker = list(color = "#ed5f26", size = 15, symbol = "circle-open"),
                text = "Basis of advice",
                name = "Basis of advice"
            )
    }
    fig_catch <- fig_catch %>% layout(
        paper_bgcolor = "rgb(255,255,255)",
        plot_bgcolor = "rgb(255,255,255)",
        hovermode = "x",
        autosize = T,
        xaxis = list(
            title = Catches_yaxis_label,
            gridcolor = "rgb(235,235,235)",
            showgrid = TRUE,
            showline = TRUE,
            tickcolor = "rgb(127,127,127)",
            titlefont = titlefont_format(),
            tickfont = tickfont_format(),
            showticklabels = TRUE
        ),
        yaxis = list(
            title = F_yaxis_label,
            gridcolor = "rgb(235,235,235)",
            showgrid = TRUE,
            showline = TRUE,
            showticklabels = TRUE,
            tickcolor = "rgb(127,127,127)",
            ticks = "outside",
            zeroline = TRUE,
            rangemode = "tozero",
            titlefont = titlefont_format(),
            tickfont = tickfont_format()
            
        )
    )


    fig2 <- plot_ly(tmp,
        x = ~cat,
        y = ~CatchUnwantedSurviving,
        type = "bar",
        name = "Proj. surviving Discards",
        marker = list(color = "#00c7b0")
    )
    fig2 <- fig2 %>% add_trace(
        y = ~CatchUnwanted,
        name = "Proj. dead Discards",
        marker = list(color = "#00c7b091")
    )
    fig2 <- fig2 %>% layout(
        paper_bgcolor = "rgb(255,255,255)",
        plot_bgcolor = "rgb(255,255,255)",
        hovermode = "x",
        barmode = "stack",
        autosize = T,
        xaxis = list(
            title = "Scenarios",
            tickangle = -45,
            gridcolor = "rgb(235,235,235)",
            showgrid = TRUE,
            showline = TRUE,
            tickcolor = "rgb(127,127,127)",
            titlefont = titlefont_format(),
            tickfont = tickfont_format(),
            showticklabels = TRUE
        ),
        yaxis = list(
            title = Discards_yaxis_label,
            gridcolor = "rgb(235,235,235)",
            showgrid = TRUE,
            showline = TRUE,
            showticklabels = TRUE,
            tickcolor = "rgb(127,127,127)",
            ticks = "outside",
            zeroline = TRUE,
            rangemode = "tozero",
            titlefont = titlefont_format(),
            tickfont = tickfont_format()
            
        )
    )
    
    fig_final <- subplot(fig_catch, fig2, nrows = 1, margin = 0.1) %>%
        layout(
            autosize = T,
            xaxis = list(title = Catches_yaxis_label),
            xaxis2 = list(title = "Catch scenarios", categoryorder = "total ascending"),
            yaxis = list(title = F_yaxis_label),
            yaxis2 = list(title = Discards_yaxis_label),
            legend = list(
                orientation = "h",
                y = 1.05,
                yanchor = "bottom",
                x = 0.5,
                xanchor = "center",
                title = list(text = ""),
                traceorder = "reversed"
            )
        )
    fig_final #%>%
        #config(modeBarButtonsToAdd = list(data_download_button(disclaimer)))
}

#' Returns ....
#'
#' Downloads ...
#'
#' @param final_df (a df created with wrangle_Catches_with_scenarios())
#' @param catch_scenarios (one or more catch scenarios chosen by the user using selectizeInput)
#' @param df (SAG data)
#'
#' @return a plotly object
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' TAC_timeline(test_table(), input$catch_choice, SAG_data_reactive())
#' }
#'
#' @references
#'
#'
#'
#' @export
#'
TAC_timeline <- function(final_df, catch_scenarios, df) {
   
    Catches_yaxis_label <- sprintf("Catches (%s)", dplyr::last(df$CatchesLandingsUnits))

    catch_time <- plot_ly(final_df,
        x = ~Year,
        y = ~TotCatch
    ) %>%
        filter(cat %in% catch_scenarios) %>%
        group_by(cat) %>%
        add_trace(
            x = ~Year,
            y = ~TotCatch,
            type = "scatter",
            mode = "lines+markers",
            color = ~cat
        )

    catch_time <- catch_time %>% layout(
        
        paper_bgcolor = "rgb(255,255,255)",
        plot_bgcolor = "rgb(255,255,255)",
        legend = list(
            orientation = "h",
            y = -.6,
            yanchor = "bottom",
            x = 0.5,
            xanchor = "center",
            title = list(text = "Scenarios")
        ),

        xaxis = list(
            title = "Years",
            gridcolor = "rgb(235,235,235)",
            showgrid = TRUE,
            showline = TRUE,
            tickcolor = "rgb(127,127,127)",
            titlefont = titlefont_format(),
            tickfont = tickfont_format(),
            showticklabels = TRUE
        ),
    # )
        yaxis = list(
            title = Catches_yaxis_label, # "StockSize",
            gridcolor = "rgb(235,235,235)",
            showgrid = TRUE,
            showline = TRUE,
            showticklabels = TRUE,
            tickcolor = "rgb(127,127,127)",
            ticks = "outside",
            zeroline = TRUE,
            titlefont = titlefont_format(),
            tickfont = tickfont_format()
        )
    ) #%>% 
        #config(modeBarButtonsToAdd = list(data_download_button(disclaimer)))
}


#' Function to plot % of change from previous year assessement using a lollipop plot
#'
#' @param tmp (catch scenario table scaled in percentages)
#' @param indicator_choice_lollipop (one or more catch indicators chosen by the user using selectizeInput)
#'
#' @return a ggplotly object
#'
#' @note
#' 
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' 
#' }
#'
#' @references
#'
#' 
#'
#' @export
#' 
lollipop_plot <- function(df, indicator_choice_lollipop) {
    
    Basis <- df[df$cS_Purpose == "Basis Of Advice",]
    
    df <- df %>% select(-Year, -cS_Purpose)
    dd <- df %>% pivot_longer(cols = -1, names_to = "indicator")
    dd <- dd %>% filter(indicator %in% c(indicator_choice_lollipop))

    #### this is a function to highlight the basis of advice tick label
    #### at the moment is not working with all stocks, might have to do with 
    #### special characters in the string like "/". When it works it moves 
    #### the plot to the leaft leaving a big space on the left of the y labels
    
    
    pvar <- ggplot(dd, aes(x = cat, y = value, fill = indicator, colour = indicator)) +
        geom_segment(aes(x = cat, xend = as.factor(cat), y = 0, yend = value),
            color = "gray", lwd = 2
        ) +
        geom_point(size = 3) +
        coord_flip() +
    
        labs(y = "%", x = NULL) +
        facet_wrap(~indicator)
        
        
    fig8 <- ggplotly(pvar) %>% layout(showlegend = FALSE) #%>% 
        #config(modeBarButtonsToAdd = list(data_download_button(disclaimer)))
}
