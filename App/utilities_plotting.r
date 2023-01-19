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
#' theme_ICES_plots(type = "catches", df)
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
    type = c("catches", "recruitment", "F", "SSB", "quality_SSB", "quality_F", "quality_R"), df,
    title = NULL, ylegend = NULL, ymax = NULL) {
    font <- "Calibri, sans-serif" # assign font family up front
    tmp <- theme_minimal() %+replace% # replace elements we want to change

        theme(
            axis.title = element_text( # axis titles
                family = font, # font family
                size = 20,
                colour = "darkgrey",
                vjust = -2
            ),
            axis.text = element_text( # axis titles
                family = font, # font family
                size = 15,
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
                size = 23, # set font size
                face = "bold", # bold typeface
                hjust = 0, # left align
                vjust = 1,
                margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
                if (type == "catches") {
                    color <- "#002b5f"
                } else if (type == "recruitment" | type == "quality_R") {
                    color <- "#28b3e8"
                } else if (type == "F" | type == "quality_F") {
                    color <- "#ed5f26"
                } else if (type == "SSB" | type == "quality_SSB") {
                    color <- "#047c6c"
                }
            ),
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

    if (type == "catches") {

        if (is.null(title)) {
          title <- "Catches"
        }
        if (is.null(ylegend)) {
          ylegend <- sprintf("Catches in 1000 %s", dplyr::last(df$units))
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
                "landings" = "#002b5f",
                "discards" = "#fda500",
                "catches" = "#002b5f",
                "industrial bycatch" = "#00b29d",
                "unallocated_Removals" = "#6eb200"
            )),
            limits,
            scale_y_continuous(
                expand = expansion(mult = c(0, 0.1)),
                labels = function(l) {
                    trans <- l / 1000
                }
            )
        )
    } else if (type == "recruitment") {

        if (is.null(title)) {
          title <- sprintf("Recruitment <sub>(age %s)</sub>", dplyr::last(df$recruitment_age))
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
            scale_fill_manual(values = c("recruitment" = "#28b3e8")),
            scale_y_continuous(
                expand = expansion(mult = c(0, 0.1)),
                labels = function(l) {
                    trans <- l / 1000000
                }
            )
        )
    } else if (type == "F") {
        if (is.null(title)) {
          title <- "Fishing pressure"
        }
        if (is.null(ylegend)) {
          ylegend <- sprintf("%s <sub>(ages %s)</sub>", dplyr::last(df$fishingPressureDescription), dplyr::last(df$Fage))
        }

        theme_ICES_plots <- list(
            tmp,
            labs(
                title = title, #"Fishing pressure", # sprintf("Recruitment <sub>(age %s)</sub>", dplyr::last(df$recruitment_age)),
                y = ylegend, #sprintf("%s <sub>(ages %s)</sub>", dplyr::last(df$fishingPressureDescription), dplyr::last(df$Fage)), # sprintf("Catches in 1000 %s", dplyr::last(df$units))
                x = "Year"
            ),
            scale_color_manual(values = c(
                "F" = "#ed5f26",
                "F<sub>MSY</sub>" = "#00AC67",
                "F<sub>Lim</sub>" = "#000000",
                "F<sub>pa</sub>" = "#000000"
            )),
            scale_linetype_manual(values = c(
                "F" = "solid",
                "F<sub>Lim</sub>" = "dashed",
                "F<sub>pa</sub>" = "dotted",
                "F<sub>MSY</sub>" = "solid"
            )),
            scale_size_manual(values = c(
                "F" = 1.5,
                "F<sub>Lim</sub>" = .8,
                "F<sub>pa</sub>" = 1,
                "F<sub>MSY</sub>" = .5
            )),
            scale_fill_manual(values = c("#f2a497")),
            expand_limits(y = 0),
            scale_y_continuous(
                expand = expansion(mult = c(0, 0.1)) # ,
            )
        )
    } else if (type == "SSB") {
        if (is.null(title)) {
          title <- "Spawning Stock Biomass"
        }
        if (is.null(ylegend)) {
          ylegend <- sprintf("%s in 1000 %s", dplyr::last(df$stockSizeDescription), dplyr::last(df$stockSizeUnits))
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
                "SSB" = "#047c6c",
                "MSY B<sub>trigger</sub>" = "#689dff",
                "B<sub>Lim</sub>" = "#000000",
                "B<sub>pa</sub>" = "#000000",
                "Average" = "#ed5f26"
            )),
            scale_linetype_manual(values = c(
                "SSB" = "solid",
                "B<sub>Lim</sub>" = "dashed",
                "B<sub>pa</sub>" = "dotted",
                "MSY B<sub>trigger</sub>" = "solid",
                "Average" = "solid"
            )),
            scale_size_manual(values = c(
                "SSB" = 1.5,
                "B<sub>Lim</sub>" = .8,
                "B<sub>pa</sub>" = 1,
                "MSY B<sub>trigger</sub>" = .5,
                "Average" = .8
            )),
            scale_fill_manual(values = c("#94b0a9")),
            limits,
            scale_y_continuous(
                expand = expansion(mult = c(0, 0.1)),
                labels = ylabels_func
            )
        )
    } else if (type == "quality_SSB") {

        if (is.null(title)) {
          title <- sprintf("%s in 1000 %s", dplyr::last(df$stockSizeDescription), dplyr::last(df$stockSizeUnits))
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
            )
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

        theme_ICES_plots <- list(
            tmp,
            labs(
                title = sprintf("%s <sub>(ages %s)</sub>", dplyr::last(df$fishingPressureDescription), dplyr::last(df$Fage)),
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

            )
        )
    } else if (type == "quality_R") {
        line_type <- sapply(as.character(sort(unique(df$AssessmentYear))), function(x) "solid")
        line_size <- sapply(as.character(sort(unique(df$AssessmentYear))), function(x) 1)
        line_color <- c("#969696","#737373","#525252","#252525","#28b3e8") %>% tail(length(unique(df$AssessmentYear)))
        names(line_color) <- as.character(sort(unique(df$AssessmentYear)))

        theme_ICES_plots <- list(
            tmp,
            labs(
                title = sprintf("Rec <sub>(age %s)</sub> (Billions)", dplyr::last(df$RecruitmentAge)),
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
            )
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
data_download_button <- function(){

    icon_svg_path = "M15.608,6.262h-2.338v0.935h2.338c0.516,0,0.934,0.418,0.934,0.935v8.879c0,0.517-0.418,0.935-0.934,0.935H4.392c-0.516,0-0.935-0.418-0.935-0.935V8.131c0-0.516,0.419-0.935,0.935-0.935h2.336V6.262H4.392c-1.032,0-1.869,0.837-1.869,1.869v8.879c0,1.031,0.837,1.869,1.869,1.869h11.216c1.031,0,1.869-0.838,1.869-1.869V8.131C17.478,7.099,16.64,6.262,15.608,6.262z M9.513,11.973c0.017,0.082,0.047,0.162,0.109,0.226c0.104,0.106,0.243,0.143,0.378,0.126c0.135,0.017,0.274-0.02,0.377-0.126c0.064-0.065,0.097-0.147,0.115-0.231l1.708-1.751c0.178-0.183,0.178-0.479,0-0.662c-0.178-0.182-0.467-0.182-0.645,0l-1.101,1.129V1.588c0-0.258-0.204-0.467-0.456-0.467c-0.252,0-0.456,0.209-0.456,0.467v9.094L8.443,9.553c-0.178-0.182-0.467-0.182-0.645,0c-0.178,0.184-0.178,0.479,0,0.662L9.513,11.973z"

    dl_button <- list(
        name = "Download data",
        icon = list(
            path = icon_svg_path,
            transform = "scale(0.84) translate(-1, -1)"
            ),
        click = htmlwidgets::JS("
            function(gd) {
                var text = '';
                for(var i = 0; i < gd.data.length; i++){
                text += gd.layout.xaxis.title.text + gd.data[i].name + ',' + gd.data[i].x + '\\n';
                text += gd.layout.yaxis.title.text + gd.data[i].name + ',' + gd.data[i].y + '\\n';
                };
                var blob = new Blob([text], {type: 'text/plain'});
                var a = document.createElement('a');
                const object_URL = URL.createObjectURL(blob);
                a.href = object_URL;
                a.download = 'data.csv';
                document.body.appendChild(a);
                a.click();
                URL.revokeObjectURL(object_URL);
            }
    ")
    )
return(dl_button)
}


#' Function to plot landings ans discards
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
ICES_plot_1 <- function(df, sagSettings, additional_LandingData) {
    
    sagSettings1 <- sagSettings %>% filter(sagChartKey == 1)

    df <- df %>% left_join(y = additional_LandingData, by = "Year")

    df1 <- df %>%
        filter(Purpose == "Advice") %>%
        select(Year, landings, catches, discards, units, SAGStamp, ibc, unallocated_Removals) %>%
        relocate(c(ibc, unallocated_Removals), .after = discards) %>%
        rename("industrial bycatch" = ibc) 


    # Function to check if a column is made up of all NA values
    is_na_column <- function(dataframe, col_name) {
        return(all(is.na(dataframe[, ..col_name])))
    }

    if (is_na_column(df,"landings")){
        # df1$landings <- df1$catches
        df1 <- df1 %>%
        gather(type, count, catches:unallocated_Removals)
    } else {
        df1 <- df1 %>%
        select(-catches) %>% 
        gather(type, count, landings:unallocated_Removals)
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
        geom_bar(position = "stack", stat = "identity")

    nullifempty <- function(x) if (length(x) == 0) NULL else x

    p1 <-
        p1 +
        theme_ICES_plots(
            type = "catches", df,
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
        ) %>% 
        config(modeBarButtonsToAdd = list(data_download_button()))
    fig1
}


#' Function to plot recruitment
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
        select(Year, recruitment, low_recruitment, high_recruitment, recruitment_age, SAGStamp)

    sagSettings2 <- sagSettings %>% filter(sagChartKey == 2)


    shadeYears <- sagSettings2 %>%
        filter(settingKey == 14) %>%
        pull(settingValue) %>%
        str_split(pattern = ",", simplify = TRUE) %>%
        as.numeric()


    p2 <- df2 %>%
        ggplot(., aes(
            x = Year, y = recruitment,
            fill = "recruitment",
            text = map(
                paste0(
                    "<b>Year: </b>", Year,
                    "<br>",
                    "<b>Recruitment: </b>", recruitment
                ), HTML
            )
        )
        ) +
        geom_bar(stat = "identity", data = df2 %>% filter(!Year %in% shadeYears))


    if (any(!is.na(df2$high_recruitment))) {
        p2 <- p2 +
            geom_errorbar(
                data = df2 %>% filter(!is.na(high_recruitment)),
                aes(
                    ymin = low_recruitment,
                    ymax = high_recruitment,
                    text = map(
                        paste0(
                            "<b>Year: </b>", Year,
                            "<br>",
                            "<b>High recruitment: </b>", high_recruitment,
                            "<br>",
                            "<b>Low recruitment: </b>", low_recruitment
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
                            y = recruitment, 
                            fill = "recruitment",
                            text = map(
                                    paste0(
                                        "<b>Year: </b>", Year,
                                        "<br>",
                                        "<b>Assumed recruitment: </b>", recruitment
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
            type = "recruitment", df,
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
        ) %>%
        config(modeBarButtonsToAdd = list(data_download_button()))
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
  
    sagSettings3 <- sagSettings %>% filter(sagChartKey == 3)
  

    df3 <- df %>%
        filter(Purpose == "Advice") %>%
        select(Year, F, low_F, high_F, FLim, Fpa, FMSY, Fage, fishingPressureDescription, SAGStamp) %>%
        drop_na(F) # %>%
    
    p3 <- df3 %>%
        ggplot(., aes(x = Year, y = F))

    if (any(!is.na(df3$low_F))) {
        p3 <- p3 +
            geom_ribbon(aes(
                ymin = low_F,
                ymax = high_F,
                fill = "2*sd",
                text = map(
                    paste0(
                        "<b>Year: </b>", Year,
                        "<br>",
                        "<b>F: </b>", F,
                        "<br>",
                        "<b>High F: </b>", high_F,
                        "<br>",
                        "<b>Low F: </b>", low_F
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
        y = F,
        color = "F",
        text = map(
            paste0(
                "<b>Year: </b>", Year,
                "<br>",
                "<b>F: </b>", F
            ), HTML
        )
    )
   
    )
    
    if (any(!is.na(df3$FMSY))) {
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

    if (any(!is.na(df3$FLim))) {
        p3 <- p3 +
            geom_line(aes(
                x = Year,
                y = FLim,
                linetype = "F<sub>Lim</sub>",
                colour = "F<sub>Lim</sub>",
                size = "F<sub>Lim</sub>",
                text = map(
                    paste0(
                        "<b>F<sub>Lim</sub>: </b>", tail(FLim, 1)
                    ), HTML
                )
            ))
    }
    
    if (any(!is.na(df3$Fpa))) {
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
    
    
    
    nullifempty <- function(x) if (length(x) == 0) NULL else x

    p3 <-
        p3 +
        theme_ICES_plots(
        type = "F", df,
        title = sagSettings3 %>% filter(settingKey == 1) %>% pull(settingValue) %>% nullifempty(),
        ylegend = sagSettings3 %>% filter(settingKey == 20) %>% pull(settingValue) %>% nullifempty() 
        )



fig3 <- ggplotly(p3, tooltip = "text") %>%
    layout(
        legend = list(
            orientation = "h",
            itemwidth = 20,
            itemsizing= "trace",
            y = -.3, yanchor = "bottom",
            x = 0.5, xanchor = "center",
            title = list(text = "")
        ),
        xaxis = list(zeroline = TRUE),
        annotations = list(
            showarrow = FALSE,
                text = tail(df$SAGStamp,1),
                font = list(family = "Calibri, serif", size = 12, color = "#acacac"),
                yref = "paper", y = 1, xref = "paper", x = 1,
                yanchor = "right", xanchor = "right")
    ) %>% 
        config(modeBarButtonsToAdd = list(data_download_button()))

for (i in 1:length(fig3$x$data)) {
    if (!is.null(fig3$x$data[[i]]$name)) {
        fig3$x$data[[i]]$name <- gsub("\\(", "", str_split(fig3$x$data[[i]]$name, ",")[[1]][1])
    }
}
fig3
}

#' Function to plot spawning stock biomass (SSB)
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

  sagSettings4 <- sagSettings %>% filter(sagChartKey == 4)

df4 <- df %>%
  filter(Purpose == "Advice") %>%
  select(Year, low_SSB, SSB, high_SSB, Blim, Bpa, MSYBtrigger, stockSizeDescription, stockSizeUnits, SAGStamp) #%>%

p4 <- df4 %>%
    ggplot(., aes(x = Year, y = SSB))

if (any(!is.na(df4$low_SSB))) {
  p4 <- p4 +
    geom_ribbon(data =  df4 %>% filter(!is.na(high_SSB)), aes(
      ymin = low_SSB,
      ymax = high_SSB,
      fill = "2*sd",
      text = map(
        paste0(
          "<b>Year: </b>", Year,
          "<br>",
          "<b>SSB: </b>", SSB,
          "<br>",
          "<b>High SSB: </b>", high_SSB,
          "<br>",
          "<b>Low SSB: </b>", low_SSB
        ), HTML
      )
    ),
    linetype = "blank",
    size = 0
    )
}

p4 <- p4 +
    geom_line(data = df4 %>% filter(!is.na(SSB)), aes(
        x = Year,
        y = SSB,
        color = "SSB",
        text = map(
            paste0(
                "<b>Year: </b>", Year,
                "<br>",
                "<b>SSB: </b>", SSB
            ), HTML
        )
    ))

if (any(!is.na(df4$MSYBtrigger))) {
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

if (any(!is.na(df4$Blim))) {
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

if (any(!is.na(df4$Bpa))) {
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



# add average lines
averageYears <-
    sagSettings4 %>%
    filter(settingKey == 46) %>%
    pull(settingValue) %>%
    str_split(",", simplify = TRUE) %>%
    as.numeric()

if (length(averageYears)) {
    id1 <- nrow(df4) - 1:averageYears[1] + 1
    id2 <- nrow(df4) - 1:averageYears[2] - averageYears[1] + 1
    avedf1 <- data.frame(
        Year = range(df4$Year[id1]) + c(-0.5, 0.5),
        SSB = mean(df4$SSB[id1], na.rm = TRUE)
    )
    avedf2 <- data.frame(
        Year = range(df4$Year[id2]) + c(-0.5, 0.5),
        SSB = mean(df4$SSB[id2], na.rm = TRUE)
    )

    p4 <-
        p4 + geom_line(data = avedf1,
                        aes(x = Year,
                            y = SSB,
                            linetype = "Average",
                            colour = "Average",
                            size = "Average",
                            text = map(
                                paste0(
                                    "<b>Average: </b>", SSB
                                ), HTML
            ))) + 
            geom_line(data = avedf2,
                        aes(x = Year,
                            y = SSB,
                            linetype = "Average",
                            colour = "Average",
                            size = "Average",
                            text = map(
                                paste0(
                                    "<b>Average: </b>", SSB
                                ), HTML
            )))
}

nullifempty <- function(x) if (length(x) == 0) NULL else x

  p4 <-
    p4 +
    theme_ICES_plots(
      type = "SSB", df,
      title = sagSettings4 %>% filter(settingKey == 1) %>% pull(settingValue) %>% nullifempty(),
      ylegend = sagSettings4 %>% filter(settingKey == 20) %>% pull(settingValue) %>% as.character() %>% nullifempty(),
      ymax = sagSettings4 %>%
        filter(settingKey == 6) %>%
        pull(settingValue) %>%
        as.numeric() %>%
        nullifempty()
    )


#converting
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
            itemsizing= "trace",
            title = list(text = "")
        ),
        xaxis = list(zeroline = TRUE),
        annotations = list(
            showarrow = FALSE,
                text = tail(df$SAGStamp,1),
                font = list(family = "Calibri, serif", size = 12, color = "#acacac"),
                yref = "paper", y = 1, xref = "paper", x = 1,
                yanchor = "right", xanchor = "right")
    )  %>% 
        config(modeBarButtonsToAdd = list(data_download_button()))

for (i in 1:length(fig4$x$data)){
    if (!is.null(fig4$x$data[[i]]$name)){
        fig4$x$data[[i]]$name =  gsub("\\(","",str_split(fig4$x$data[[i]]$name,",")[[1]][1])
    }
}

fig4
}


#' Function to plot spawning stock biomass (SSB) for the last 5 years (quality of assessement section)
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

    sagSettings4 <- sagSettings %>% filter(sagChartKey == 4)
    
    df5 <- df %>%
        filter(Purpose == "Advice") %>%
        select(Year, AssessmentYear, SSB, Blim, Bpa, MSYBtrigger, stockSizeDescription, stockSizeUnits, SAGStamp) #%>%
     
    p5 <- df5 %>%
        ggplot(., aes(x = Year, y = SSB, color = AssessmentYear))
        
    p5 <- p5 +    
        geom_line(
            aes(
                x = Year,
                y = SSB,
                color = AssessmentYear,
                size = "SSB",
                linetype = "SSB",
                text = map(
                    paste0(
                        "<b>Year: </b>", Year,
                        "<br>",
                        "<b>Assessment year: </b>", AssessmentYear,
                        "<br>",
                        "<b>", stockSizeDescription, ": </b>", SSB, " ", stockSizeUnits
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

        p5 <-
            p5 +
            theme_ICES_plots(
            type = "quality_SSB", df,
            title = sagSettings4 %>% filter(settingKey == 55) %>% pull(settingValue) %>% nullifempty()
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
        ) %>% 
        config(modeBarButtonsToAdd = list(data_download_button())) 

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
    sagSettings6 <- sagSettings %>% filter(sagChartKey == 3)

    df6 <- df %>%
        filter(Purpose == "Advice") %>%
        select(Year, F, FLim, Fpa, FMSY, Fage, fishingPressureDescription, AssessmentYear, SAGStamp) # %>%
       
    p6 <- df6 %>%
        ggplot(., aes(x = Year, y = F, color = AssessmentYear)) 

    p6 <- p6 +    
        geom_line(
            aes(
                x = Year,
                y = F,
                color = AssessmentYear,
                size = "F",
                linetype = "F",
                text = map(
                    paste0(
                        "<b>Year: </b>", Year,
                        "<br>",
                        "<b>Assessment year: </b>", AssessmentYear,
                        "<br>",
                        "<b>", fishingPressureDescription, ": </b>", F
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

        if (any(!is.na(df6$FLim))) {
            p6 <- p6 +
                geom_hline(aes(
                    yintercept = tail(FLim, 1),
                    linetype = "F<sub>Lim</sub>",
                    colour = "F<sub>Lim</sub>",
                    size = "F<sub>Lim</sub>",
                    text = map(
                        paste0(
                            "<b>F<sub>Lim</sub>: </b>", tail(FLim, 1)
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

        p6 <-
            p6 +
            theme_ICES_plots(
            type = "quality_F", df,
            title = sagSettings3 %>% filter(settingKey == 55) %>% pull(settingValue) %>% nullifempty()
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
        ) %>% 
        config(modeBarButtonsToAdd = list(data_download_button()))
    
    for (i in 1:length(fig6$x$data)) {
        if (!is.null(fig6$x$data[[i]]$name)) {
            fig6$x$data[[i]]$name <- gsub("\\(", "", str_split(fig6$x$data[[i]]$name, ",")[[1]][1])
        }
    }
    fig6
}


#' Function to plot recruitment (R) for the last 5 years (quality of assessement section)
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
ICES_plot_7 <- function(df) {
    p7 <- df %>% filter(Purpose == "Advice") %>%
        select(Year, recruitment, RecruitmentAge, AssessmentYear, SAGStamp) %>%
        drop_na(recruitment) %>%
        ggplot(., aes(x = Year, y = recruitment, color = AssessmentYear)) +
        geom_line(
            aes(
                x = Year,
                y = recruitment,
                color = AssessmentYear,
                size = "recruitment",
                linetype = "recruitment",
                text = map(
                    paste0(
                        "<b>Year: </b>", Year,
                        "<br>",
                        "<b>Assessment year: </b>", AssessmentYear,
                        "<br>",
                        "<b>Recruitment: </b>", recruitment
                    ), HTML
                )
            )
        ) +
        theme_ICES_plots(type = "quality_R", df)

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
        ) %>% 
        config(modeBarButtonsToAdd = list(data_download_button()))

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
            values.radar = c("-100%", "0%","100%"),
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
        )
    ) %>% 
        config(modeBarButtonsToAdd = list(data_download_button()))
    zz
}



#' Plot to visualise the effect of the different catch scenarios on F, SSB and the resulting total catches
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
#' catch_scenarios_plot2(catch_scenario_table(), SAG_data_reactive())
#' }
#'
#' @references
#'
#'
#'
#' @export
#'

catch_scenarios_plot2 <- function(tmp, df) {
    F_yaxis_label <- sprintf("%s <sub>(ages %s)</sub>",dplyr::last(df$fishingPressureDescription), dplyr::last(df$Fage))
    SSB_yaxis_label<- sprintf("%s (%s)", dplyr::last(df$stockSizeDescription), dplyr::last(df$stockSizeUnits))
    catches_yaxis_label <- sprintf("Catches (%s)", dplyr::last(df$units))


    labels <- sprintf(
            "Catch Scenario: %s", tmp$cat
        ) %>% lapply(htmltools::HTML)

    
    Basis <- tmp[tmp$cS_Purpose == "Basis Of Advice",]

    fig_catch <- plot_ly(tmp, source = "ranking") %>%
        add_trace(
            x = ~ TotCatch,
            y = ~ F,
            type = "scatter",
            mode = "lines+markers",
            text = labels,
            marker = list(size = 10),
            name = "F"
        )
    ay <- list(
        overlaying = "y",
        side = "right",
        title = SSB_yaxis_label,
        titlefont = titlefont_format(),
        tickfont = tickfont_format()
    )
    fig_catch <- fig_catch %>% add_trace(
        x = ~ TotCatch,
        y = ~ SSB,
        type = "scatter",
        mode = "lines+markers",
        text = labels,
        marker = list(size = 10, color = "#ff7300"),
        name = "SSB",
        yaxis = "y2"
    )

    b <- list(
        x = Basis$TotCatch,
        y = Basis$F,
        text = Basis$cS_Purpose,
        xref = "x",
        yref = "y",
        showarrow = TRUE,
        arrowcolor = "#999999",
        arrowhead = 15,
        ax = 7,
        ay = -50, font = list(
            color = "#999999",
            family = "sans serif",
            size = 20
        )
    )

    fig_catch <- fig_catch %>% layout(
        paper_bgcolor = "rgb(255,255,255)",
        plot_bgcolor = "rgb(255,255,255)",
        hovermode = "x",
        yaxis2 = ay,
        annotations = b,
        legend = list(
            font = list(size = 20,
            color = "black"),
            bgcolor = "rgba(255,255,255, 0.2)",
            x = 0.1,
            y = 0.5
        ),
        autosize = T,
        margin = list(l = 120, r = 120, b = 120, t = 50, pad = 8),
        xaxis = list(
            title = catches_yaxis_label,
            gridcolor = "rgb(235,235,235)",
            showgrid = TRUE,
            showline = TRUE,
            tickcolor = "rgb(127,127,127)",
            titlefont = titlefont_format(),
            tickfont = tickfont_format(),
            showticklabels = TRUE
        ),
        yaxis = list(
            title = F_yaxis_label, # "SSB",
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
    ) %>% 
        config(modeBarButtonsToAdd = list(data_download_button()))


}

#' Returns ....
#'
#' Downloads ...
#'
#' @param final_df (a df created with wrangle_catches_with_scenarios())
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
   
    catches_yaxis_label <- sprintf("Catches (%s)", dplyr::last(df$units))

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
            title = catches_yaxis_label, # "SSB",
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
    ) %>% 
        config(modeBarButtonsToAdd = list(data_download_button()))
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
        
        
    fig8 <- ggplotly(pvar) %>% layout(showlegend = FALSE) %>% 
        config(modeBarButtonsToAdd = list(data_download_button()))
}
