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
    type = c("Catches", "Recruitment", "FishingPressure", "StockSize","Custom1", 
    "quality_SSB", "quality_F", "quality_R"), df,
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
        # Determine scaling factor based on StockSizeUnits
        scaling_factor_catches <- get_scaling_factor("CatchesLandingsUnits", df$CatchesLandingsUnits[1])        
        
        # Determine scaling based on Recruitment values
        scaling <- get_scaling(as.numeric(c(df$Catches, df$Landings, df$Discards)), scaling_factor_catches, type = "catches")
        divisor <- scaling$divisor
        suffix <- scaling$suffix
        
        if (is.null(title)) {
          title <- "Catches"
        }
        if (is.null(ylegend)) {
          ylegend <- paste0("Catches (", suffix, ")")
        }

        if (is.null(ymax)) {
          limits <- expand_limits(y = 0)
        } else {
          limits <- expand_limits(y = c(0, as.numeric(ymax)))
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
                "Down-weighted Catches" = "#6eb5d2",
                "RecreationalCatch" = "#eb5c24"
            )),
            limits,
            scale_y_continuous(
                expand = expansion(mult = c(0, 0.1)),
                labels = function(l) l / divisor # Scale labels dynamically
            )#,
            #  scale_x_continuous(breaks = breaks_pretty())
        )
    } else if (type == "Recruitment") {

        # Determine scaling factor based on RecruitmentUnit
        scaling_factor_recruitment <- get_scaling_factor("UnitOfRecruitment", df$UnitOfRecruitment[1])
        
        # Determine scaling based on Recruitment values
        scaling <- get_scaling(as.numeric(df$Recruitment), scaling_factor_recruitment)
        divisor <- scaling$divisor
        suffix <- scaling$suffix

        if (is.null(title)) {
          title <- sprintf("Recruitment <sub>(age %s)</sub>", dplyr::last(df$RecruitmentAge))
        }
        if (is.null(ylegend)) {
          ylegend <- paste0("Recruitment (", suffix, ")")
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
            labels = function(l) l / divisor # Scale labels dynamically
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
                "F<sub>management</sub>" = "#769b8b",
                "HR<sub>management</sub>" = "#769b8b"
                

            )),
            scale_linetype_manual(values = c(
                "FishingPressure" = "solid",
                "F<sub>Lim</sub>" = "dashed",
                "F<sub>pa</sub>" = "dotted",
                "F<sub>MSY</sub>" = "solid",
                "HR MSY<sub>proxy</sub>" = "dotdash",
                "FMSY<sub>proxy</sub>" = "dotdash",
                "F<sub>management</sub>" = "dotdash",
                "HR<sub>management</sub>" = "dotdash"
            )),
            scale_size_manual(values = c(
                "FishingPressure" = 1.5,
                "F<sub>Lim</sub>" = .8,
                "F<sub>pa</sub>" = 1,
                "F<sub>MSY</sub>" = .5,
                "HR MSY<sub>proxy</sub>" = .8,
                "FMSY<sub>proxy</sub>" = .8,
                "F<sub>management</sub>" = .8,
                "HR<sub>management</sub>" = .8
            )),
            scale_fill_manual(values = c("#f2a497")),
            expand_limits(y = 0),
            scale_y_continuous(
                expand = expansion(mult = c(0, 0.1)) # ,
            )#,
            # scale_x_continuous(breaks = breaks_pretty())
        )
    } else if (type == "StockSize") {
        
        # Determine scaling factor based on StockSizeUnits
        scaling_factor_stockSize <- get_scaling_factor("StockSizeUnits", df$StockSizeUnits[1])
                
        
        # Determine scaling based on Recruitment values
        scaling <- get_scaling(as.numeric(c(df$StockSize, df$High_StockSize, df$Low_StockSize)), scaling_factor_stockSize, type = "ssb")
        divisor <- scaling$divisor
        suffix <- scaling$suffix
        
        
        if (is.null(title)) {
          title <- "Spawning Stock Biomass"
        }
        if (is.null(ylegend)) {
          ylegend <- paste0("SSB (", suffix, ")")
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
                "B<sub>management</sub>" = "#769b8b",
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
                "B<sub>management</sub>" = "dotdash",
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
                "B<sub>management</sub>" = .8,
                "BMGT<sub>lower</sub>" = .8,
                "BMGT<sub>upper</sub>" = .8
                            )),
            scale_fill_manual(values = c("#94b0a9","#689dff","#769b8b")),
            # add custom scale for marker size
            limits,
            scale_y_continuous(
                expand = expansion(mult = c(0, 0.1)),
                labels = function(l) l / divisor # Scale labels dynamically
            )#,
            #  scale_x_continuous(breaks = breaks_pretty())
        )

    } else if (type == "Custom1") {
        # Determine scaling factor based on StockSizeUnits
        # scaling_factor_catches <- get_scaling_factor("CatchesLandingsUnits", df$CatchesLandingsUnits[1])        
        
        # # Determine scaling based on Recruitment values
        # scaling <- get_scaling(c(df$Catches, df$Landings, df$Discards), scaling_factor_catches, type = "catches")
        # divisor <- scaling$divisor
        # suffix <- scaling$suffix
        
        if (is.null(title)) {
          title <- ""
        }
        if (is.null(ylegend)) {
          ylegend <- ""
        }

        # if (is.null(ymax)) {
        #   limits <- expand_limits(y = 0)
        # } else {
        #   limits <- expand_limits(y = c(0, ymax))
        # }
        
        theme_ICES_plots <- list(
            tmp,
            labs(
                title = title,
                y = ylegend
            ),
            scale_fill_manual(values = c("#fda500", 
                                            "#002b5f", 
                                            "#00b29d", 
                                            "#6eb200", 
                                            "#6eb5d2", 
                                            "#eb5c24")),
            scale_linetype_manual(values = c("solid", 
                                            "dashed", 
                                            "dotted", 
                                            "dotdash", 
                                            "longdash", 
                                            "twodash")),
            scale_size_manual(values = c(1, 
                                        .8, 
                                        .8, 
                                        .8, 
                                        .8, 
                                        .8))
            # scale_fill_manual(values = scales::hue_pal()(length(unique(selected_data$type))))
            # limits
            # scale_y_continuous(
            #     expand = expansion(mult = c(0, 0.1)),
            #     labels = function(l) l / divisor # Scale labels dynamically
            # ),
            #  scale_x_continuous(breaks = breaks_pretty())
        )
    } else if (type == "quality_SSB") {

        # Determine scaling factor based on StockSizeUnits
        scaling_factor_stockSize <- get_scaling_factor("StockSizeUnits", tail(df$StockSizeUnits, 1))


        # Determine scaling based on Recruitment values
        scaling <- get_scaling(c(df$StockSize, df$High_StockSize, df$Low_StockSize), scaling_factor_stockSize, type = "ssb")
        divisor <- scaling$divisor
        suffix <- scaling$suffix

        if (is.null(title)) {
          title <- "Spawning Stock Biomass"
        }

        if (is.null(ylegend)) {
          ylegend <- paste0("SSB (", suffix, ")") 
        } else {
          if (is.na(ylegend)) ylegend <- ""
          ylabels_func <- function(l) {
            trans <- l
          }
        }

        rfpt <- c( "B<sub>Lim</sub>", "B<sub>pa</sub>","MSY B<sub>trigger</sub>")
        
        line_color <- rev(c("#969696","#737373","#525252","#252525","#047c6c")) %>% head(length(unique(df$AssessmentYear)))
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
                y = ylegend
                
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
                labels = function(l) l / divisor # Scale labels dynamically
            )#,
            # scale_x_continuous(breaks= pretty_breaks())

        )
    } else if (type == "quality_F") {
        rfpt <- c( "F<sub>Lim</sub>","F<sub>pa</sub>", "F<sub>MSY</sub>")

        line_color <- rev(c("#969696","#737373","#525252","#252525","#ed5f26")) %>% head(length(unique(df$AssessmentYear)))
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
        if (is.null(ylegend)) {
          ylegend <- sprintf("%s <sub>(ages %s)</sub>", dplyr::last(df$FishingPressureDescription), dplyr::last(df$FAge))
        }

        theme_ICES_plots <- list(
            tmp,
            labs(
                title = title,
                y = ylegend
               
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

            )#,
            # scale_x_continuous(breaks= pretty_breaks())
        )
    } else if (type == "quality_R") {
        # Determine scaling factor based on RecruitmentUnit
        scaling_factor_recruitment <- get_scaling_factor("UnitOfRecruitment", df$UnitOfRecruitment[1])
        
        # Determine scaling based on Recruitment values
        scaling <- get_scaling(df$Recruitment, scaling_factor_recruitment)
        divisor <- scaling$divisor
        suffix <- scaling$suffix

        if (is.null(title)) {
          title <- sprintf("Recruitment <sub>(age %s)</sub>", dplyr::last(df$RecruitmentAge))
        }
        if (is.null(ylegend)) {
          ylegend <- paste0("Recruitment (", suffix, ")")
        }

        line_type <- sapply(as.character(sort(unique(df$AssessmentYear))), function(x) "solid")
        line_size <- sapply(as.character(sort(unique(df$AssessmentYear))), function(x) 1)
        line_color <- rev(c("#969696","#737373","#525252","#252525","#28b3e8")) %>% head(length(unique(df$AssessmentYear)))
        names(line_color) <- as.character(sort(unique(df$AssessmentYear)))
        
        
        theme_ICES_plots <- list(
            tmp,
            labs(
                title = title,
                y = ylegend
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
                labels = function(l) l / divisor # Scale labels dynamically
            )#,
            # scale_x_continuous(breaks= pretty_breaks())
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
# data_download_button <- function(disclaimer_text) {

#     icon_svg_path = "M15.608,6.262h-2.338v0.935h2.338c0.516,0,0.934,0.418,0.934,0.935v8.879c0,0.517-0.418,0.935-0.934,0.935H4.392c-0.516,0-0.935-0.418-0.935-0.935V8.131c0-0.516,0.419-0.935,0.935-0.935h2.336V6.262H4.392c-1.032,0-1.869,0.837-1.869,1.869v8.879c0,1.031,0.837,1.869,1.869,1.869h11.216c1.031,0,1.869-0.838,1.869-1.869V8.131C17.478,7.099,16.64,6.262,15.608,6.262z M9.513,11.973c0.017,0.082,0.047,0.162,0.109,0.226c0.104,0.106,0.243,0.143,0.378,0.126c0.135,0.017,0.274-0.02,0.377-0.126c0.064-0.065,0.097-0.147,0.115-0.231l1.708-1.751c0.178-0.183,0.178-0.479,0-0.662c-0.178-0.182-0.467-0.182-0.645,0l-1.101,1.129V1.588c0-0.258-0.204-0.467-0.456-0.467c-0.252,0-0.456,0.209-0.456,0.467v9.094L8.443,9.553c-0.178-0.182-0.467-0.182-0.645,0c-0.178,0.184-0.178,0.479,0,0.662L9.513,11.973z"

#     dl_button <- list(
#         name = "Download data",
#         icon = list(
#             path = icon_svg_path,
#             transform = "scale(0.84) translate(-1, -1)"
#         ),
#         click = htmlwidgets::JS(paste0("
#             function(gd) {
#                 console.log(gd.data);
#                 var text = '';
#                 for(var i = 0; i < gd.data.length; i++) {
#                     text += gd.layout.xaxis.title.text + gd.data[i].name + ',' + gd.data[i].x + '\\n';
#                     text += gd.layout.yaxis.title.text + gd.data[i].name + ',' + gd.data[i].y + '\\n';
#                 };

#                 // Add the disclaimer to the text
#                 text += '\\nDisclaimer: ' + '", disclaimer_text, "' + '\\n';

#                 var blob = new Blob([text], {type: 'text/plain'});
#                 var a = document.createElement('a');
#                 const object_URL = URL.createObjectURL(blob);
#                 a.href = object_URL;
#                 a.download = 'data.csv';
#                 document.body.appendChild(a);
#                 a.click();
#                 URL.revokeObjectURL(object_URL);
#             }
#         "))
#     )

#     return(dl_button)
# }



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
ICES_plot_1 <- function(df, sagSettings, sagStamp) {
    # If df$UnitOfRecruitment is empty, set it to NA
    
    if (is.na(df$CatchesLandingsUnits[1])) {
        df$CatchesLandingsUnits <- "empty"
    }
    scaling_factor_catches <- get_scaling_factor("CatchesLandingsUnits", df$CatchesLandingsUnits[1])


    sagSettings1 <- sagSettings %>% filter(SAGChartKey == 1)

    
    additionalCustomeSeries <-
        sagSettings1 %>%
        filter(settingKey == 43) %>%
        pull(settingValue) %>%
        as.numeric() %>%
        nullifempty()
    
    
    df1 <- process_dataframe_catches(df, additionalCustomeSeries, scaling_factor_catches)
    
    
    shadeYears <- sagSettings1 %>%
        filter(settingKey == 14) %>%
        pull(settingValue) %>%
        str_split(pattern = ",", simplify = TRUE) %>%
        as.numeric()

    OnlyCatches <-
        sagSettings1 %>%
        filter(settingKey == 32) %>%
        pull(settingValue) %>%
        nullifempty()
    
    if (is_na_column(df1, "Landings") || !is.null(OnlyCatches)) {        
        df1 <- df1 %>%
            select(-Landings) %>%
            # gather(type, count, Catches:`Unallocated Removals`)
            pivot_longer(cols = Catches:`Unallocated Removals`,
                 names_to = "type",
                 values_to = "count")
    } else if (!is.null(additionalCustomeSeries)) {
        df1 <- df1 %>%
            select(-Catches) %>%
            # gather(type, count, Landings:`Unallocated Removals`)
            pivot_longer(cols = Landings:`Unallocated Removals`,
                 names_to = "type",
                 values_to = "count")
    } else {
        df1 <- df1 %>%
            select(-Catches) %>%
            # gather(type, count, Landings:`Unallocated Removals`)
            pivot_longer(cols = Landings:`Unallocated Removals`,
                 names_to = "type",
                 values_to = "count")
    }
    
    ## remove rows with NA in count
    df1 <- df1 %>% filter(!is.na(count))

        
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
        p1 <- p1 + geom_bar(
            stat = "identity",
            data = df1 %>% filter(Year %in% shadeYears),
            aes(
                x = Year,
                y = count,
                fill = "Down-weighted Catches",
                text = map(
                    paste0(
                        "<b>Year: </b>", Year,
                        "<br>",
                        "<b>Down-weighted or preliminary Catches: </b>", count
                    ), HTML
                )
            ),
            alpha = 0.5,
            show.legend = FALSE,
            inherit.aes = FALSE
        )
    }


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
                text = sagStamp,
                font = list(family = "Calibri, serif", size = 12, color = "#acacac"),
                yref = "paper", y = 1.05, 
                xref = "paper", x = 1,
                yanchor = "right", xanchor = "right"
            )
        )
    
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
ICES_plot_2 <- function(df, sagSettings, sagStamp) {
    
    # If df$UnitOfRecruitment is empty, set it to NA
    if (is.na(df$UnitOfRecruitment[1])) {
        df$UnitOfRecruitment <- "empty"
    }
    
    # Determine scaling factor based on RecruitmentUnit
    scaling_factor_recruitment <- get_scaling_factor("UnitOfRecruitment", df$UnitOfRecruitment[1])
    
  
    df2 <- df %>%
        filter(Purpose == "Advice") %>%
        select(Year, Recruitment, Low_Recruitment, High_Recruitment, UnitOfRecruitment, RecruitmentAge) %>% # , SAGStamp
        mutate(
            Year = as.numeric(Year),
            Recruitment = as.numeric(Recruitment) * scaling_factor_recruitment,
            Low_Recruitment = as.numeric(Low_Recruitment) * scaling_factor_recruitment,
            High_Recruitment = as.numeric(High_Recruitment) * scaling_factor_recruitment
        )

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
                text = sagStamp,
                font = list(family = "Calibri, serif", size = 12, color = "#acacac"),
                yref = "paper", y = 1.05,
                xref = "paper", x = 1,
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
ICES_plot_3 <- function(df, sagSettings, sagStamp) {

    processed <- process_dataframe_F(df, sagSettings)
    
    # Filter out rows with NAs and create a segment identifier
    df_segments <- processed$df3 %>%
        filter(!is.na(FishingPressure)) %>%
        group_by(segment) %>%
        mutate(start = first(Year), end = last(Year))


    p3 <- df_segments %>%
        ggplot(., aes(x = Year, y = FishingPressure))
    
    if (any(!is.na(df_segments$Low_FishingPressure))) {
        p3 <- p3 +
            geom_ribbon(
                data = df_segments %>% filter(!is.na(Low_FishingPressure) & !is.na(High_FishingPressure)), aes(
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
                alpha = 0.8,
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

    if (any(!is.na(df_segments$Fmanagement)) && length(processed$customRefPoint) != 0 && processed$customRefPoint == "Fmanagement") {
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


    if (any(!is.na(df_segments$HRMGT)) && length(processed$customRefPoint) != 0 && processed$customRefPoint == "HRMGT") {
        p3 <- p3 +
            geom_line(aes(
                x = Year,
                y = HRMGT,
                linetype = "HR<sub>management</sub>",
                colour = "HR<sub>management</sub>",
                size = "HR<sub>management</sub>",
                text = map(
                    paste0(
                        "<b>HR<sub>management</sub>: </b>", tail(HRMGT, 1)
                    ), HTML
                )
            ))
    }

    # custom reference points 1    
        if (any(!is.na(df_segments[[paste0("CustomRefPointValue", processed$customRefPoint[1])]])) && !processed$customRefPoint[1] %in% colnames(df) && grepl("^[0-5]$", processed$customRefPoint[1])) {
            p3 <- p3 +
                geom_line(aes(
                    x = Year,
                    y = df_segments[[paste0("CustomRefPointValue", processed$customRefPoint[1])]],
                    linetype = df_segments[[paste0("CustomRefPointName", processed$customRefPoint[1])]][1],
                    colour = df_segments[[paste0("CustomRefPointName", processed$customRefPoint[1])]][1],
                    size = df_segments[[paste0("CustomRefPointName", processed$customRefPoint[1])]][1],
                    text = map(
                        paste0(
                            "<b>", df_segments[[paste0("CustomRefPointName", processed$customRefPoint[1])]][1], ": </b>", tail(df_segments[[paste0("CustomRefPointValue", processed$customRefPoint[1])]], 1)
                        ), HTML
                    )
                ))
        }

    # custom reference points 2
    if (any(!is.na(df_segments[[paste0("CustomRefPointValue", processed$customRefPoint[2])]])) && !processed$customRefPoint[2] %in% colnames(df) && grepl("^[0-5]$", processed$customRefPoint[2])) {
        p3 <- p3 +
            geom_line(aes(
                x = Year,
                y = df_segments[[paste0("CustomRefPointValue", processed$customRefPoint[2])]],
                linetype = df_segments[[paste0("CustomRefPointName", processed$customRefPoint[2])]][1],
                colour = df_segments[[paste0("CustomRefPointName", processed$customRefPoint[2])]][1],
                size = df_segments[[paste0("CustomRefPointName", processed$customRefPoint[2])]][1],
                text = map(
                    paste0(
                        "<b>", df_segments[[paste0("CustomRefPointName", processed$customRefPoint[2])]][1], ": </b>", tail(df_segments[[paste0("CustomRefPointValue", processed$customRefPoint[2])]], 1)
                    ), HTML
                )
            ))
    }

    # custom reference points 3   
        if (any(!is.na(df_segments[[paste0("CustomRefPointValue", processed$customRefPoint[3])]])) && !processed$customRefPoint[3] %in% colnames(df) && grepl("^[0-5]$", processed$customRefPoint[3])) {
            p3 <- p3 +
                geom_line(aes(
                    x = Year,
                    y = df_segments[[paste0("CustomRefPointValue", processed$customRefPoint[3])]],
                    linetype = df_segments[[paste0("CustomRefPointName", processed$customRefPoint[3])]][1],
                    colour = df_segments[[paste0("CustomRefPointName", processed$customRefPoint[3])]][1],
                    size = df_segments[[paste0("CustomRefPointName", processed$customRefPoint[3])]][1],
                    text = map(
                        paste0(
                            "<b>", df_segments[[paste0("CustomRefPointName", processed$customRefPoint[3])]][1], ": </b>", tail(df_segments[[paste0("CustomRefPointValue", processed$customRefPoint[3])]], 1)
                        ), HTML
                    )
                ))
        }

    


    # custom data time series
    if (!is.null(processed$additionalCustomeSeries) && !is.na(processed$additionalCustomeSeries) && length(processed$new_name) != 0) {
        p3 <- p3 +
            geom_line(data = df_segments, aes(
                x = Year,
                y = df_segments[[processed$new_name]],
                color = processed$new_name,
                group = segment,
                text = map(
                    paste0(
                        "<b>Year: </b>", Year,
                        "<br>",
                        "<b>", processed$new_name,": </b>", df_segments[[processed$new_name]]
                    ), HTML
                )
            ))
    }


    diamondYears <-
        processed$sagSettings3 %>%
        filter(settingKey == 14) %>%
        pull(settingValue) %>%
        str_split(pattern = ",", simplify = TRUE) %>%
        as.numeric()

    if (any(!is.na(diamondYears))) {
        p3 <- p3 + geom_point(
            data = df_segments %>% filter(Year %in% diamondYears),
            aes(
                x = Year,
                y = FishingPressure,
                text = map(
                    paste0(
                        "<b>Year: </b>", Year,
                        "<br>",
                        "<b>Estimated fishing pressure: </b>", FishingPressure
                    ), HTML
                )
            ),
            shape = 23,
            fill = "#cfcfcf",
            color = "#ed5f26",
            size = 2.5,
            show.legend = FALSE,
            inherit.aes = FALSE
        )
    }




    min_year <- min(df_segments$Year[which(!is.na(df_segments$FishingPressure))])
    nullifempty <- function(x) if (length(x) == 0) NULL else x

    p3 <-
        p3 +
        xlim(min_year, max(df_segments$Year)) +
        theme_ICES_plots(
            type = "FishingPressure", df_segments,
            title = processed$sagSettings3 %>% filter(settingKey == 1) %>% pull(settingValue) %>% nullifempty(),
            ylegend = processed$sagSettings3 %>% filter(settingKey == 20) %>% pull(settingValue) %>% replace_subscript_symbols(.)  %>% nullifempty()
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
                text = sagStamp,
                font = list(family = "Calibri, serif", size = 12, color = "#acacac"),
                yref = "paper", y = 1.05,
                xref = "paper", x = 1,
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
ICES_plot_4 <- function(df, sagSettings, sagStamp) {
    # nullifempty <- function(x) if (length(x) == 0) NULL else x
    # If df$UnitOfRecruitment is empty, set it to NA
    if (is.na(df$StockSizeUnits[1])) {
        df$StockSizeUnits <- "empty"
    }
    
    scaling_factor_stockSize <- get_scaling_factor("StockSizeUnits", df$StockSizeUnits[1])    
       
    processed <- process_dataframe_SSB(df, sagSettings, scaling_factor_stockSize)

   

    # Filter out rows with NAs and create a segment identifier
    df_segments <- processed$df4 %>%
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
                    fill = as.character(ConfidenceIntervalDefinition),
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
                alpha = 0.8,
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
    
    p4 <- p4 +
        geom_point(data = df_segments[df_segments$show_error, ], aes(
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
    # p4 <- p4 +
    #     geom_errorbar(data = df_segments[df_segments$show_error, ], aes(
            
    #         y = StockSize,
    #         ymin = Low_StockSize,
    #         ymax = High_StockSize,
    #         color = "StockSize",
    #         group = segment,
    #         orientation = "y",
    #         text = map(
    #             paste0(
    #                 "<b>Year: </b>", Year,
    #                 "<br>",
    #                 "<b>", StockSizeDescription,": </b>", StockSize
    #             ), HTML
    #         )
    #     ))

              
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
    if (any(!is.na(df_segments$Bmanagement)) && length(processed$customRefPoint) != 0 && processed$customRefPoint == "Bmanagement") {
        p4 <- p4 +
            geom_line(aes(
                x = Year,
                y = Bmanagement,
                linetype = "B<sub>management</sub>",
                colour = "B<sub>management</sub>",
                size = "B<sub>management</sub>",
                text = map(
                    paste0(
                        "<b>B<sub>management</sub>: </b>", tail(Bmanagement, 1)
                    ), HTML
                )
            ))
    }
   
    if (any(!is.na(df_segments$BMGT_lower)) && length(processed$customRefPoint) != 0 && any(processed$customRefPoint == "BMGT_lower")) {
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

    if (any(!is.na(df_segments$BMGT_upper)) && length(processed$customRefPoint) != 0 && any(processed$customRefPoint == "BMGT_upper")) {
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

    # custom reference point 1
    if (any(!is.na(df_segments[[paste0("CustomRefPointValue", processed$customRefPoint[1])]])) && !all(processed$customRefPoint[1] %in% colnames(df)) && grepl("^[0-5]$", processed$customRefPoint[1])) {
        p4 <- p4 +
            geom_line(aes(
                x = Year,
                y = df_segments[[paste0("CustomRefPointValue", processed$customRefPoint[1])]],
                linetype = df_segments[[paste0("CustomRefPointName", processed$customRefPoint[1])]][1],
                colour = df_segments[[paste0("CustomRefPointName", processed$customRefPoint[1])]][1],
                size = df_segments[[paste0("CustomRefPointName", processed$customRefPoint[1])]][1],
                text = map(
                    paste0(
                        "<b>", df_segments[[paste0("CustomRefPointName",processed$customRefPoint[1])]][1], ": </b>", tail(df_segments[[paste0("CustomRefPointValue", processed$customRefPoint[1])]], 1)
                    ), HTML
                )
            ))
    }

    # custom reference point 2
    if (any(!is.na(df_segments[[paste0("CustomRefPointValue", processed$customRefPoint[2])]])) && !all(processed$customRefPoint[2] %in% colnames(df)) && grepl("^[0-5]$", processed$customRefPoint[2])) {
        p4 <- p4 +
            geom_line(aes(
                x = Year,
                y = df_segments[[paste0("CustomRefPointValue", processed$customRefPoint[2])]],
                linetype = df_segments[[paste0("CustomRefPointName", processed$customRefPoint[2])]][1],
                colour = df_segments[[paste0("CustomRefPointName", processed$customRefPoint[2])]][1],
                size = df_segments[[paste0("CustomRefPointName", processed$customRefPoint[2])]][1],
                text = map(
                    paste0(
                        "<b>", df_segments[[paste0("CustomRefPointName", processed$customRefPoint[2])]][1], ": </b>", tail(df_segments[[paste0("CustomRefPointValue", processed$customRefPoint[2])]], 1)
                    ), HTML
                )
            ))
    }

    # custom reference point 2
    if (any(!is.na(df_segments[[paste0("CustomRefPointValue", processed$customRefPoint[3])]])) && !all(processed$customRefPoint[3] %in% colnames(df)) && grepl("^[0-5]$", processed$customRefPoint[3])) {
        p4 <- p4 +
            geom_line(aes(
                x = Year,
                y = df_segments[[paste0("CustomRefPointValue", processed$customRefPoint[3])]],
                linetype = df_segments[[paste0("CustomRefPointName", processed$customRefPoint[3])]][1],
                colour = df_segments[[paste0("CustomRefPointName", processed$customRefPoint[3])]][1],
                size = df_segments[[paste0("CustomRefPointName", processed$customRefPoint[3])]][1],
                text = map(
                    paste0(
                        "<b>", df_segments[[paste0("CustomRefPointName", processed$customRefPoint[3])]][1], ": </b>", tail(df_segments[[paste0("CustomRefPointValue", processed$customRefPoint[3])]], 1)
                    ), HTML
                )
            ))
    }
   
    # custom data time series
    if (!is.null(processed$additionalCustomeSeries) && !is.na(processed$additionalCustomeSeries) && length(processed$new_name) != 0) {
        p4 <- p4 +
            geom_line(data = df_segments, aes(
                x = Year,
                y = df_segments[[processed$new_name]] * scaling_factor_stockSize,
                color = processed$new_name,
                group = segment,
                text = map(
                    paste0(
                        "<b>Year: </b>", Year,
                        "<br>",
                        "<b>", processed$new_name,": </b>", df_segments[[processed$new_name]]
                    ), HTML
                )
            ))
    }

    diamondYears <-
        processed$sagSettings4 %>%
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
            color = "#047c6c",
            size = 2.5,
            show.legend = FALSE,
            inherit.aes = FALSE
        )
    }


    # add average lines
    averageYears <-
        processed$sagSettings4 %>%
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
    

    p4 <-
        p4 +
        xlim(min_year, max(df_segments$Year)) +
        theme_ICES_plots(
            type = "StockSize", df_segments,
            title = processed$sagSettings4 %>% filter(settingKey == 1) %>% pull(settingValue) %>% nullifempty(),
            ylegend = processed$sagSettings4 %>% filter(settingKey == 20) %>% pull(settingValue) %>% as.character() %>% replace_subscript_symbols(.) %>% nullifempty(),
            ymax = processed$sagSettings4 %>% filter(settingKey == 6) %>% pull(settingValue) %>% as.numeric() %>% nullifempty()
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
                text = sagStamp,
                font = list(family = "Calibri, serif", size = 12, color = "#acacac"),
                yref = "paper", y = 1.05, 
                xref = "paper", x = 1,
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


ICES_custom_plot <- function(df, sagSettings, ChartKey, sagStamp) {
    sagSettingsCustom <- sagSettings %>% filter(SAGChartKey == ChartKey)
   
    # Extract custom data and graph type
    customData <- sagSettingsCustom %>%
        filter(settingKey == 44) %>%
        pull(settingValue) %>%
        str_split(",", simplify = TRUE) %>%
        as.numeric()

    # Extract custom reference points
    customRefPoint <-
        sagSettingsCustom %>%
        filter(settingKey == 51) %>%
        pull(settingValue) %>%
        standardiseRefPoints(.) %>%
        str_split(pattern = ",", simplify = TRUE) %>% 
        as.numeric()
    

    graphType <- sagSettingsCustom %>%
        filter(settingKey == 50) %>%
        pull(settingValue) %>%
        as.numeric()

    # Create regex patterns for selecting columns
    patternValues <- paste0("Custom(", paste(customData, collapse = "|"), ")$")
    patternNames <- paste0("CustomName(", paste(customData, collapse = "|"), ")$")
    patternValuesRefPoint <- paste0("CustomRefPointValue(", paste(customRefPoint, collapse = "|"), ")$")
    patternNamesRefPoint <- paste0("CustomRefPointName(", paste(customRefPoint, collapse = "|"), ")$")
    
    # Select relevant columns
    selected_data <- df %>%
        arrange(Year) %>%
        select(c(Year, matches(patternValues), matches(patternNames))) %>% 
        mutate(Year = as.numeric(Year)) %>% 
        mutate_at(vars(matches(patternValues)), as.numeric)
        # mutate(segment = cumsum(is.na(matches(patternValues))))
    
    custom_cols <- grep("^Custom[0-9]+$", names(selected_data), value = TRUE)
    custom_name_cols <- grep("^CustomName[0-9]+$", names(selected_data), value = TRUE)
    # custom_cols_ref <- grep("^CustomRefPointValue[0-9]+$", names(selected_data), value = TRUE)
    # custom_name_cols_ref <- grep("^CustomRefPointName[0-9]+$", names(selected_data), value = TRUE)
    
    custom_cols <- sort(custom_cols)
    custom_name_cols <- sort(custom_name_cols)

    # Replace custom column names with extracted names
    first_col_index <- which(names(selected_data) == custom_name_cols[1])
    last_col_index <- ncol(selected_data)
    first_row_values <- selected_data[1, first_col_index:last_col_index]
    names(selected_data)[which(names(selected_data) %in% custom_cols)] <- as.character(first_row_values)
    
    # if in the column names there is any NA replace with trace1, trace2, etc.
    names(selected_data)[is.na(names(selected_data))] <- paste0("trace", seq_along(names(selected_data)[is.na(names(selected_data))]))

    # Remove custom name columns and reshape data
    selected_data <- selected_data %>%
        select(-custom_name_cols) %>%
        gather(type, count, -Year)
    
    # Include reference points in the plot
    ref_points <- df %>% select(Year, matches(patternValuesRefPoint),matches(patternNamesRefPoint))  %>% 
        filter(Year >= min(selected_data$Year[which(!is.na(selected_data$count))])) %>% 
        mutate_at(vars(matches(patternValuesRefPoint)), as.numeric) %>% 
        mutate(Year = as.numeric(Year))
    
    
    # Determine the number of series available
    num_series <- length(unique(selected_data$type))

    # Reshape data to wide format for easier series management
    selected_data_wide <- selected_data %>%
        spread(type, count)
        
    
    # Dynamically rename columns for clarity
    series_names <- colnames(selected_data_wide)[-1] # Exclude 'Year' column
    
    if (num_series == 1) {
        names(selected_data_wide)[2] <- "Series1"        
    }
    if (num_series >= 2) {
        names(selected_data_wide)[2] <- "Series1"
        names(selected_data_wide)[3] <- "Series2"
    }
    if (num_series == 3) {
        names(selected_data_wide)[4] <- "Series3"
    }
    
    # Add segment column to identify single data points
    selected_data_wide <- selected_data_wide %>%
        mutate(
            segment = cumsum(is.na(Series1)),
            is_single_value_in_segment = ave(!is.na(Series1), segment, FUN = function(x) sum(x) == 1),
            show_error = !is.na(Series1) & is_single_value_in_segment
        ) %>% 
        na.omit()
    
    # Initialize base plot
    pCustom <- ggplot(selected_data_wide, aes(x = Year))

    # Logic for graphType == 1 (Standard line plot)
    if (graphType == 1) {
        pCustom <- pCustom +
            geom_line(aes(
                x = Year,
                y = Series1,
                color = series_names[1],
                group = segment,
                text = map(
                    paste0(
                        "<b>Year: </b>", Year,
                        "<br>",
                        "<b>", series_names[1], ": </b>", Series1
                    ), HTML
                )
            ))
    } else if (graphType == 2) {
        if (num_series == 1) {
            # Single data series → plot as line
            pCustom <- pCustom +
                geom_line(aes(
                    y = Series1,
                    color = series_names[1],
                    group = segment,
                    text = map(
                        paste0(
                            "<b>Year: </b>", Year,
                            "<br>",
                            "<b>", series_names[1], ": </b>", Series1
                        ), HTML
                    )
                ))
        } else if (num_series == 2) {
            # Two data series → plot as two lines
            pCustom <- pCustom +
                geom_line(aes(
                    y = Series1,
                    color = series_names[1],
                    group = segment,
                    text = map(
                        paste0(
                            "<b>Year: </b>", Year,
                            "<br>",
                            "<b>", series_names[1], ": </b>", Series1
                        ), HTML
                    )
                )) +
                geom_line(aes(
                    y = Series2,
                    color = series_names[2],
                    group = segment,
                    text = map(
                        paste0(
                            "<b>Year: </b>", Year,
                            "<br>",
                            "<b>", series_names[2], ": </b>", Series2
                        ), HTML
                    )
                ))
        } else if (num_series == 3) {
            # Three data series → plot ribbon (shaded area) with middle line
            pCustom <- pCustom +
                geom_ribbon(
                    aes(
                        ymin = Series2,
                        ymax = Series3,
                        fill = "sd",
                        group = segment,
                        text = map(
                        paste0(
                            "<b>Year: </b>", Year,
                            "<br>",
                            "<b>High ", series_names[3],": </b>", Series3,
                            "<br>",
                            "<b>Low", series_names[2],": </b>", Series2
                        ), HTML
                    )
                    ),
                    alpha = 0.4
                ) +
                geom_line(aes(
                    y = Series1,
                    color = series_names[1],
                    group = segment,
                    text = map(
                        paste0(
                            "<b>Year: </b>", Year,
                            "<br>",
                            "<b>", series_names[1], ": </b>", Series1
                        ), HTML
                    )
                )) #+
            # geom_point(aes(y = Series1, color = series_names[1]), size = 2, data = selected_data_wide[selected_data_wide$show_error, ]) +
            # geom_errorbar(aes(y = Series1, ymin = Series2, ymax = Series3, color = series_names[1]), width = 0.2, data = selected_data_wide[selected_data_wide$show_error, ])
        }
        # Logic for graphType == 3 (Bar plot)
    } else if (graphType == 3 || graphType == 4) {        

        # Determine scaling based on Recruitment values
        if (is.na(df$CatchesLandingsUnits[1])) {
            df$CatchesLandingsUnits <- "empty"
        }
        scaling_factor_catches <- get_scaling_factor("CatchesLandingsUnits", df$CatchesLandingsUnits[1])        
        
        
        scaling <- get_scaling(as.numeric(c(df$Catches, df$Landings, df$Discards)), scaling_factor_catches, type = "catches")
        divisor <- scaling$divisor
        suffix <- scaling$suffix

        pCustom <- ggplot(selected_data, aes(
            x = Year,
            text = map(
                paste0(
                    "<b>Year: </b>", Year,
                    "<br>",
                    "<b>", type, ": </b>", count
                ), HTML
            )
        ))

        pCustom <- pCustom +
            geom_bar(
                data = selected_data,
                aes(
                    y = count,
                    fill = type,
                ),
                position = "stack",
                stat = "identity"
                
            ) +
            scale_y_continuous(
                expand = expansion(mult = c(0, 0.1)),
                labels = function(l) l / divisor #divisor # Scale labels dynamically
            )
    }
    
    # custom reference point 1
    if (any(!is.na(ref_points[[paste0("CustomRefPointValue", customRefPoint[1])]])) && !all(customRefPoint[1] %in% colnames(df)) && grepl("^[0-5]$", customRefPoint[1])) {
        pCustom <- pCustom +
            geom_line(data = ref_points, aes(
                x = Year,
                y = ref_points[[paste0("CustomRefPointValue", customRefPoint[1])]],
                linetype = ref_points[[paste0("CustomRefPointName", customRefPoint[1])]][1],
                colour = ref_points[[paste0("CustomRefPointName", customRefPoint[1])]][1],
                size = ref_points[[paste0("CustomRefPointName", customRefPoint[1])]][1],
                text = map(
                    paste0(
                        "<b>", ref_points[[paste0("CustomRefPointName",customRefPoint[1])]][1], ": </b>", tail(ref_points[[paste0("CustomRefPointValue", customRefPoint[1])]], 1)
                    ), HTML
                )
            ))
    }
    
    
    
    
    # Nullify empty values
    nullifempty <- function(x) if (length(x) == 0) NULL else x
    # Add themes and labels
     pCustom <- pCustom +
        xlim(min(selected_data$Year[which(!is.na(selected_data$count))]), max(selected_data$Year)) +
        theme_ICES_plots(
            type = "Custom1", df,
            title = sagSettingsCustom %>%
                filter(settingKey == 1) %>%
                pull(settingValue) %>%
                nullifempty(),
            ylegend = sagSettingsCustom %>%
                filter(settingKey == 20) %>%
                pull(settingValue) %>%
                as.character() %>%
                nullifempty(),
            ymax = sagSettingsCustom %>%
                filter(settingKey == 6) %>%
                pull(settingValue) %>%
                as.numeric() %>%
                nullifempty()
        )

    # Convert to plotly for interactivity
    figC2 <- ggplotly(pCustom, tooltip = "text") %>%
        layout(
            autosize = TRUE,
            legend = list(
                itemsizing = "trace",
                orientation = "h",
                y = -0.3,
                yanchor = "bottom",
                x = 0.5,
                xanchor = "center",
                title = list(text = "")
            ),
            xaxis = list(zeroline = TRUE),
            yaxis = list(
                rangemode='tozero',
                zeroline = TRUE
            ),
            annotations = list(
                showarrow = FALSE,
                text = sagStamp,
                font = list(family = "Calibri, serif", size = 12, color = "#acacac"),
                yref = "paper", y = 1.05, 
                xref = "paper", x = 1,
                yanchor = "right", xanchor = "right"
            )
        )

    # Clean legend names
    for (i in seq_along(figC2$x$data)) {
        if (!is.null(figC2$x$data[[i]]$name)) {
            figC2$x$data[[i]]$name <- gsub("\\(", "", str_split(figC2$x$data[[i]]$name, ",")[[1]][1])
        }
    }

    return(figC2)
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
ICES_plot_5 <- function(df, sagSettings, sagStamp) {
    
    sagSettings4 <- sagSettings %>% filter(SAGChartKey == 4)

    # If df$UnitOfRecruitment is empty, set it to NA
    if (df$StockSizeUnits[1] == "") {
        df$StockSizeUnits <- "empty"
    }
    
    scaling_factor_stockSize <- get_scaling_factor("StockSizeUnits", df$StockSizeUnits[1])
    

    if (is.na(tail(df$StockSizeUnits, 1))) {
        df$StockSizeUnits <- "empty"
    }
    
    scaling_factor_stockSize <- get_scaling_factor("StockSizeUnits", tail(df$StockSizeUnits, 1))
    
    df5 <- df %>%
        filter(Purpose == "Advice") %>%
        select(Year, AssessmentYear, StockSize, Blim, Bpa, MSYBtrigger, StockSizeDescription, StockSizeUnits) %>%  #%>% , SAGStamp
        arrange(desc(AssessmentYear))
    
    
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
                text = sagStamp,
                font = list(family = "Calibri, serif", size = 12, color = "#acacac"),
                yref = "paper", y = 1.05, 
                xref = "paper", x = 1,
                yanchor = "right", xanchor = "right")
        )
        

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
ICES_plot_6 <- function(df, sagSettings, sagStamp) {
    sagSettings3 <- sagSettings %>% filter(SAGChartKey == 3)

    df6 <- df %>%
        filter(Purpose == "Advice") %>%
        select(Year, FishingPressure, Flim, Fpa, FMSY, FAge, FishingPressureDescription, AssessmentYear) %>%  # %>% , SAGStamp
        arrange(desc(AssessmentYear))
        
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
                text = sagStamp,
                font = list(family = "Calibri, serif", size = 12, color = "#acacac"),
                yref = "paper", y = 1.05, 
                xref = "paper", x = 1,
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
ICES_plot_7 <- function(df, sagSettings, sagStamp) {

    # If df$UnitOfRecruitment is empty, set it to NA
    if (is.na(df$UnitOfRecruitment[1])) {
        df$UnitOfRecruitment <- "empty"
    }
    
    # Determine scaling factor based on RecruitmentUnit
    scaling_factor_recruitment <- get_scaling_factor("UnitOfRecruitment", df$UnitOfRecruitment[1])
    
  
    sagSettings2 <- sagSettings %>% filter(SAGChartKey == 2)

    p7 <- df %>% filter(Purpose == "Advice") %>%
        select(Year, Recruitment, RecruitmentAge, AssessmentYear) %>%  #, SAGStamp
        arrange(desc(AssessmentYear)) %>%
        drop_na(Recruitment) %>%
        mutate(Recruitment = Recruitment * scaling_factor_recruitment
               ) %>%
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

        # nullifempty <- function(x) if (length(x) == 0) NULL else x

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
                text = sagStamp,
                font = list(family = "Calibri, serif", size = 12, color = "#acacac"),
                yref = "paper", y = 1.05, 
                xref = "paper", x = 1,
                yanchor = "right", xanchor = "right")
        ) 

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
        traceorder = "grouped"
    )
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
#' shorten_labels(tmp$Scenario)
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
        ggradar(tmp %>% select(-cS_Purpose) %>% filter(Scenario %in% catch_scenarios),
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
        tooltip = c("Scenario")
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
    
    
    tmp$fmsy <- as.numeric(tail(df$FMSY,1))
    tmp$blim <- as.numeric(tail(df$Blim,1))
    
    

    
    Basis <- tmp[tmp$cS_Purpose == "Basis Of Advice",]
    tmp <- tmp[tmp$cS_Purpose == "Other Scenarios",] %>% 
      dplyr::filter(TotCatch != Basis$TotCatch) %>% 
      dplyr::bind_rows(Basis)
    
    
    # Function to check if a column is made up of all NA values
    # is_na_column <- function(dataframe, col_name) {
    #     return(all(is.na(dataframe[, col_name])))
    # }
    
    if (is_na_column(tmp, "F")){
        tmp <- arrange(tmp, F_wanted)

        labels <- sprintf(
            "Catch Scenario: %s", tmp$Scenario
        ) %>% lapply(htmltools::HTML)

        fig_catch <- plot_ly(tmp) %>%
        add_trace(
            x = ~ TotCatch,
            y = ~ F_wanted,
            type = "scatter",
            mode = "lines+markers",
            text = labels,
            marker = list(color = "#ed5f26", size = 15, symbol = "diamond"),
            line = list(color = "#ed5f26", width = 5, dash = 'solid'),
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
            marker = list(color = "#000000", size = 20, symbol = "diamond-open"),
            text = "Basis of advice",
            name = "Basis of advice",
            showlegend = FALSE
        )

    if (is_na_column(tmp, "F_wanted")){
        tmp <- arrange(tmp, HR)

        labels <- sprintf(
            "Catch Scenario: %s", tmp$Scenario
        ) %>% lapply(htmltools::HTML)

        fig_catch <- plot_ly(tmp) %>%
        add_trace(
            x = ~ TotCatch,
            y = ~ HR,
            type = "scatter",
            mode = "lines+markers",
            text = labels,
            marker = list(color = "#ed5f26", size = 15, symbol = "diamond"),
            line = list(color = "#ed5f26", width = 5, dash = 'solid'),
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
            marker = list(color = "#000000", size = 20, symbol = "diamond-open"),
            text = "Basis of advice",
            name = "Basis of advice",
            showlegend = FALSE
        )
    
    }
    } else {
        tmp <- arrange(tmp, F)

        labels <- sprintf(
            "Catch Scenario: %s", tmp$Scenario
        ) %>% lapply(htmltools::HTML)

        fig_catch <- plot_ly(tmp) %>%
        add_trace(
            x = ~ TotCatch,
            y = ~ F,
            type = "scatter",
            mode = "lines+markers",
            text = labels,            
            marker = list(color = "#ed5f26", size = 15, symbol = "diamond"),
            line = list(color = "#ed5f26", width = 5, dash = 'solid'),
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
            marker = list(color = "#000000", size = 20, symbol = "diamond-open"),
            text = "Basis of advice",
            name = "Basis of advice",
            showlegend = FALSE
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
                    line = list(color = "#047c6c", width = 5, dash = "solid"),
                    marker = list(color = "#047c6c", size = 15, symbol = "diamond"),
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
                    marker = list(color = "#000000", size = 20, symbol = "diamond-open"),
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
    tmp$fmsy <- as.numeric(tail(df$FMSY,1))
    tmp$blim <- as.numeric(tail(df$Blim,1))
    

    
    tmp$Scenario <- shorten_labels(tmp$Scenario)
    Basis <- tmp[tmp$cS_Purpose == "Basis Of Advice", ]
    tmp <- tmp[tmp$cS_Purpose == "Other Scenarios",] %>% 
      dplyr::filter(TotCatch != Basis$TotCatch) %>% 
      dplyr::bind_rows(Basis)
    
        
    if (is_na_column(tmp, "F")) {
        tmp <- arrange(tmp, F_wanted)
        labels <- sprintf(
            "Catch Scenario: %s", tmp$Scenario
        ) %>% lapply(htmltools::HTML)
        fig_catch <- plot_ly(tmp) %>%
            add_trace(
                x = ~TotCatch,
                y = ~F_wanted,
                type = "scatter",
                mode = "lines+markers",
                text = labels,
                marker = list(color = "#ed5f26",  size = 15, symbol = "diamond"),
                line = list(color = "#ed5f26", width = 5, dash = "solid"),
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
                marker = list(color = "#000000", size = 20, symbol = "diamond-open"),
                text = "Basis of advice",
                name = "Basis of advice"
            )

        if (is_na_column(tmp, "F_wanted")) {
          
            tmp <- arrange(tmp, HR)
            labels <- sprintf(
              "Catch Scenario: %s", tmp$Scenario
            ) %>% lapply(htmltools::HTML)
            
            fig_catch <- plot_ly(tmp) %>%
                add_trace(
                    x = ~TotCatch,
                    y = ~HR,
                    type = "scatter",
                    mode = "lines+markers",
                    text = labels,
                    marker = list(color = "#ed5f26",  size = 15, symbol = "diamond"),
                    line = list(color = "#ed5f26", width = 5, dash = "solid"),
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
                    marker = list(color = "#000000", size = 20, symbol = "diamond-open"),
                    text = "Basis of advice",
                    name = "Basis of advice"
                )
        }
    } else {
        tmp <- arrange(tmp, F)
        labels <- sprintf(
          "Catch Scenario: %s", tmp$Scenario
        ) %>% lapply(htmltools::HTML)
        fig_catch <- plot_ly(tmp) %>%
            add_trace(
                x = ~TotCatch,
                y = ~F,
                type = "scatter",
                mode = "lines+markers",
                text = labels,
                marker = list(color = "#ed5f26",  size = 15, symbol = "diamond"),
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
                marker = list(color = "#000000", size = 20, symbol = "diamond-open"),
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
        x = ~Scenario,
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

    # Separate historical catches
    historical_df <- final_df %>% filter(Scenario == "Historical Catches" | Scenario == "Historical Landings")
    previousAdvice <- final_df %>% filter(Scenario == "Previous advice")
    other_df <- final_df %>%
        filter(Scenario %in% catch_scenarios) %>%
        filter(Scenario != "Historical Catches" & Scenario != "Historical Landings" & Scenario != "Previous advice")
    
    # Create plot for historical catches (lines only)
    catch_time <- plot_ly(
        historical_df,
        x = ~Year,
        y = ~Catches,
        type = "scatter",
        mode = "lines",
        line = list(
            color = ~Color,
            width = ~MarkerSize
        ),
        name = ~Scenario
    )



    # # Add dotted lines connecting the historical catches to previous advice, and previous advice to scenarios
    # # 1. Last entry of Historical Catches to Previous Advice
    # last_historical <- historical_df[nrow(historical_df),]
    # first_previous_advice <- previousAdvice[1,]

    # catch_time <- catch_time %>%
    #     add_trace(
    #         x = c(last_historical$Year, first_previous_advice$Year),
    #         y = c(last_historical$Catches, first_previous_advice$Catches),
    #         type = "scatter",
    #         mode = "lines",
    #         line = list(dash = "dot", color = "black", width = 1),
    #         # name = "Historical to Previous Advice",
    #         showlegend = FALSE
    #     )


    # # 2. Previous Advice to other scenarios
    # for (scenario in other_df$Scenario) {
    #     scenario_data <- other_df %>% filter(Scenario == scenario)
    #     first_scenario <- scenario_data[1,]

    #     catch_time <- catch_time %>%
    #         add_trace(
    #             x = c(first_previous_advice$Year, first_scenario$Year),
    #             y = c(first_previous_advice$Catches, first_scenario$Catches),
    #             type = "scatter",
    #             mode = "lines",
    #             line = list(dash = "dot", color = "black", width = 1),
    #             # name = paste("Previous Advice to", scenario)
    #             showlegend = FALSE
    #         )
    # }

    catch_time <- catch_time %>%
        add_markers(
            data = previousAdvice,
            x = ~Year,
            y = ~Catches,
            type = "scatter",
            mode = "markers",
            # line = list(color = previousAdvice$Color[1],
            #             width = 4, dash = "dash"),
            marker = list(size = ~MarkerSize, symbol = "circle", color = previousAdvice$Color),
            name = "Previous advice" # Set legend name for each scenario
        )

    catch_time <- catch_time %>%
        add_markers(
            data = other_df,
            x = ~Year,
            y = ~Catches,
            type = "scatter",
            mode = "markers",
            # line = list(color = other_df$Color),
            marker = list(size = ~MarkerSize, symbol = "diamond", color = other_df$Color),
            name = ~Scenario # Set legend name for each scenario
        )
    # }

    # Layout settings
    catch_time <- catch_time %>% layout(
        paper_bgcolor = "rgb(255,255,255)",
        plot_bgcolor = "rgb(255,255,255)",
        legend = list(
            orientation = "h",
            y = -0.6,
            yanchor = "bottom",
            x = 0.5,
            xanchor = "center"
            # title = list(text = "Scenarios")
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
        yaxis = list(
            title = Catches_yaxis_label,
            gridcolor = "rgb(235,235,235)",
            showgrid = TRUE,
            showline = TRUE,
            showticklabels = TRUE,
            tickcolor = "rgb(127,127,127)",
            ticks = "outside",
            zeroline = TRUE,
            titlefont = titlefont_format(),
            tickfont = tickfont_format(),
            rangemode='tozero'
        )
    )

    return(catch_time)
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
    
    
    pvar <- ggplot(dd, aes(x = Scenario, y = value, fill = indicator, colour = indicator)) +
        geom_segment(aes(x = Scenario, xend = as.factor(Scenario), y = 0, yend = value),
            color = "gray", lwd = 2
        ) +
        geom_point(size = 3) +
        coord_flip() +
    
        labs(y = "%", x = NULL) +
        facet_wrap(~indicator)
        
        
    fig8 <- ggplotly(pvar) %>% layout(showlegend = FALSE) #%>% 
        #config(modeBarButtonsToAdd = list(data_download_button(disclaimer)))
}
