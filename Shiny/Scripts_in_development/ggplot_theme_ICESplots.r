library(icesSAG)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(purrr)
library(stringr)

access_sag_data <- function(stock_code, year) {

    # Dowload the data
    SAGsummary <- getSAG(stock_code, year,
        data = "summary", combine = TRUE, purpose = "Advice"
    )
    SAGrefpts <- getSAG(stock_code, year,
        data = "refpts", combine = TRUE, purpose = "Advice"
    )

    data_sag <- cbind(SAGsummary, SAGrefpts)
    data_sag <- subset(data_sag, select = -fishstock)
    data_sag <- filter(data_sag, StockPublishNote == "Stock published")
    #print(data_sag %>% tibble())
}

# download data

df <- access_sag_data("cod.27.47d20", 2021)
df
# create the theme
theme_ICES_plots <- function(type = c("catches", "recruitment", "F", "SSB", "quality_SSB", "quality_F", "quality_R")) {
    font <- "Calibri, sans-serif" # assign font family up front

    # scale_color_manual(values = mycolors)
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
            ), # raise slightly
            # grid elements
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.border = element_rect(
                colour = "black",
                fill = NA,
                size = 0.5
            ),
            # legend
            legend.text = element_text(
                family = "sans-serif",
                size = 15,
                color = "black"
                
            ),
            legend.title = element_blank(),
            legend.position = "bottom"
            
        )

    #   axis.text.x = element_text(            #margin for axis text
    #                 margin=margin(5, b = 10))
    if (type == "catches") {
        # mycolors <- c("#fda500", "#002b5f")
        theme_ICES_plots <- list(
            tmp,
            labs(
                title = "Catches",
                y = sprintf("Catches in 1000 %s", dplyr::last(df$units))
            ),
            scale_fill_manual(values = c(
                "landings" = "#002b5f",
                "discards" = "#fda500"
            )),
            scale_y_continuous(
                expand = expansion(mult = c(0, 0.1)),
                labels = function(l) {
                    trans <- l / 1000
                }
            )
        )
    } else if (type == "recruitment") {
        # mycolors <- c("recruitment" = "#28b3e8")
        theme_ICES_plots <- list(
            tmp,
            labs(
                title = sprintf("Recruitment <sub>(age %s)</sub>", dplyr::last(df$recruitment_age)),
                y = "Recruitment in billions" # sprintf("Catches in 1000 %s", dplyr::last(df$units))
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
        # mycolors <- c("#ed5f26")#, "#f2a497")
        theme_ICES_plots <- list(
            tmp,
            labs(
                title = "Fishing pressure", # sprintf("Recruitment <sub>(age %s)</sub>", dplyr::last(df$recruitment_age)),
                y = sprintf("%s <sub>(ages %s)</sub>", dplyr::last(df$fishingPressureDescription), dplyr::last(df$Fage)), # sprintf("Catches in 1000 %s", dplyr::last(df$units))
                x = "Year"
            ),
            scale_color_manual(values = c(
                "F" = "#ed5f26",
                "F<sub>MSY</sub>" = "#00AC67",
                "F<sub>Lim</sub>" = "#a1a1a1",
                "F<sub>pa</sub>" = "#a1a1a1"
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
                # labels = function(l) {
                #     trans <- l / 1000000
                # }
            )
        )
    } else if (type == "SSB") {
        # mycolors <- c("#ed5f26")#, "#f2a497")
        theme_ICES_plots <- list(
            tmp,
            labs(
                title = "Spawning Stock Biomass", # sprintf("Recruitment <sub>(age %s)</sub>", dplyr::last(df$recruitment_age)),
                y = sprintf("%s in millions %s", dplyr::last(df$stockSizeDescription), dplyr::last(df$stockSizeUnits)),
                x = "Year"
            ),
            scale_color_manual(values = c(
                "SSB" = "#047c6c",
                "MSY B<sub>trigger</sub>" = "#689dff",
                "B<sub>Lim</sub>" = "#a1a1a1",
                "B<sub>pa</sub>" = "#a1a1a1"
            )),
            scale_linetype_manual(values = c(
                "SSB" = "solid",
                "B<sub>Lim</sub>" = "dashed",
                "B<sub>pa</sub>" = "dotted",
                "MSY B<sub>trigger</sub>" = "solid"
            )),
            scale_size_manual(values = c(
                "SSB" = 1.5,
                "B<sub>Lim</sub>" = .8,
                "B<sub>pa</sub>" = 1,
                "MSY B<sub>trigger</sub>" = .5
            )),
            scale_fill_manual(values = c("#94b0a9")),


            # scale_color_manual(values = c("#047c6c")),
            # scale_fill_manual(values = c("#94b0a9")),
            expand_limits(y = 0),
            scale_y_continuous(
                expand = expansion(mult = c(0, 0.1)),
                labels = function(l) {
                    trans <- l / 1000000
                }
            )
        )
    } else if (type == "quality_SSB") {
        theme_ICES_plots <- list(
            tmp,
            labs(
                title = sprintf("%s in 1000 %s", dplyr::last(df$stockSizeDescription), dplyr::last(df$stockSizeUnits)),
                y = "",
                x = ""
            ),
            scale_color_manual(values = c(
                "2021" = "#047c6c",
                "2020" = "#252525",
                "2019" = "#525252",
                "2018" = "#737373",
                "2017" = "#969696",
                "MSY B<sub>trigger</sub>" = "#689dff",
                "B<sub>Lim</sub>" = "#a1a1a1",
                "B<sub>pa</sub>" = "#a1a1a1"
            )),
            scale_linetype_manual(values = c(
                "2021" = "solid",
                "2020" = "solid",
                "2019" = "solid",
                "2018" = "solid",
                "2017" = "solid",
                "B<sub>Lim</sub>" = "dashed",
                "B<sub>pa</sub>" = "dotted",
                "MSY B<sub>trigger</sub>" = "solid"
            )),
            scale_size_manual(values = c(
                "2021" = 1,
                "2020" = 1,
                "2019" = 1,
                "2018" = 1,
                "2017" = 1,
                "B<sub>Lim</sub>" = .8,
                "B<sub>pa</sub>" = 1,
                "MSY B<sub>trigger</sub>" = .5
            )),
            # scale_fill_manual(values = c("#94b0a9")),


            # scale_color_manual(values = c("#047c6c")),
            # scale_fill_manual(values = c("#94b0a9")),
            expand_limits(y = 0),
            scale_y_continuous(
                expand = expansion(mult = c(0, 0.1)),
                labels = function(l) {
                    trans <- l / 1000
                }
            )
        )
    } else if (type == "quality_F") {
        theme_ICES_plots <- list(
            tmp,
            labs(
                title = sprintf("%s <sub>(ages %s)</sub>", dplyr::last(df$fishingPressureDescription), dplyr::last(df$Fage)),
                y = "",
                x = "Year"
            ),
            scale_color_manual(values = c(
                "2021" = "#ed5f26",
                "2020" = "#252525",
                "2019" = "#525252",
                "2018" = "#737373",
                "2017" = "#969696",
                "F<sub>MSY</sub>" = "#00AC67",
                "F<sub>Lim</sub>" = "#a1a1a1",
                "F<sub>pa</sub>" = "#a1a1a1"
            )),
            scale_linetype_manual(values = c(
                "2021" = "solid",
                "2020" = "solid",
                "2019" = "solid",
                "2018" = "solid",
                "2017" = "solid",
                "F<sub>Lim</sub>" = "dashed",
                "F<sub>pa</sub>" = "dotted",
                "F<sub>MSY</sub>" = "solid"
            )),
            scale_size_manual(values = c(
                "2021" = 1,
                "2020" = 1,
                "2019" = 1,
                "2018" = 1,
                "2017" = 1,
                "F<sub>Lim</sub>" = .8,
                "F<sub>pa</sub>" = 1,
                "F<sub>MSY</sub>" = .5
            )),
            # scale_fill_manual(values = c("#94b0a9")),


            # scale_color_manual(values = c("#047c6c")),
            # scale_fill_manual(values = c("#94b0a9")),
            expand_limits(y = 0),
            scale_y_continuous(
                expand = expansion(mult = c(0, 0.1))
                
            )
        )
    } else if (type == "quality_R") {
        theme_ICES_plots <- list(
            tmp,
            labs(
                title = sprintf("Rec <sub>(age %s)</sub> (Billions)", dplyr::last(df$RecruitmentAge)),
                y = "",
                x = ""
            ),
            scale_color_manual(values = c(
                "2021" = "#28b3e8",
                "2020" = "#252525",
                "2019" = "#525252",
                "2018" = "#737373",
                "2017" = "#969696"                
            )),
            scale_linetype_manual(values = c(
                "2021" = "solid",
                "2020" = "solid",
                "2019" = "solid",
                "2018" = "solid",
                "2017" = "solid"                
            )),
            scale_size_manual(values = c(
                "2021" = 1,
                "2020" = 1,
                "2019" = 1,
                "2018" = 1,
                "2017" = 1                
            )),
            # scale_fill_manual(values = c("#94b0a9")),


            # scale_color_manual(values = c("#047c6c")),
            # scale_fill_manual(values = c("#94b0a9")),
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


# clean_plotly_legend <- function(plotly_obj) {
#     for (i in 1:length(plotly_obj$x$data)) {
#         if (!is.null(plotly_obj$x$data[[i]]$name)) {
#             plotly_obj$x$data[[i]]$name <- gsub("\\(", "", str_split(plotly_obj$x$data[[i]]$name, ",")[[1]][1])
#         }
#     }
# }
# # define labs
# text_labels <- labs(
#     title = "Catches",
#     y = sprintf("Catches in 1000 %s", dplyr::last(df$units))
# )
#### SAG stamp
SAGstamp <- list( showarrow = FALSE,
                                        text = "hom.27.2a4a5b6a7a-ce-k8_2021_16773_2022429010501",
                                        font = list(family = "Calibri, serif",size = 10, color = "black"),
                                        yref = 'paper', y = 1, xref = "paper", x = 1,
                                        yanchor = "right", xanchor = "right"
                                        )

# selecting ddata and plotting
##################################catches#########################################################
ICES_plot_1 <- function(df, SAGstamp) {
    p1 <- df %>%
        select(Year, landings, discards, units) %>%
        gather(type, count, discards:landings) %>%
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
        geom_bar(position = "stack", stat = "identity") +
        theme_ICES_plots(type = "catches")

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
            annotations = list(SAGstamp)
        )
    fig1
}
# ICES_plot_1(df, SAGstamp)
######################################recruitment###################################################
ICES_plot_2 <- function(df, SAGstamp) {
    p2 <- df %>%
        select(Year, recruitment, low_recruitment, high_recruitment, recruitment_age) %>%
        #    gather(type, count, discards:landings) %>%
        ggplot(., aes(
            x = Year,
            y = recruitment,
            fill = "recruitment",
            text = map(
                paste0(
                    "<b>Year: </b>", Year,
                    "<br>",
                    "<b>Recruitment: </b>", recruitment
                ), HTML
            )
        )) +
        geom_bar(stat = "identity") +
        geom_errorbar(aes(
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
        ), # , color = "2*sd"
        width = .3
        ) +
        theme_ICES_plots(type = "recruitment")

    # p2
    # converting
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
            annotations = list(SAGstamp)
        )
    fig2
}
# ICES_plot_2(df, SAGstamp)

ICES_plot_3 <- function(df, SAGstamp) {
p3 <- df %>%
    select(Year, F, low_F, high_F, FLim, Fpa, FMSY, Fage, fishingPressureDescription) %>%
    drop_na(F) %>%
    #    gather(type, count, discards:landings) %>%
    ggplot(., aes(x = Year, y = F)) +
    # , alpha = 0.2


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
    ), linetype = "blank", size = 0) +
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
    )) + #, size = 1.5
    geom_line(aes(
        x = Year,
        y = FMSY,
        colour = "F<sub>MSY</sub>",
        linetype = "F<sub>MSY</sub>",
        size = "F<sub>MSY</sub>",
        text = map(
            paste0(
                "<b>F<sub>MSY</sub>: </b>", tail(FMSY, 1)
            ), HTML
        )
    )) +
    geom_line(aes(
        x = Year,
        y = FLim,
        colour = "F<sub>Lim</sub>",
        linetype = "F<sub>Lim</sub>",
        size = "F<sub>Lim</sub>",
        text = map(
            paste0(
                "<b>F<sub>Lim</sub>: </b>", tail(FLim, 1)
            ), HTML
        )
    )) +
    geom_line(aes(
        x = Year,
        y = Fpa,
        colour = "F<sub>pa</sub>",
        linetype = "F<sub>pa</sub>",
        size = "F<sub>pa</sub>",
        text = map(
            paste0(
                "<b>F<sub>pa</sub>: </b>", tail(Fpa, 1)
            ), HTML
        )
    # geom_hline(aes(
    #     yintercept = tail(FMSY, 1),
    #     colour = "FMSY",
    #     linetype = "FMSY",
    #     size = "FMSY",
    #     text = map(
    #         paste0(
    #             "<b>FMSY: </b>", tail(FMSY, 1)
    #         ), HTML
    #     )
    # )) +
    # geom_hline(aes(
    #     yintercept = tail(FLim, 1),
    #     colour = "FLim",
    #     linetype = "FLim",
    #     size = "FLim",
    #     text = map(
    #         paste0(
    #             "<b>FLim: </b>", tail(FLim, 1)
    #         ), HTML
    #     )
    # )) +
    # geom_hline(aes(
    #     yintercept = tail(Fpa, 1),
    #     colour = "Fpa",
    #     linetype = "Fpa",
    #     size = "Fpa",
    #     text = map(
    #         paste0(
    #             "<b>Fpa: </b>", tail(Fpa, 1)
    #         ), HTML
    #     )
    )) +
    theme_ICES_plots(type = "F") 
    
   
# plot <- p + text_labels
# plot
# p3
#converting
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
        annotations = list(SAGstamp)
    ) 
for (i in 1:length(fig3$x$data)) {
    if (!is.null(fig3$x$data[[i]]$name)) {
        fig3$x$data[[i]]$name <- gsub("\\(", "", str_split(fig3$x$data[[i]]$name, ",")[[1]][1])
    }
}
fig3
}
# ICES_plot_3(df, SAGstamp)

ICES_plot_4 <- function(df, SAGstamp) {
p4 <- df %>%
    select(Year, low_SSB, SSB, high_SSB, Blim, Bpa, MSYBtrigger, stockSizeDescription, stockSizeUnits) %>%
    drop_na(SSB, high_SSB) %>%
    #    gather(type, count, discards:landings) %>%
    ggplot(., aes(x = Year, y = SSB)) +
    geom_ribbon(aes(
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
    ) +
    geom_line(aes(
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
    )
    # size = 1.5
    ) +
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
    )) +
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
    )) +
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
    )) +
    theme_ICES_plots(type = "SSB")
   
# plot <- p + text_labels
# plot
# p4
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
        annotations = list(SAGstamp)
    ) # nolint

for (i in 1:length(fig4$x$data)){
    if (!is.null(fig4$x$data[[i]]$name)){
        fig4$x$data[[i]]$name =  gsub("\\(","",str_split(fig4$x$data[[i]]$name,",")[[1]][1])
    }
}
fig4
}

# ICES_plot_4(df, SAGstamp)
# clean_plotly_legend(fig4)
# library(gridExtra)
# gring <- grid.arrange(p1,p2,p3,p4, nrow = 2)


# ggplotly(gring)

# fig <- subplot(list(fig1, fig2, fig3, fig4))
# fig <- subplot(list(fig1, fig2, fig3, fig4),
#         nrows = 2, shareX = TRUE, titleX = TRUE, titleY = TRUE, widths = c(0.4, 0.4), heights = c(0.4, 0.4), margin = c(0.1,0.1,0.1,0.1)
#     )
# fig


# clean_pltly_legend <- function(.pltly_obj, .new_legend = c()) {
#   # Cleans up a plotly object legend, particularly when ggplot is facetted
  
#   assign_leg_grp <- function(.legend_group, .leg_nms) {
#     # Assigns a legend group from the list of possible entries
#     # Used to modify the legend settings for a plotly object
    
#     leg_nms_rem <- .leg_nms
    
#     parse_leg_nms <- function(.leg_options) {
#       # Assigns a .leg_name, if possible
#       # .leg_options is a 2-element list: 1 = original value; 2 = remaining options
      
#       if (is.na(.leg_options)) {
#         .leg_options
#       } else if(length(leg_nms_rem) == 0) {
#         # No more legend names to assign
#         .leg_options
#       } else {
#         # Transfer the first element of the remaining options
#         leg_nm_new <- leg_nms_rem[[1]]
#         leg_nms_rem <<- leg_nms_rem[-1]
        
#         leg_nm_new
#       }
      
#     }
    
#     .legend_group %>% 
#       map(~ parse_leg_nms(.))
    
#   }
  
#   simplify_leg_grps <- function(.legendgroup_vec) {
#     # Simplifies legend groups by removing brackets, position numbers and then de-duplicating
    
#     leg_grp_cln <-
#       map_chr(.legendgroup_vec, ~ str_replace_all(., c("^\\(" = "", ",\\d+\\)$" = "")))
    
#     modify_if(leg_grp_cln, duplicated(leg_grp_cln), ~ NA_character_)
    
#   }
  
#   pltly_obj_data <-
#     .pltly_obj$x$data
  
#   pltly_leg_grp <-
#     # pltly_leg_grp is a character vector where each element represents a legend group. Element is NA if legend group not required or doesn't exist
#     pltly_obj_data%>% 
#     map(~ pluck(., "legendgroup")) %>% 
#     map_chr(~ if (is.null(.)) {NA_character_} else {.}) %>%
#     # Elements where showlegend = FALSE have legendgroup = NULL. 
    
#     simplify_leg_grps() %>% 
    
#     assign_leg_grp(.new_legend) 
  
#   pltly_obj_data_new <-
#     pltly_obj_data %>% 
#     map2(pltly_leg_grp, ~ list_modify(.x, legendgroup = .y)) %>%
#     map2(pltly_leg_grp, ~ list_modify(.x, name = .y)) %>%
#     map2(pltly_leg_grp, ~ list_modify(.x, showlegend = !is.na(.y)))
#   # i.e. showlegend set to FALSE when is.na(pltly_leg_grp), TRUE when not is.na(pltly_leg_grp)
  
#   .pltly_obj$x$data <- pltly_obj_data_new
  
#   .pltly_obj
  
# }

# f1 <- clean_pltly_legend(fig1, .new_legend = c("discards","landings"))
# f2 <- clean_pltly_legend(fig2, .new_legend = c("recruitment"))
# f3 <- clean_pltly_legend(fig4, .new_legend = c("2*sd","SSB","Blim","Bpa","MSYtrigger"))
# f4 <- clean_pltly_legend(fig3, .new_legend = c("2*sd","F","FMSY","Flim","Fpa"))

# fig_sub <- subplot(list(f1, f2, f3, f4),
#         nrows = 2, shareX = TRUE, titleX = TRUE, titleY = TRUE, widths = c(0.4, 0.4), heights = c(0.4, 0.4), margin = c(0.1,0.1,0.1,0.1)
#     )
# fig

# for (i in 1:length(fig4$x$data)){
#     if (!is.null(fig4$x$data[[i]]$name)){
#         fig4$x$data[[i]]$name =  gsub("\\(","",str_split(fig4$x$data[[i]]$name,",")[[1]][1])
#     }
# }

quality_assessment_data <- function(stock_code){

years <- c(2021, 2020, 2019, 2018, 2017)
datalist = list()

for (i in years) {
    print(i)
    data_temp <- try(access_sag_data(stock_code, i)) # "had.27.6b"

    ###############
    if (isTRUE(class(data_temp) == "try-error")) {
        next
    }
    else {
        #
        data_temp <- filter(data_temp, between(Year, 2005, 2021))
        data_temp <- data_temp %>% select(Year,
                                            recruitment, RecruitmentAge,
                                            SSB, Bpa, Blim, MSYBtrigger, stockSizeDescription, stockSizeUnits,
                                            F, FLim, Fpa, FMSY, Fage, fishingPressureDescription,
                                            AssessmentYear, StockPublishNote,Purpose)

        data_temp$RecruitmentAge <- as.character(data_temp$RecruitmentAge)
        data_temp$stockSizeDescription <- as.character(data_temp$stockSizeDescription)
        data_temp$ stockSizeUnits <- as.character(data_temp$ stockSizeUnits)
        data_temp$Fage <- as.character(data_temp$Fage)
        data_temp$fishingPressureDescription <- as.character(data_temp$fishingPressureDescription)

        datalist[[i]] <- data_temp
        # }
    }
}


#print(tibble(datalist))
### bind data in unique df
big_data <- dplyr::bind_rows(datalist)  ####################probem is with this function

# find last asseement year
last_year <- tail(big_data$AssessmentYear, n=1)

# subset last year
big_data_last_year <- big_data  %>% filter(AssessmentYear == last_year)

# take out non published data from before 2021 in big data
big_data <- filter(big_data, StockPublishNote == "Stock published")
big_data <- filter(big_data, Purpose == "Advice")
# put together the published data from before 2021 with the unpublished from 2021
big_data <- rbind(big_data, big_data_last_year)
big_data <- big_data  %>% distinct()

#make assessmentYear as factor
big_data$AssessmentYear <- as.factor(big_data$AssessmentYear)
big_data_last_year$AssessmentYear <- as.factor(big_data_last_year$AssessmentYear)

df_list <- list(big_data, big_data_last_year)
return(df_list)
}

#download quality of assessment data
df_qual <- quality_assessment_data("cod.27.47d20")


#plot
ICES_plot_5 <- function(df, SAGstamp) {
    p5 <- df %>%
        select(Year, AssessmentYear, SSB, Blim, Bpa, MSYBtrigger, stockSizeDescription, stockSizeUnits) %>%
        # drop_na(SSB, high_SSB) %>%
        #    gather(type, count, discards:landings) %>%
        ggplot(., aes(x = Year, y = SSB, color = AssessmentYear)) +
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
            ) # ,
            # size = 1,
            # linetype = "solid",
        ) +
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
        )) +
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
        )) +
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
        )) +
        theme_ICES_plots(type = "quality_SSB")
    # theme(legend.position = "none")

    # plot <- p + text_labels
    # plot
    # p5
    # converting
    fig5 <- ggplotly(p5, tooltip = "text") %>%
        layout(
            legend = list(
                orientation = "h",
                y = -.25,
                yanchor = "bottom",
                x = 0.5,
                xanchor = "center",
                title = list(text = "")
            ),
            xaxis = list(zeroline = TRUE),
            annotations = list(SAGstamp)
        ) # nolint

    for (i in 1:length(fig5$x$data)) {
        if (!is.null(fig5$x$data[[i]]$name)) {
            fig5$x$data[[i]]$name <- gsub("\\(", "", str_split(fig5$x$data[[i]]$name, ",")[[1]][1])
        }
    }
    fig5
}

# ICES_plot_5(df_qual[[1]], SAGstamp)

#F
ICES_plot_6 <- function(df, SAGstamp) {
    p6 <- df %>%
        select(Year, F, FLim, Fpa, FMSY, Fage, fishingPressureDescription, AssessmentYear) %>%
        # drop_na(SSB, high_SSB) %>%
        #    gather(type, count, discards:landings) %>%
        ggplot(., aes(x = Year, y = F, color = AssessmentYear)) +
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
            ) # ,
            # size = 1,
            # linetype = "solid",
        ) +
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
        )) +
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
        )) +
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
        )) +
        theme_ICES_plots(type = "quality_F")
    # theme(legend.position = "none")

    # plot <- p + text_labels
    # plot
    # p6
    # converting
    fig6 <- ggplotly(p6, tooltip = "text") %>%
        layout(
            legend = list(
                orientation = "h",
                y = -.25,
                yanchor = "bottom",
                x = 0.5,
                xanchor = "center",
                title = list(text = "")
            ),
            xaxis = list(zeroline = TRUE),
            annotations = list(SAGstamp)
        ) # nolint

    for (i in 1:length(fig6$x$data)) {
        if (!is.null(fig6$x$data[[i]]$name)) {
            fig6$x$data[[i]]$name <- gsub("\\(", "", str_split(fig6$x$data[[i]]$name, ",")[[1]][1])
        }
    }
    fig6
}
# ICES_plot_6(df_qual[[1]], SAGstamp)
#Rec
ICES_plot_7 <- function(df, SAGstamp) {
    p7 <- df %>%
        select(Year, recruitment, RecruitmentAge, AssessmentYear) %>%
        drop_na(recruitment) %>%
        #    gather(type, count, discards:landings) %>%
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
            ) # ,
            # size = 1,
            # linetype = "solid",
        ) +
        theme_ICES_plots(type = "quality_R")
    # theme(legend.position = "none")

    # plot <- p + text_labels
    # plot
    # p7
    # converting
    fig7 <- ggplotly(p7, tooltip = "text") %>%
        layout(
            legend = list(
                orientation = "h",
                y = -.25,
                yanchor = "bottom",
                x = 0.5,
                xanchor = "center",
                title = list(text = "")
            ),
            xaxis = list(zeroline = TRUE),
            annotations = list(SAGstamp)
        ) # nolint

    for (i in 1:length(fig7$x$data)) {
        if (!is.null(fig7$x$data[[i]]$name)) {
            fig7$x$data[[i]]$name <- gsub("\\(", "", str_split(fig7$x$data[[i]]$name, ",")[[1]][1])
        }
    }
    fig7
}
# ICES_plot_7(df_qual[[1]], SAGstamp)


# layout_ggplotly <- function(gg, x = -0.1, y = -0.05, x_legend=1.05, y_legend=0.95, mar=list(l=50, r=150)){
#   # The 1 and 2 goes into the list that contains the options for the x and y axis labels respectively
#   gg[['x']][['layout']][['annotations']][[1]][['y']] <- x
#   gg[['x']][['layout']][['annotations']][[2]][['x']] <- y
#   gg[['x']][['layout']][['annotations']][[11]][['x']] <- x_legend # the legend title was the 11-th list element in my case!
#   gg[['x']][['layout']][['legend']][['y']] <- y_legend
#   gg[['x']][['layout']][['legend']][['x']] <- x_legend
#   gg %>% layout(margin = mar)
# }

# layout_ggplotly(fig7)

# fig7[['x']][['layout']][["margin"]][["t"]] <- 50
# fig7[['x']][['layout']][["margin"]][["b"]] <- 10
# fig7[['x']][['layout']][['legend']][['x']]





library(shiny)
library(shinyWidgets)

title_html <- tags$a(
    href = "https://ices-taf.shinyapps.io/online-single-stock-advice/",
    target = "_blank",
        tags$img(
            src = "https://www.ices.dk/SiteCollectionImages/ICES%20logos/NEGATIVE%20ICES-logo.png",
            style = "margin-top: -10px; padding-right:10px;padding-bottom:10px",
            height = "50px"
        )
)

ui <- navbarPage(

    # tab title
    windowTitle = "Online Advice",
    id = "tabset",
    fluid = TRUE,
    # navbar title
    title = title_html,
    tabPanel("Stock selection"),
    navbarMenu(
        "Stock assessment graphs",
        tabPanel(
            "Development over time",
            panel(
                style = "height: 90vh; overflow-y: auto;",
                fluidRow(
                    column(
                        width = 6, style = "height: 43vh;",
                        plotlyOutput("plot1", height = "100%", width = "100%")
                    ),
                    column(
                        width = 6, style = "height: 43vh;",
                        plotlyOutput("plot2", height = "100%", width = "100%")
                    ),
                ),
                fluidRow(
                    column(
                        width = 6, style = "height: 43vh;",
                        plotlyOutput("plot3", height = "100%", width = "100%")
                    ),
                    column(
                        width = 6, style = "height: 43vh;",
                        plotlyOutput("plot4", height = "100%", width = "100%")
                    ),
                )
            )
        ),
        tabPanel(
            "Quality of assessment",
            panel(
                style = "height: 90vh; overflow-y: auto;",
                fluidRow(
                    column(
                        width = 4, style = "height: 85vh;",
                        plotlyOutput("plot5", height = "100%", width = "100%"),
                    ),
                    column(
                        width = 4, style = "height: 85vh;",
                        plotlyOutput("plot6", height = "100%", width = "100%")
                    ),
                    column(
                        width = 4, style = "height: 85vh;",
                        plotlyOutput("plot7", height = "100%", width = "100%")
                    )
                )
            )
        )
    ),
    tabPanel("Advice")
)

# Define UI ----
# ui <- fluidPage(
#     sidebarLayout(
#         sidebarPanel(
#             width = 9,
#             panel(
#                 style = "height: 94vh; overflow-y: auto;",
#                 fluidRow(
#                     column(
#                         width = 6, style = "height: 45vh;",
#                         plotlyOutput("plot1", height = "100%", width = "100%")
#                     ),
#                     column(
#                         width = 6, style = "height: 45vh;",
#                         plotlyOutput("plot2", height = "100%", width = "100%")
#                     ),
#                 ),
#                 fluidRow(
#                     column(
#                         width = 6, style = "height: 45vh;",
#                         plotlyOutput("plot3", height = "100%", width = "100%")
#                     ),
#                     column(
#                         width = 6, style = "height: 45vh;",
#                         plotlyOutput("plot4", height = "100%", width = "100%")
#                     ),
#                 )
#             )
#         ),
#         sidebarPanel(
#             width = 3,
#             panel(
#                 style = "height: 94vh; overflow-y: auto;",
#                 fluidRow(
#                     column(
#                         width = 12, style = "height: 30vh;",
#                         plotlyOutput("plot5", height = "100%", width = "100%")
#                     )
#                 ),
#                 fluidRow(
#                     column(
#                         width = 12, style = "height: 30vh;",
#                         plotlyOutput("plot6", height = "100%", width = "100%")
#                     )
#                 ),
#                 fluidRow(
#                     column(
#                         width = 12, style = "height: 30vh;",
#                         plotlyOutput("plot7", height = "100%", width = "100%")
#                     )
#                 ),
#             )
#         )
#     )
# )
# ui<- fluidPage(
#     tabPanel(
#         "Stock development over time",
#         sidebarLayout(
#              mainPanel = plots_panel,
#             sidebarPanel = allocations_plotspanel
           
#         )
#         # includeMarkdown("Instructions.Rmd")
#     )
# )
stock_name <- "cod.27.47d20"
year <- 2021
# Define server logic ----
server <- function(input, output) {
# df <- access_sag_data("cod.27.47d20", 2021)
advice_action <- eventReactive(req(stock_name, year), {
    #   # Dowload the data
    access_sag_data(stock_name, year)
})

  output$plot1 <- renderPlotly(
      ICES_plot_1(advice_action(), SAGstamp)
  )
  output$plot2 <- renderPlotly(
      ICES_plot_2(advice_action(), SAGstamp)
  )
  output$plot3 <- renderPlotly(
      ICES_plot_3(advice_action(), SAGstamp)
  )
  output$plot4 <- renderPlotly(
      ICES_plot_4(advice_action(), SAGstamp)
  )
  output$plot5 <- renderPlotly(
      ICES_plot_5(df_qual[[1]], SAGstamp)
  )
  output$plot6 <- renderPlotly(
      ICES_plot_6(df_qual[[1]], SAGstamp)
  )
  output$plot7 <- renderPlotly(
      ICES_plot_7(df_qual[[1]], SAGstamp)
  )
}

# Run the app ----

shinyApp(ui = ui, server = server)



