library(icesSAG)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)



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

df <- access_sag_data("ple.27.420", 2021)
df
# create the theme
theme_ICES_plots <- function(type = c("catches", "recruitment", "F")) {
    font <- "Calibri, sans-serif" # assign font family up front

    # scale_color_manual(values = mycolors)
    tmp <- theme_minimal() %+replace% # replace elements we want to change

        theme(
            axis.title = element_text( # axis titles
                family = font, # font family
                size = 25,
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
                size = 25, # set font size
                face = "bold", # bold typeface
                hjust = 0, # left align
                vjust = 2,
                if (type == "catches") {
                    color <- "#002b5f"
                } else if (type == "recruitment") {
                    color <- "#28b3e8"
                } else if (type == "F") {
                    color <- "#ed5f26"
                } else if (type == "SSB") {
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
                size = 20,
                color = "black"
            ),
            legend.title = element_blank(),
            legend.position = "bottom"
        )

    #   axis.text.x = element_text(            #margin for axis text
    #                 margin=margin(5, b = 10))
    if (type == "catches") {
        mycolors <- c("#fda500", "#002b5f")
        theme_ICES_plots <- list(
            tmp,
            labs(
                title = "Catches",
                y = sprintf("Catches in 1000 %s", dplyr::last(df$units))
            ),
            scale_fill_manual(values = mycolors),
            scale_y_continuous(
                expand = expansion(mult = c(0, 0.1)),
                labels = function(l) {
                    trans <- l / 1000
                }
            )
        )
    } else if (type == "recruitment") {
        mycolors <- c("#28b3e8", "#666666")
        theme_ICES_plots <- list(
            tmp,
            labs(
                title = sprintf("Recruitment <sub>(age %s)</sub>", dplyr::last(df$recruitment_age)),
                y = "Recruitment in billions" # sprintf("Catches in 1000 %s", dplyr::last(df$units))
            ),
            scale_fill_manual(values = mycolors),
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
                title = "Fishing pressure", #sprintf("Recruitment <sub>(age %s)</sub>", dplyr::last(df$recruitment_age)),
                y = sprintf("%s <sub>(ages %s)</sub>",dplyr::last(df$fishingPressureDescription), dplyr::last(df$Fage)) # sprintf("Catches in 1000 %s", dplyr::last(df$units))
            ),
            scale_color_manual(values = c("#ed5f26")),
            scale_fill_manual(values = c("#f2a497")),
            expand_limits(y = 0),
            scale_y_continuous(
                expand = expansion(mult = c(0, 0.1))#,
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
                title = "Spawning Stock Biomass", #sprintf("Recruitment <sub>(age %s)</sub>", dplyr::last(df$recruitment_age)),
                y =  sprintf("%s in millions %s", dplyr::last(df$stockSizeDescription), dplyr::last(df$stockSizeUnits))
            ),
            scale_color_manual(values = c("#047c6c")),
            scale_fill_manual(values = c("#94b0a9")),
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


# # define labs
# text_labels <- labs(
#     title = "Catches",
#     y = sprintf("Catches in 1000 %s", dplyr::last(df$units))
# )


# selecting ddata and plotting
##################################catches#########################################################
p1 <- df %>% 
   select(Year, landings,  discards, units) %>% 
   gather(type, count, discards:landings) %>% 
   ggplot(., aes(x=Year, y=count, fill=type)) +
   geom_bar(position="stack", stat="identity") +
   theme_ICES_plots(type = "catches")
   
# plot <- p + text_labels
# plot
p1
#converting
fig1 <- ggplotly(p1) %>%
layout(legend = list(orientation = "h", y=-.1, yanchor="bottom", x = 0.4, title = list(text = "")))

######################################recruitment###################################################
# define labs
text_labels <- labs(
    title = sprintf("Recruitment <sub>(age %s)</sub>", dplyr::last(df$recruitment_age)),
    y = "Recruitment in billions" #sprintf("Catches in 1000 %s", dplyr::last(df$units))
)


p2 <- df %>%
    select(Year, recruitment, low_recruitment, high_recruitment, recruitment_age) %>%
    #    gather(type, count, discards:landings) %>%
    ggplot(., aes(x = Year, y = recruitment, fill = "Recruitment")) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = low_recruitment, ymax = high_recruitment), #, color = "2*sd"
        width = .2,
        position = position_dodge(.9)        
    ) +
    # scale_color_manual(values = c("#666666")) +
    # guides(colour = guide_legend(override.aes = list(linetype = c("solid"), 
    #                                                shape = c(15))))
    # scale_fill_manual(values = mycolors)
    theme_ICES_plots(type = "recruitment")
   
# plot <- p + text_labels
# plot
p2
#converting
fig2 <- ggplotly(p2) %>%
layout(legend = list(orientation = "h", y=-.1, yanchor="bottom", x = 0.4, title = list(text = "")))


p3 <- df %>%
    select(Year, F, low_F, high_F, FLim, Fpa, FMSY,Fage, fishingPressureDescription) %>% drop_na(F) %>% 
    #    gather(type, count, discards:landings) %>%
    ggplot(., aes(x = Year, y =  F, color= "F")) +
    geom_hline(yintercept = tail(df$FLim), linetype = 'dashed', colour = "#a1a1a1", size = 2) +
    geom_hline(yintercept = tail(df$Fpa), linetype = 'dotted', colour = "#a1a1a1", size = 2) +
    geom_hline(yintercept = tail(df$FMSY), linetype = 'solid', colour = "#00AC67", size = 2) +
    geom_ribbon(aes(ymin = low_F, ymax = high_F, fill = "2*sd"), linetype = "blank", size  =0) + #, alpha = 0.2
    geom_line(size = 1.5) +
    
    theme_ICES_plots(type = "F")
   
# plot <- p + text_labels
# plot
p3
#converting
fig3 <- ggplotly(p3) %>%
layout(legend = list(orientation = "h", y=-.1, yanchor="bottom", x = 0.4, title = list(text = "")),xaxis = list(zeroline = TRUE)) # nolint

p4 <- df %>%
    select(Year, low_SSB, SSB, high_SSB, Blim, Bpa, MSYBtrigger, stockSizeDescription, stockSizeUnits) %>% drop_na(SSB,high_SSB) %>% 
    #    gather(type, count, discards:landings) %>%
    ggplot(., aes(x = Year, y =  SSB, color= "SSB")) +
    geom_hline(yintercept = tail(df$Blim), linetype = 'dashed', colour = "#a1a1a1", size = 2) +
    geom_hline(yintercept = tail(df$Bpa), linetype = 'dotted', colour = "#a1a1a1", size = 2) +
    geom_hline(yintercept = tail(df$MSYBtrigger), linetype = 'solid', colour = "#689dff", size = 2) +
    geom_ribbon(aes(ymin = low_SSB, ymax = high_SSB, fill = "2*sd"), linetype = "blank", size  = 0) + #, alpha = 0.2
    geom_line(size = 1.5) +
    
    theme_ICES_plots(type = "SSB")
   
# plot <- p + text_labels
# plot
p4
#converting
fig4 <- ggplotly(p4) %>%
layout(legend = list(orientation = "h", y=-.1, yanchor="bottom", x = 0.2, title = list(text = "")),xaxis = list(zeroline = TRUE)) # nolint














fig <- subplot(list(fig1, fig2, fig3, fig4),
        nrows = 2, shareX = TRUE, titleX = TRUE, titleY = TRUE, widths = c(0.5, 0.5), heights = c(0.5, 0.5), margin = c(0.06,0.06,0.02,0.02)
    )
fig
