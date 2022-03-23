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

# create the theme
theme_ICES_plots <- function() {
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
                color = "#002b5f"
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

    mycolors <- c("#fda500", "#002b5f")
    theme_ICES_plots <- list(
        tmp,
        scale_fill_manual(values = mycolors),
        scale_y_continuous(
            expand = expansion(mult = c(0, 0.1)),
            labels = function(l) {
                trans <- l / 1000
            }
        )
    )
    return(theme_ICES_plots)
}


# define labs
text_labels <- labs(
    title = "Catches",
    y = sprintf("Catches in 1000 %s", dplyr::last(df$units))
)


# selecting ddata and plotting
p <- df %>% 
   select(Year, landings,  discards, units) %>% 
   gather(type, count, discards:landings) %>% 
   ggplot(., aes(x=Year, y=count, fill=type)) +
   geom_bar(position="stack", stat="identity") +
   theme_ICES_plots()
   
plot <- p + text_labels
plot


#converting
ggplotly(plot) %>%
layout(legend = list(orientation = "h", y=-.1, yanchor="bottom", x = 0.4, title = list(text = "")))

