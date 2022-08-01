# Dorleta Garcia
# 2022/05/21

library(ggthemes)
library(RColorBrewer)
library(tidyverse)
library(ggpubr)
library(plotly)

setwd("~/Shiny/catch table")
dd <- read.csv("Shiny/Scripts_in_development/catch_table.csv" )

# Change the name of the columns and convert it to long format.
names(dd) <- c('Scenario', 'Catch advice', 'Landings corresponding to advice',
               'Discards corresponding to advice', 'Fc', 'Fl', 'Fd', 'SSB in 2024', 
               'Variation in SSB', 'Variation in TAC', 'Variation in catch advice')
dd <- dd %>% pivot_longer(cols = -1,names_to = 'indicator')


# Order the levels of the factor.
dd$Scenario <- factor(dd$Scenario, levels = c('F=Fmsy', 'F=Flow', 'F=Fupp', 'F=0', 'F=Fpa', 'F=Flim',
                                              'SSB=Bpa', 'SSB=Blim', 'Equal Advice'))

# ggplot(dd %>% filter(indicator %in% c('Catch advice', 'SSB in 2024', 'Variation in SSB', 'Variation in catch advice')), aes(Scenario, value, fill = Scenario)) + geom_bar(stat = 'identity') +
# ggtitle('Catch options in 2023') + xlab('') + ylab('tonnes') + theme_hc() +  facet_wrap(~indicator, ncol =2, scales = 'free') +
#   scale_fill_brewer(palette='Set3')

# Filter the data set to use it in the lollipop plots.
dd1 <- dd %>% filter(indicator %in% c('Variation in SSB', 'Variation in catch advice')) %>% 
       mutate(Scenario = ifelse(indicator == 'Variation in SSB', paste0(Scenario, '.'), Scenario),
              indicator = ifelse(indicator == 'Variation in SSB', 'SSB', 'Catch advice'))


# Catch advice plot.
pcatch <- ggplot(dd %>% filter(indicator %in% c('Landings correspondit to advice', 'Discards correspondit to advice')), aes(Scenario, value, fill = indicator)) + 
  geom_bar(stat = 'identity') +
  ggtitle('Catch in 2023') + xlab('') + ylab('tonnes') + theme_hc() +  scale_color_brewer(palette='Set3') +
  theme( legend.position = 'bottom',  legend.title = element_blank())

# SSB plot
pssb <- ggplot(dd %>% filter(indicator %in% c('SSB in 2024')), aes(Scenario, value, fill = indicator)) + 
  geom_bar(stat = 'identity') +
  ggtitle('SSB in 2024') + xlab('') + ylab('tonnes') + theme_hc() +  scale_fill_manual(values="#00BFC4") +
  theme( legend.position = 'none')

# Variation in advice plot.
pvar <- ggplot(dd1, aes(x = Scenario, y = value*100)) +
  geom_segment(aes(x = Scenario, xend = Scenario, y = 0, yend = value*100),
               color = "gray", lwd = 2) + ylab( '%') + xlab('')+
  geom_point(size = 4, pch = 21, col = 1, aes(bg = indicator)) +
  scale_x_discrete(labels = unique(dd$Scenario), breaks = unique(dd$Scenario)) +
  theme(text = element_text(size = 16), legend.position = 'bottom', legend.title = element_blank()) +
  theme_hc() + ggtitle('Variation in catch advice and SSB') + 
  geom_vline(xintercept = seq(0.5, 19.5, 2))

# ggplotly(pvar)

ggarrange(pcatch, pssb, pvar, ncol = 2, nrow = 2)
#########################################################

df <- df %>% select(-Year,-cS_Purpose)
dd <- df %>% pivot_longer(cols = -1, names_to = 'indicator')


pvar <- ggplot(dd, aes(x = cat, y = value, fill = indicator, colour = indicator)) +
  geom_segment(aes(x = cat, xend = as.factor(cat), y = 0, yend = value),
    color = "gray", lwd = 2
  ) +
  geom_point(size = 4) +
  coord_flip() +
  facet_wrap(~indicator)

ggplotly(pvar)
