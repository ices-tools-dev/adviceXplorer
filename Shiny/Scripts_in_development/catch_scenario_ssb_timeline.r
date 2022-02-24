library(ggforce)
library(colorRamps)
library(ggplot2)
library(ggforce)
library(grid)


data <- access_sag_data_local("san.sa.1r", 2021)
data_ssb <- data %>% select(Year, catches)
tibble(data_ssb)	

data_catch <- get_catch_scenario_table("san.sa.1r")
data_catch_stand <- standardize_catch_scenario_table(data_catch)
tibble(data_catch_stand)

data_ssb <- data_ssb %>% add_column(cat = "Historical")

data_catch_f <- data_catch_stand %>% select(Year, TotCatch, cat)

data_catch_f_year_min_one <- data_catch_f
data_catch_f_year_min_one$Year <- 2020

data_catch_f_year_min_one$TotCatch <- data_ssb$catches[data_ssb$Year == 2020]


data_ssb_rename <- setNames(data_ssb, names(data_catch_f))
final_df <- rbind(data_ssb_rename, data_catch_f_year_min_one, data_catch_f )#



p <- ggplot(data=final_df ,
 aes(x=Year, y=TotCatch, group=cat, fill=cat, colour= cat)) +
    geom_line() +
    geom_point() + 
    theme(legend.position = "bottom") +
    # theme(panel.background = element_rect(fill = "#ffffff",
    #                             colour = "#ffffff",
    #                             size = 0.5, linetype = "solid")) +
  guides(color = guide_legend(ncol =  3, byrow = TRUE)) +
    facet_zoom2(xlim = c(2010, 2022), ylim = c(0, 50000))
p
fig <- ggplotly(p)

fig
mypalette<-primary.colors(length(unique(final_df$cat)))
p1 <- plot_ly(final_df, x = ~Year, y = ~TotCatch, type = 'scatter', mode = 'lines+markers', showlegend = T, #linetype = ~cat,
            color = ~cat, colors = mypalette)
p1
#### modification of zoom found at https://stackoverflow.com/questions/52665619/how-to-change-the-position-of-the-zoomed-area-from-facet-zoom


p1 <- plot_ly(final_df) %>%
        add_trace(data = final_df %>% filter(cat =="Historical"), x = ~Year, y = ~TotCatch, type = "scatter", mode = "lines+markers") %>%
        add_trace(data = final_df %>% filter(cat !="Historical"),x = ~Year, y = ~TotCatch, type = "scatter", mode = "lines+markers", color = ~cat)
#  add_trace(x = ~DATE, y = ~data.user, yaxis = 'y2',name = 'Event',type = 'scatter',mode = 'markers') %>% 
p1

p1 <- catch_scenarios_plot2(data_catch_stand)
p1
mypalette<-primary.colors(length(unique(final_df$cat)))
p2 <- plot_ly(final_df, x = ~Year, y = ~TotCatch, type = 'scatter', mode = 'lines+markers', showlegend = F)#, #linetype = ~cat,
            #color = ~cat, colors = mypalette, cliponaxis = FALSE)
p2 <- p2 %>% layout(autosize = T,  margin=list( l = 120, r = 120, b = 120, t = 50,  pad = 6))
p2

fig <- subplot(p1, p2, nrows = 2,  heights = c(0.7, 0.3))#, margin = list(b = 1, t=1, l= 4, r =4, pad = 6))

fig <- fig %>% layout(autosize = T,  margin=list( l = 120, r = 120, b = 120, t = 50,  pad = 6),showlegend=FALSE,showlegend2=FALSE)

# fig <- fig %>% layout(
#          yaxis = list(domain=list(x=c(0,0.5),y=c(0,0.5))),
#          scene = list(domain=list(x=c(0.5,1),y=c(0,0.5))),
#          yaxis2 = list(domain=list(x=c(0.5,1),y=c(0.5,1))),
#          showlegend=FALSE,showlegend2=FALSE)
fig



TAC_timeline(data_ssb_rename,data_catch_f)

####################timelien

advice_timeline <- function(stock_code){
  timeL <- get_Advice_View_info(stock_code)

release_date <- timeL[timeL["advice View"] == "adviceReleasedDate",2]
applicable_from <- timeL[timeL["advice View"] == "adviceApplicableFrom",2]
applicable_until <- timeL[timeL["advice View"] == "adviceApplicableUntil",2]

release_date <- strptime(as.character(release_date), "%d/%m/%Y")
release_date <- format(release_date, "%Y-%m-%d")
applicable_from <- strptime(as.character(applicable_from), "%d/%m/%Y")
applicable_from <- format(applicable_from, "%Y-%m-%d")
applicable_until <- strptime(as.character(applicable_until), "%d/%m/%Y")
applicable_until <- format(applicable_until, "%Y-%m-%d")



data <- data.frame(
      id      = 1:2,
      content = c("Advice Release Date" , "Advice Applicable Between"),
      start   = c(release_date, applicable_from),      
      end     = c(NA          , applicable_until)
    )


timevis(data)
}

timeL <- get_Advice_View_info("cod.27.47d20")

release_date <- timeL[timeL["advice View"] == "adviceReleasedDate",2]
applicable_from <- timeL[timeL["advice View"] == "adviceApplicableFrom",2]
applicable_until <- timeL[timeL["advice View"] == "adviceApplicableUntil",2]

release_date <- strptime(as.character(release_date), "%d/%m/%Y")
release_date <- format(release_date, "%Y-%m-%d")
applicable_from <- strptime(as.character(applicable_from), "%d/%m/%Y")
applicable_from <- format(applicable_from, "%Y-%m-%d")
applicable_until <- strptime(as.character(applicable_until), "%d/%m/%Y")
applicable_until <- format(applicable_until, "%Y-%m-%d")


library(timevis)
data <- data.frame(
      id      = 1:2,
      content = c("Advice Release Date" , "Advice Applicable Between"),
      start   = c(release_date, applicable_from),      
      end     = c(NA          , applicable_until)
    )


timevis(data)


### scraping function to retrieve the dates of the next WG meeeting for a stock
library(rvest)

observeEvent(input$tbl_rows_selected, {
    filtered_row <- res_mod()[input$tbl_rows_selected, ]
    WG <- filtered_row$ExpertGroupUrl
    print(str_match(WG, "\\>\\s*(.*?)\\s*\\<\\/a>")[,2])
})
# str_match(page, "\\>\\s*(.*?)\\s*\\<\\/a>")[,2]

page <- read_html("https://www.ices.dk/news-and-events/meeting-calendar/Pages/ICES-CalendarSearch.aspx?k=HAWG")
page

start_date <- page %>% 
html_nodes("td") %>% 
html_text()

title_meeting <- start_date[1]
library(gsubfn)
start_WG <- strapplyc(start_date[2], "\\d+/\\d+/\\d+", simplify = TRUE)
end_WG <- strapplyc(start_date[3], "\\d+/\\d+/\\d+", simplify = TRUE)














########################################################################################################################################

# define facet_zoom2 function to use FacetZoom2 instead of FacetZoom
# (everything else is the same as facet_zoom)
facet_zoom2 <- function(x, y, xy, zoom.data, xlim = NULL, ylim = NULL, 
                        split = FALSE, horizontal = TRUE, zoom.size = 2, 
                        show.area = TRUE, shrink = TRUE) {
  x <- if (missing(x)) if (missing(xy)) NULL else lazyeval::lazy(xy) else lazyeval::lazy(x)
  y <- if (missing(y)) if (missing(xy)) NULL else lazyeval::lazy(xy) else lazyeval::lazy(y)
  zoom.data <- if (missing(zoom.data)) NULL else lazyeval::lazy(zoom.data)
  if (is.null(x) && is.null(y) && is.null(xlim) && is.null(ylim)) {
    stop("Either x- or y-zoom must be given", call. = FALSE)
  }
  if (!is.null(xlim)) x <- NULL
  if (!is.null(ylim)) y <- NULL
  ggproto(NULL, FacetZoom2,
          shrink = shrink,
          params = list(
            x = x, y = y, xlim = xlim, ylim = ylim, split = split, zoom.data = zoom.data,
            zoom.size = zoom.size, show.area = show.area,
            horizontal = horizontal
          )
  )
}

# define FacetZoom as a ggproto object that inherits from FacetZoom,
# with a modified draw_panels function. the compute_layout function references
# the version currently on GH, which is slightly different from the CRAN
# package version.
FacetZoom2 <- ggproto(
  "FacetZoom2",
  ggforce::FacetZoom,

  compute_layout = function(data, params) {
    layout <- rbind( # has both x & y dimension
      data.frame(name = 'orig', SCALE_X = 1L, SCALE_Y = 1L),
      data.frame(name = 'x', SCALE_X = 2L, SCALE_Y = 1L),
      data.frame(name = 'y', SCALE_X = 1L, SCALE_Y = 2L),
      data.frame(name = 'full', SCALE_X = 2L, SCALE_Y = 2L),
      data.frame(name = 'orig_true', SCALE_X = 1L, SCALE_Y = 1L),
      data.frame(name = 'zoom_true', SCALE_X = 1L, SCALE_Y = 1L)
    )
    if (is.null(params$y) && is.null(params$ylim)) { # no y dimension
      layout <- layout[c(1,2, 5:6),]
    } else if (is.null(params$x) && is.null(params$xlim)) { # no x dimension
      layout <- layout[c(1,3, 5:6),]
    }
    layout$PANEL <- seq_len(nrow(layout))
    layout
  },

  draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord,
                         data, theme, params) {

    if (is.null(params$x) && is.null(params$xlim)) {
      params$horizontal <- TRUE
    } else if (is.null(params$y) && is.null(params$ylim)) {
      params$horizontal <- FALSE
    }
    if (is.null(theme[['zoom']])) {
      theme$zoom <- theme$strip.background
    }
    if (is.null(theme$zoom.x)) {
      theme$zoom.x <- theme$zoom
    }
    if (is.null(theme$zoom.y)) {
      theme$zoom.y <- theme$zoom
    }
    axes <- render_axes(ranges, ranges, coord, theme, FALSE)
    panelGrobs <- ggforce:::create_panels(panels, axes$x, axes$y)
    panelGrobs <- panelGrobs[seq_len(length(panelGrobs) - 2)]
    if ('full' %in% layout$name && !params$split) {
      panelGrobs <- panelGrobs[c(1, 4)]
    }

    # changed coordinates in indicator / lines to zoom from 
    # the opposite horizontal direction
    if ('y' %in% layout$name) {
      if (!inherits(theme$zoom.y, 'element_blank')) {
        zoom_prop <- scales::rescale(
          y_scales[[2]]$dimension(ggforce:::expansion(y_scales[[2]])),
          from = y_scales[[1]]$dimension(ggforce:::expansion(y_scales[[1]])))
        indicator <- polygonGrob(
          x = c(0, 0, 1, 1), # was x = c(1, 1, 0, 0), 
          y = c(zoom_prop, 1, 0), 
          gp = gpar(col = NA, fill = alpha(theme$zoom.y$fill, 0.5)))
        lines <- segmentsGrob(
          x0 = c(1, 1), x1 = c(0, 0), # was x0 = c(0, 0), x1 = c(1, 1)
          y0 = c(0, 1), y1 = zoom_prop,
          gp = gpar(col = theme$zoom.y$colour,
                    lty = theme$zoom.y$linetype,
                    lwd = theme$zoom.y$size,
                    lineend = 'round'))
        indicator_h <- grobTree(indicator, lines)
      } else {
        indicator_h <- zeroGrob()
      }
    }

    if ('x' %in% layout$name) {
      if (!inherits(theme$zoom.x, 'element_blank')) {
        zoom_prop <- scales::rescale(x_scales[[2]]$dimension(ggforce:::expansion(x_scales[[2]])),
                                     from = x_scales[[1]]$dimension(ggforce:::expansion(x_scales[[1]])))
        indicator <- polygonGrob(c(zoom_prop, 1, 0), c(1, 1, 0, 0), 
                                 gp = gpar(col = NA, fill = alpha(theme$zoom.x$fill, 0.5)))
        lines <- segmentsGrob(x0 = c(0, 1), y0 = c(0, 0), x1 = zoom_prop, y1 = c(1, 1), 
                              gp = gpar(col = theme$zoom.x$colour,
                                        lty = theme$zoom.x$linetype,
                                        lwd = theme$zoom.x$size,
                                        lineend = 'round'))
        indicator_v <- grobTree(indicator, lines)
      } else {
        indicator_v <- zeroGrob()
      }
    }

    if ('full' %in% layout$name && params$split) {
      space.x <- theme$panel.spacing.x
      if (is.null(space.x)) space.x <- theme$panel.spacing
      space.x <- unit(5 * as.numeric(convertUnit(space.x, 'cm')), 'cm')
      space.y <- theme$panel.spacing.y
      if (is.null(space.y)) space.y <- theme$panel.spacing
      space.y <- unit(5 * as.numeric(convertUnit(space.y, 'cm')), 'cm')

      # change horizontal order of panels from [zoom, original] to [original, zoom]
      # final <- gtable::gtable_add_cols(panelGrobs[[3]], space.x)
      # final <- cbind(final, panelGrobs[[1]], size = 'first')
      # final_tmp <- gtable::gtable_add_cols(panelGrobs[[4]], space.x)
      # final_tmp <- cbind(final_tmp, panelGrobs[[2]], size = 'first')
      final <- gtable::gtable_add_cols(panelGrobs[[1]], space.x)
      final <- cbind(final, panelGrobs[[3]], size = 'first')
      final_tmp <- gtable::gtable_add_cols(panelGrobs[[2]], space.x)
      final_tmp <- cbind(final_tmp, panelGrobs[[4]], size = 'first')

      final <- gtable::gtable_add_rows(final, space.y)
      final <- rbind(final, final_tmp, size = 'first')
      final <- gtable::gtable_add_grob(final, list(indicator_h, indicator_h),
                                       c(2, 6), 3, c(2, 6), 5,
                                       z = -Inf, name = "zoom-indicator")
      final <- gtable::gtable_add_grob(final, list(indicator_v, indicator_v), 
                                       3, c(2, 6), 5, 
                                       z = -Inf, name = "zoom-indicator")
      heights <- unit.c(
        unit(max_height(list(axes$x[[1]]$top, axes$x[[3]]$top)), 'cm'),
        unit(1, 'null'),
        unit(max_height(list(axes$x[[1]]$bottom, axes$x[[3]]$bottom)), 'cm'),
        space.y,
        unit(max_height(list(axes$x[[2]]$top, axes$x[[4]]$top)), 'cm'),
        unit(params$zoom.size, 'null'),
        unit(max_height(list(axes$x[[2]]$bottom, axes$x[[4]]$bottom)), 'cm')
      )

      # swop panel width specifications according to the new horizontal order
      widths <- unit.c(
        # unit(max_width(list(axes$y[[3]]$left, axes$y[[4]]$left)), 'cm'),
        # unit(params$zoom.size, 'null'),
        # unit(max_height(list(axes$y[[3]]$right, axes$y[[4]]$right)), 'cm'),
        # space.x,
        # unit(max_width(list(axes$y[[1]]$left, axes$y[[2]]$left)), 'cm'),
        # unit(1, 'null'),
        # unit(max_height(list(axes$y[[1]]$right, axes$y[[2]]$right)), 'cm')        
        unit(max_width(list(axes$y[[1]]$left, axes$y[[2]]$left)), 'cm'),
        unit(1, 'null'),
        unit(max_height(list(axes$y[[1]]$right, axes$y[[2]]$right)), 'cm'),
        space.x,
        unit(max_width(list(axes$y[[3]]$left, axes$y[[4]]$left)), 'cm'),
        unit(params$zoom.size, 'null'),
        unit(max_height(list(axes$y[[3]]$right, axes$y[[4]]$right)), 'cm')

      )
      final$heights <- heights
      final$widths <- widths
    } else {
      if (params$horizontal) {
        space <- theme$panel.spacing.x
        if (is.null(space)) space <- theme$panel.spacing
        space <- unit(5 * as.numeric(convertUnit(space, 'cm')), 'cm')
        heights <- unit.c(
          unit(max_height(list(axes$x[[1]]$top, axes$x[[2]]$top)), 'cm'),
          unit(1, 'null'),
          unit(max_height(list(axes$x[[1]]$bottom, axes$x[[2]]$bottom)), 'cm')
        )

        # change horizontal order of panels from [zoom, original] to [original, zoom]
        # first <- gtable::gtable_add_cols(panelGrobs[[2]], space)
        # first <- cbind(final, panelGrobs[[1]], size = 'first')
        final <- gtable::gtable_add_cols(panelGrobs[[1]], space) 
        final <- cbind(final, panelGrobs[[2]], size = "first") 

        final$heights <- heights

        # swop panel width specifications according to the new horizontal order
        # unit(c(params$zoom.size, 1), 'null')
        final$widths[panel_cols(final)$l] <- unit(c(1, params$zoom.size), 'null') 

        final <- gtable::gtable_add_grob(final, indicator_h, 2, 3, 2, 5, 
                                         z = -Inf, name = "zoom-indicator")
      } else {
        space <- theme$panel.spacing.y
        if (is.null(space)) space <- theme$panel.spacing
        space <- unit(5 * as.numeric(convertUnit(space, 'cm')), 'cm')
        widths <- unit.c(
          unit(max_width(list(axes$y[[1]]$left, axes$y[[2]]$left)), 'cm'),
          unit(1, 'null'),
          unit(max_height(list(axes$y[[1]]$right, axes$y[[2]]$right)), 'cm')
        )
        final <- gtable::gtable_add_rows(panelGrobs[[1]], space)
        final <- rbind(final, panelGrobs[[2]], size = 'first')
        final$widths <- widths
        final$heights[panel_rows(final)$t] <- unit(c(1, params$zoom.size), 'null')
        final <- gtable::gtable_add_grob(final, indicator_v, 3, 2, 5, 
                                         z = -Inf, name = "zoom-indicator")
      }
    }
    final
  }
)



############ test on a small app

library(shiny)
library(plotly)

ui <- fluidPage(
  selectizeInput(
    inputId = "scenarios", 
    label = "Select a scenario", 
    choices = unique(final_df$cat), 
    selected = "Historical",
    multiple = TRUE
  ),
  plotlyOutput(outputId = "p")
)

server <- function(input, output, session) {
  output$p <- renderPlotly({
    plot_ly(final_df, x = ~Year, y = ~TotCatch) %>%
      filter(cat %in% input$scenarios) %>%
      group_by(cat) %>%
      add_trace(x = ~Year, y = ~TotCatch, type = 'scatter', mode = 'lines+markers', color = ~cat)
  })
}

shinyApp(ui, server)



########
library(shiny)
library(plotly)

ui <- fluidPage(
  selectizeInput(
    inputId = "scenarios", 
    label = "Select a scenario", 
    choices = unique(final_df$cat), 
    selected = "Historical",
    multiple = TRUE
  ),
  plotlyOutput(outputId = "p")
)