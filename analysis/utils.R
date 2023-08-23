# set of functions for data visualization and analysis of experiments 1 and 2.

library(tidyverse)
library(ggdist)
library(wesanderson)

# behavioural data

# table distribution

# original source: https://www.anthonyschmidt.co/post/2020-06-03-making-apa-tables-with-gt/
# with my own tweaks

dist_tab <- function(x, title = " ") {
  gt(x, groupname_col = 'fluency', rowname_col = 'judgement') %>%
    tab_options(
      table.border.top.color = "white",
      heading.title.font.size = px(16),
      column_labels.border.top.width = 3,
      column_labels.border.top.color = "black",
      column_labels.border.bottom.width = 3,
      column_labels.border.bottom.color = "black",
      table_body.border.bottom.color = "black",
      table.border.bottom.color = "white",
      table.width = pct(100),
      table.background.color = "white"
    ) %>%
    cols_align(align="center") %>%
    tab_style(
      style = list(
        cell_borders(
          sides = c("top", "bottom"),
          color = "white",
          weight = px(1)
        ),
        cell_text(
          align="center"
        ),
        cell_fill(color = "white", alpha = NULL)
      ),
      locations = cells_body(
        columns = everything(),
        rows = everything()
      )
    ) %>%
    #title setup
    tab_header(
      title = html("<i>", title, "</i>")
    ) %>%
    opt_align_table_header(align = "left") 
    
}

# visualization of RT

# functions from psyteachR for violin plots
# source: https://psyteachr.github.io/introdataviz/advanced-plots.html

geom_split_violin <- function (mapping = NULL, 
                               data = NULL, 
                               stat = "ydensity", 
                               position = "identity", ..., 
                               draw_quantiles = NULL, 
                               trim = TRUE, 
                               scale = "area", 
                               na.rm = FALSE, 
                               show.legend = NA, 
                               inherit.aes = TRUE) {
  GeomSplitViolin <- ggplot2::ggproto(
    "GeomSplitViolin", 
    GeomViolin, 
    draw_group = function(self, data, ..., draw_quantiles = NULL) {
      data <- transform(data, 
                        xminv = x - violinwidth * (x - xmin), 
                        xmaxv = x + violinwidth * (xmax - x))
      grp <- data[1,'group']
      newdata <- plyr::arrange(
        transform(data, x = if(grp%%2==1) xminv else xmaxv), 
        if(grp%%2==1) y else -y
      )
      newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
      newdata[c(1,nrow(newdata)-1,nrow(newdata)), 'x'] <- round(newdata[1, 'x']) 
      if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
        stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 1))
        quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
        aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
        aesthetics$alpha <- rep(1, nrow(quantiles))
        both <- cbind(quantiles, aesthetics)
        quantile_grob <- ggplot2::GeomPath$draw_panel(both, ...)
        ggplot2:::ggname("geom_split_violin", 
                         grid::grobTree(ggplot2::GeomPolygon$draw_panel(newdata, ...), quantile_grob))
      } else {
        ggplot2:::ggname("geom_split_violin", ggplot2::GeomPolygon$draw_panel(newdata, ...))
      }
    }
  )
  
  
  layer(data = data, 
        mapping = mapping, 
        stat = stat, 
        geom = GeomSplitViolin, 
        position = position, 
        show.legend = show.legend, 
        inherit.aes = inherit.aes, 
        params = list(trim = trim, 
                      scale = scale, 
                      draw_quantiles = draw_quantiles, 
                      na.rm = na.rm, ...)
  )
}

geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  # define flat violin geom
  GeomFlatViolin <- ggplot2::ggproto(
    "Violinist", 
    Geom,
    setup_data = function(data, params) {
      data$width <- data$width %||%
        params$width %||% (ggplot2::resolution(data$x, FALSE) * 0.9)
      
      # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
      data %>%
        dplyr::group_by(group) %>%
        dplyr::mutate(ymin = min(y),
                      ymax = max(y),
                      xmin = x,
                      xmax = x + width / 2) %>%
        dplyr::ungroup()
      
    },
    draw_group = function(data, panel_scales, coord) {
      # Find the points for the line to go all the way around
      data <- transform(data, xminv = x,
                        xmaxv = x + violinwidth * (xmax - x))
      
      # Make sure it's sorted properly to draw the outline
      newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                       plyr::arrange(transform(data, x = xmaxv), -y))
      
      # Close the polygon: set first and last point the same
      # Needed for coord_polar and such
      newdata <- rbind(newdata, newdata[1,])
      
      ggplot2:::ggname("geom_flat_violin", 
                       ggplot2::GeomPolygon$draw_panel(newdata, panel_scales, coord))
    },
    draw_key = draw_key_polygon,
    default_aes = aes(weight = 1, colour = "grey20", 
                      fill = "white", size = 0.5,
                      alpha = NA, linetype = "solid"),
    required_aes = c("x", "y")
  )
  
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}




# eye tracking data

# visualization eye movements