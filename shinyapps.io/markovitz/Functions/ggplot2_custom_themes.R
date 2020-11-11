# The ggplot2 custom themes are defined.

custom_theme_markdown = theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                              axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                              panel.background = element_rect(fill = "white", colour = "white"),
                              panel.grid.major = element_line(colour = "grey", size = 0.4),
                              panel.grid.minor = element_line(colour = "grey", size = 0.2),
                              axis.ticks = element_blank(),
                              text = element_text(family = "Times"))

custom_theme_shiny = theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                           axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                           plot.title = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0), hjust = 0.5),
                           panel.background = element_rect(fill = "white", colour = "white"),
                           panel.grid.major = element_line(colour = "grey", size = 0.4),
                           panel.grid.minor = element_line(colour = "grey", size = 0.2),
                           axis.ticks = element_blank(),
                           text = element_text(size = 15))
