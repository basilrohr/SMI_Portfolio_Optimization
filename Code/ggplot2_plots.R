custom_theme_markdown = theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                              axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                              plot.title = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0), hjust = 0.5),
                              panel.background = element_rect(fill = "white", colour = "white"),
                              panel.grid.major = element_line(colour = "grey", size = 0.4),
                              panel.grid.minor = element_line(colour = "grey", size = 0.2),
                              axis.ticks = element_blank(),
                              text = element_text(family = "Times", size = 15),
                              legend.key =  element_blank())

custom_theme_shiny = theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                           axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                           plot.title = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0), hjust = 0.5),
                           panel.background = element_rect(fill = "white", colour = "white"),
                           panel.grid.major = element_line(colour = "grey", size = 0.4),
                           panel.grid.minor = element_line(colour = "grey", size = 0.2),
                           axis.ticks = element_blank(),
                           text = element_text(size = 15),
                           legend.key =  element_blank())


gg_cor = function(cor, lab_size, tl.cex, title = " ", theme = NULL) {
  gg = ggcorrplot(cor, lab = T, lab_size = lab_size, tl.cex = tl.cex, title = title, ggtheme = theme) +
    scale_fill_gradient2(high = "orangered2", mid = "honeydew", low = "cornflowerblue",
                         limit = c(-1, 1), name = "Cor")
  gg
}

gg_errorbar = function(x, y, error, ylim = c(min(y - error), max(y + error)), ylab = "y",
                       title = NULL, theme = NULL) {
  gg = ggplot() +
    geom_point(aes(x = x, y = y), color = "orangered3") +
    geom_errorbar(aes(x = x, ymin = y - error, ymax = y + error), color = "orangered3",
                  width = 0.02*length(x)) +
    lims(y = ylim) +
    labs(x = element_blank(), y = ylab, title = title) +
    theme(axis.text.x = element_text(angle = ifelse(length(x) > 5, 90, 0))) +
    theme
  gg
}

gg_bootstrap_ef = function(ef, fxlim = 1, fylim = 1, size = 0.5, title = NULL, theme = NULL) {
  gg = ggplot()
  for (i in seq_along(ef)) {
    gg = gg + geom_path(aes_string(x = ef[[i]][,1], y = ef[[i]][,2]), alpha = 0.05)
  }
  mvpp = t(sapply(ef, function(x){mvp_point(x)}))
  tpp = t(sapply(ef, function(x){tp_point(x)}))
  gg = gg +
    geom_point(aes(x = mvpp[,1], y = mvpp[,2], color = "MVP"), size = size) +
    geom_point(aes(x = tpp[,1], y = tpp[,2], color = "TP"), size = size) +
    lims(x = fxlim * c(0, 5), y = fylim * c(-0.2, 0.6)) +
    labs(x = "Volatility [%]", y = "Expected return [%]", title = title, color = NULL) +
    scale_color_manual(values = c("MVP" = "cornflowerblue", "TP" = "orangered3")) +
    guides(fill = guide_legend(ncol = 2)) +
    theme(legend.position = "bottom") +
    theme
  gg
}

gg_shrink2D = function(sr, srname, xlab, title = NULL, theme = NULL) {
  seq = seq(0, 1, length = length(sr[[1]]))
  labels = numeric(length(srname))
  for (i in seq_along(srname)) {
    labels[i] = paste0(srname[i], "\nmax at (",
                       format(round(seq[which.max(sr[[i]])], 2), nsmall = 2), "/",
                       format(round(max(sr[[i]]), 2), nsmall = 2), ")")
  }
  colors = c("black", "orangered3", "cornflowerblue")
  colors = colors[1:length(srname)]
  names(colors) = labels
  gg = ggplot()
  for (i in seq_along(sr)) {
    eval(substitute(expr = {gg = gg +
      geom_line(aes(x = seq, y = sr[[i]], color = labels[i])) +
      geom_segment(aes(x = seq[which.max(sr[[i]])], y = -Inf, xend = seq[which.max(sr[[i]])],
                       yend = max(sr[[i]])), color = colors[i], linetype = 2) +
      geom_point(aes(x = seq[which.max(sr[[i]])], y = max(sr[[i]])), color = colors[i])},
      env = list(i = i)))
  }
  gg = gg +
    lims(y = c(-0.25, 1.25)) +
    labs(x = paste0(xlab, " shrinkage factor"), y = "Sharpe ratio", title = title, color = NULL) +
    scale_color_manual(values = colors) +
    theme(legend.position = "bottom") +
    theme
  gg
}

gg_shrink3D = function(grid, z, title = NULL, theme = NULL) {
  gg = ggplot(mapping = aes(x = grid[,2], y = grid[,1], z = z)) +
    geom_raster(aes(fill = z), interpolate = T) +
    stat_contour(bins = 20, color = "black", alpha = 0.2) +
    scale_fill_gradientn(colors = c("honeydew", "orangered2", "orangered4"), values = c(0, 0.8, 1),
                         name = paste0("Sharpe ratio", "\nmax at (",
                                       format(round(grid[,2][which.max(z)], 2), nsmall = 2), "/",
                                       format(round(grid[,1][which.max(z)], 2), nsmall = 2), "/",
                                       format(round(max(z), 2), nsmall = 2), ")")) +
    geom_segment(aes(x = grid[,2][which.max(z)], y = -Inf, xend = grid[,2][which.max(z)],
                     yend = grid[,1][which.max(z)]), color = "honeydew", linetype = 2) +
    geom_segment(aes(x = -Inf, y = grid[,1][which.max(z)], xend = grid[,2][which.max(z)],
                     yend = grid[,1][which.max(z)]), color = "honeydew", linetype = 2) +
    geom_point(aes(x = grid[,2][which.max(z)], y = grid[,1][which.max(z)]), color = "honeydew") +
    coord_fixed() +
    labs(x = "Correlation shrinkage coefficient", y = "Return shrinkage coefficient", title = title) +
    theme(legend.title = element_text(size = 12)) +
    theme
  gg
}
