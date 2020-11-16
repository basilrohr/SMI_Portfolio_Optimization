gg_cor = function(cor, lab_size, tl.cex, title = " ", theme = NULL) {
  gg = ggcorrplot(cor, title = title, legend.title = "Cor",
                  lab = T, lab_size = lab_size, tl.cex = tl.cex, ggtheme = theme)
  return(gg)
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
  gg = gg + geom_point(aes(x = mvpp[,1], y = mvpp[,2], color = "MVP"), size = size) +
    geom_point(aes(x = tpp[,1], y = tpp[,2], color = "TP"), size = size) +
    lims(x = fxlim * c(0, 5), y = fylim * c(-0.2, 0.6)) +
    labs(x = "Volatility [%]", y = "Expected return [%]", title = title, color = NULL) +
    scale_color_manual(values = c("MVP" = "cornflowerblue", "TP" = "orangered3")) +
    theme
  gg
}

gg_shrinking2D = function(x1, x2, x1_name, x2_name, xlab, title = NULL, theme = NULL) {
  seq = seq(0, 1, length = length(x1))
  gg = ggplot() + geom_line(aes(x = seq, y = x1, color = x1_name)) +
    geom_line(aes(x = seq, y = x2, color = x2_name)) +
    geom_segment(aes(x = seq[which.max(x1)], y = -Inf, xend = seq[which.max(x1)],
                     yend = max(x1)), color = "orangered3", linetype = 2) +
    geom_point(aes(x = seq[which.max(x1)], y = max(x1)), color = "orangered3") +
    geom_text(aes(x = seq[which.max(x1)], y = max(x1),
                  label = paste0("x = ", format(round(seq[which.max(x1)], 2), nsmall = 2),
                                 "\ny = ", format(round(max(x1), 2), nsmall = 2))),
              color = "black", hjust = ifelse(seq[which.max(x1)] < 2/3, 0, 1),
              vjust = -2/3) +
    geom_segment(aes(x = seq[which.max(x2)], y = -Inf, xend = seq[which.max(x2)],
                     yend = max(x2)), color = "black", linetype = 2) +
    geom_point(aes(x = seq[which.max(x2)], y = max(x2)), color = "black") +
    geom_text(aes(x = seq[which.max(x2)], y = max(x2),
                  label = paste0("x = ", format(round(seq[which.max(x2)], 2), nsmall = 2),
                                 "\ny = ", format(round(max(x2), 2), nsmall = 2))),
              color = "black", hjust = ifelse(seq[which.max(x2)] < 2/3, 0, 1),
              vjust = -2/3) +
    labs(x = paste0(xlab, " shrinking factor"), y = "Sharpe ratio", title = title, color = NULL) +
    lims(y = c(-0.25, 1)) +
    scale_color_manual(values = c("black", "orangered3")) +
    theme
  gg
}

gg_shrinking3D = function(grid, z, title = NULL, theme = NULL) {
  gg = ggplot(mapping = aes(x = grid[,2], y = grid[,1], z = z)) +
    geom_raster(aes(fill = z), interpolate = T) +
    stat_contour(bins = 30, color = "black", alpha = 0.5) +
    scale_fill_gradientn(colors = c("honeydew", "brown1", "red4"), values = c(0, 0.8, 1),
                         name = "Sharpe ratio") +
    geom_point(aes(x = grid[,2][which.max(z)], y = grid[,1][which.max(z)]), color = "grey") +
    geom_text(aes(x = grid[,2][which.max(z)], y = grid[,1][which.max(z)],
                  label = paste0("x = ", format(round(grid[,2][which.max(z)], 2), nsmall = 2),
                                 "\ny = ", format(round(grid[,1][which.max(z)], 2), nsmall = 2),
                                 "\nz = ", format(round(max(z), 2), nsmall = 2))),
              color = "grey", vjust = -0.2) +
    labs(x = "Correlation shrinking coefficient", y = "Return shrinking coefficient", title = title) +
    theme
  return(gg)
}
