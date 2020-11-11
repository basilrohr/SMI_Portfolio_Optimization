# Correlation plot
gg_cor = function(cor, lab_size, tl.cex, title = " ", theme = NULL) {
  gg = ggcorrplot(cor, title = title, legend.title = "Cor",
                  lab = T, lab_size = lab_size, tl.cex = tl.cex, ggtheme = theme)
  return(gg)
}


# Error bar plot
gg_errorbar = function(x, y, error, limits, xangle, ylab, title = NULL, theme = NULL) {
  gg = ggplot() +
    geom_point(aes(x = x, y = y), color = "orangered3") +
    geom_errorbar(aes(x = x, ymin = y - error, ymax = y + error), color = "orangered3",
                  width = 0.02*length(x)) +
    lims(y = limits) +
    labs(x = element_blank(), y = ylab, title = title) +
    theme(axis.text.x = element_text(angle = xangle)) +
    theme
  return(gg)
}


# Bootstrap samples efficiency frontier plot
gg_bs_eff_frontier = function(bs_out, xlim_sf = 1, ylim_sf = 1, size = 0.5, title = NULL,
                              theme = NULL) {
  gg = ggplot()
  for (i in 1:nrow(bs_out$sample_eff_frontier_sd)) {
    gg = gg +
      geom_path(aes_string(x = bs_out$sample_eff_frontier_sd[i,],
                           y = bs_out$sample_eff_frontier_mu[i,]), alpha = 0.05)
  }
  gg = gg + geom_point(aes_string(x = bs_out$sample_mvp_sd,
                                  y = bs_out$sample_mvp_mu, color = "'MVP'"), size = size) +
    geom_point(aes_string(x = bs_out$sample_tp_sd,
                          y = bs_out$sample_tp_mu, color = "'TP'"), size = size) +
    lims(x = xlim_sf * c(0, 5), y = ylim_sf * c(-0.2, 0.6)) +
    labs(x = "Volatility [%]", y = "Expected return [%]", title = title, color = NULL) +
    scale_color_manual(values = c("MVP" = "cornflowerblue", "TP" = "orangered3")) +
    theme
  return(gg)
}


# Sharpe ratio as a function of shrinking factor plot
gg_shrinking2D = function(slices, r = F, cor = F, title = NULL, theme = NULL, interval = "1d") {
  seq = seq(0, 1, by = 0.01)
  if (r) {
    sr_os_SMI = sapply(out_of_sample_vec(slices, seq, interval = interval),
                       function(x){return(x$sr_os_SMI)})
    sr_os_SMI_groups = sapply(out_of_sample_vec(slices, seq, interval = interval),
                              function(x){return(x$sr_os_SMI_groups)})
    xlab = "Return"
  }
  if (cor) {
    sr_os_SMI = sapply(out_of_sample_vec(slices, 0, seq, interval = interval),
                       function(x){return(x$sr_os_SMI)})
    sr_os_SMI_groups = sapply(out_of_sample_vec(slices, 0, seq, interval = interval),
                              function(x){return(x$sr_os_SMI_groups)})
    xlab = "Correlation"
  }
  gg = ggplot() +
    geom_line(aes(x = seq, y = sr_os_SMI, color = "SMI")) +
    geom_segment(aes(x = seq[which.max(sr_os_SMI)], y = -Inf,
                     xend = seq[which.max(sr_os_SMI)], yend = max(sr_os_SMI)), color = "black",
                 linetype = 2) +
    geom_point(aes(x = seq[which.max(sr_os_SMI)], y = max(sr_os_SMI)), color = "black") +
    geom_text(aes(x = seq[which.max(sr_os_SMI)], y = max(sr_os_SMI),
                  label = paste0("x = ", format(round(seq[which.max(sr_os_SMI)], 2), nsmall = 2),
                                 "\ny = ", format(round(max(sr_os_SMI), 2), nsmall = 2))),
              color = "black", hjust = ifelse(seq[which.max(sr_os_SMI)] < 2/3, 0, 1),
              vjust = -2/3) +
    geom_line(aes(x = seq, y = sr_os_SMI_groups, color = "Groups")) +
    geom_segment(aes(x = seq[which.max(sr_os_SMI_groups)], y = -Inf,
                     xend = seq[which.max(sr_os_SMI_groups)], yend = max(sr_os_SMI_groups)),
                 color = "orangered3", linetype = 2) +
    geom_point(aes(x = seq[which.max(sr_os_SMI_groups)], y = max(sr_os_SMI_groups)),
               color = "orangered3") +
    geom_text(aes(x = seq[which.max(sr_os_SMI_groups)], y = max(sr_os_SMI_groups),
                  label = paste0("x = ", format(round(seq[which.max(sr_os_SMI_groups)], 2), nsmall = 2),
                                 "\ny = ", format(round(max(sr_os_SMI_groups), 2), nsmall = 2))),
              color = "black", hjust = ifelse(seq[which.max(sr_os_SMI_groups)] < 2/3, 0, 1),
              vjust = -2/3) +
    lims(y = c(0, 2)) +
    labs(x = paste0(xlab, " shrinking factor"), y = "Sharpe ratio", title = title, color = NULL) +
    scale_color_manual(values = c("SMI" = "black", "Groups" = "orangered3")) +
    theme
  return(gg)
}


# Sharpe ratio as a function of both shrinking factors plot
gg_shrinking3D = function(slices, groups = F, title = NULL, theme = NULL, interval = "1d") {
  if (groups) {
    selector = "sr_os_SMI_groups"
  } else {
    selector = "sr_os_SMI"
  }
  grid = expand.grid(seq(0, 1, by = 0.05), seq(0, 1, by = 0.05))
  sr = sapply(out_of_sample_vec(slices, grid[,1], grid[,2]), function(x){return(x[[selector]])})
  
  gg = ggplot(mapping = aes(x = grid[,2], y = grid[,1], z = sr)) +
    geom_raster(aes(fill = sr), interpolate = T) +
    stat_contour(bins = 30, color = "black", alpha = 0.5) +
    scale_fill_gradientn(colors = c("honeydew", "brown1", "red4"), values = c(0, 0.8, 1),
                         name = "Sharpe ratio") +
    geom_point(aes(x = grid[,2][which.max(sr)], y = grid[,1][which.max(sr)]), color = "grey") +
    geom_text(aes(x = grid[,2][which.max(sr)], y = grid[,1][which.max(sr)],
                  label = paste0("x = ", format(round(grid[,2][which.max(sr)], 2), nsmall = 2),
                                 "\ny = ", format(round(grid[,1][which.max(sr)], 2), nsmall = 2),
                                 "\nz = ", format(round(max(sr), 2), nsmall = 2))),
              color = "grey", vjust = -0.2) +
    labs(x = "Correlation shrinking coefficient", y = "Return shrinking coefficient", title = title) +
    theme
  return(gg)
}
