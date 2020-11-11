gg_errorbar = function(x, y, error, limit) {
  gg = ggplot() +
    geom_point(aes(x = x, y = y), color = "orangered3") +
    geom_errorbar(aes(x = x, ymin = y - error, ymax = y + error), color = "orangered3", width = 0.2) +
    ylim(limit[1], limit[2]) +
    xlab(element_blank()) +
    theme(axis.text.x = element_text(angle = 90)) +
    custom_theme_markdown
  return(gg)
}
