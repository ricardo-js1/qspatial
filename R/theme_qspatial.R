#' A ggplot2 theme for qspatial!
#'
#'
theme_qspatial = function(){
    theme(
    legend.title = element_text(size = 8, face = "italic"),
    panel.border = element_rect(fill = NA, color = "#dbdbdb", size = rel(1)),
    panel.grid = element_line(colour = "#dbdbdb", linetype = 2),
    panel.background  = element_blank(),
    plot.background = element_rect(fill = "#ffffff", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.key = element_rect(fill = "transparent", color = NA),
    axis.text = element_text(size = 8, face = "italic"),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12),
    legend.text = element_text(size = 8)
  )
}
