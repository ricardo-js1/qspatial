#' A ggplot2 theme for qspatial!
#'
#'
theme_qspatial = function(base_size = 11, base_family = "Arial",
                          base_line_size = base_size / 170,
                          base_rect_size = base_size / 170){

  theme_minimal(base_size = base_size,
                base_family = base_family,
                base_line_size = base_line_size) %+replace%
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
