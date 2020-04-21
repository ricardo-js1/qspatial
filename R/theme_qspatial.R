#' A ggplot2 theme for qspatial!
#'
#' @import ggplot2
#' @export

theme_qspatial = function(){
    ggplot2::theme(
    legend.title = ggplot2::element_text(size = 8, face = "italic"),
    panel.border = ggplot2::element_rect(fill = NA, color = "#dbdbdb", size = ggplot2::rel(1)),
    panel.grid = ggplot2::element_line(colour = "#dbdbdb", linetype = 2),
    panel.background  = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(fill = "#ffffff", color = NA),
    legend.background = ggplot2::element_rect(fill = "transparent", color = NA),
    legend.key = ggplot2::element_rect(fill = "transparent", color = NA),
    axis.text = ggplot2::element_text(size = 8, face = "italic"),
    axis.title = ggplot2::element_text(size = 10),
    plot.title = ggplot2::element_text(size = 12),
    legend.text = ggplot2::element_text(size = 8),
    plot.subtitle = ggplot2::element_text(size = 7)
  )
}
