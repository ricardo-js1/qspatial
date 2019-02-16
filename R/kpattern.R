#' Kpattern
#'
#' Utilizes the Kest and Envelope functions from Spatstat and creates a visualization with
#' ggplot2.
#'
#' kpattern = function(shapefile, data, nsim = 99)
#'


kpattern = function(shapefile, data, longitude = longitude, latitude = latitude, nsim = 99){
  pattern = spatstat::ppp(data$longitude, data$latitude, window = spatstat::as.owin(shapefile))
  kenv = spatstat::envelope(pattern, Kest, nsim = nsim)
  ggplot2::ggplot(data.frame(kenv), aes(x = r)) +
    gtgplot2::geom_line(aes(y = lo), col='grey') +
    ggplot2::geom_line(aes(y = hi), col='grey') +
    ggplot2::geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.5, col='grey') +
    ggplot2::geom_line(aes(y = obs)) +
    ggplot2::geom_line(aes(y = theo), col = 'red') +
    ggplot2::ggtitle("K function")
}

