fpattern = function(shapefile, data, nsim = 99){
  pattern = spatstat::ppp(data$Longitude, data$Latitude, window = spatstat::as.owin(shapefile))
  fenv = spatstat::envelope(pattern, Fest, nsim = nsim)
  ggplot2::ggplot(data.frame(fenv), aes(x = r))
  ggplot2::geom_line(aes(y = lo), col='grey') +
    ggplot2::geom_line(aes(y = hi), col='grey') +
    ggplot2::geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.5, col='grey') +
    ggplot2::geom_line(aes(y = obs)) +
    ggplot2::geom_line(aes(y = theo), col = 'red') +
    ggplot2::ggtitle("F function")
}
