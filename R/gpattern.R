
gpattern = function(shapefile, longitude = longitude, latitude = latitude, nsim = 99){
  pattern = spatstat::ppp(longitude, latitude, window = spatstat::as.owin(shapefile))
  genv = spatstat::envelope(pattern, Gest, nsim = nsim)
  ggplot2::ggplot(data.frame(genv), aes(x = r)) +
    ggplot2::ggtitle("G function") +
    ggplot2::geom_line(aes(y = lo), col='grey') +
    ggplot2::geom_line(aes(y = hi), col='grey') +
    ggplot2::geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.5, col='grey') +
    ggplot2::geom_line(aes(y = obs)) +
    ggplot2::geom_line(aes(y = theo), col = 'red') +
    ggplot2::ylab("G(r)") + ggplot2::xlab("r") +
    theme_qspatial()
}
