#' Jpattern
#'
#' Plots the J-Function envelope for point pattern data.

jpattern = function(shapefile, longitude = longitude, latitude = latitude, nsim = 99){
  pattern = spatstat::ppp(longitude, latitude, window = spatstat::as.owin(shapefile))
  jenv = spatstat::envelope(pattern, Jest, nsim = nsim)
  ggplot2::ggplot(data.frame(jenv), aes(x = r))+
    ggplot2::geom_line(aes(y = lo), col='grey') +
    ggplot2::geom_line(aes(y = hi), col='grey') +
    ggplot2::geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.5, col='grey') +
    ggplot2::geom_line(aes(y = obs)) +
    ggplot2::geom_line(aes(y = theo), col = 'red') +
    ggplot2::ggtitle("J function") +
    ggplot2::ylab("J(r)") + ggplot2::xlab("r") +
    theme_qspatial()

}

