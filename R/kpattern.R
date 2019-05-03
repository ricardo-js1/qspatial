#' Kpattern
#'
#' Utilizes the Kest and Envelope functions from Spatstat and creates a visualization with
#' ggplot2.
#'
#' kpattern = function(shapefile, data, nsim = 99)
#'


kpattern = function(shapefile, longitude = longitude, latitude = latitude, nsim = 99){

  # Creating the ppp object
  pattern = spatstat::ppp(longitude, latitude, window = spatstat::as.owin(shapefile))
  pattern = spatstat::as.ppp(pattern) # removing the events outside the specified window
  pattern = spatstat::unique.ppp(pattern) # removing duplicates

  # CSR envelope for the K function

  kenv = spatstat::envelope(pattern, Kest, nsim = nsim)
  ggplot2::ggplot(data.frame(kenv), aes(x = r)) +
  ggplot2::geom_line(aes(y = lo), col='grey') +
  ggplot2::geom_line(aes(y = hi), col='grey') +
  ggplot2::geom_ribbon(aes(ymin = lo - 0.01, ymax = hi + 0.01), alpha = 0.75, fill = 'grey') +
  ggplot2::geom_line(aes(y = theo), col = 'red') +
  ggplot2::geom_line(aes(y = obs)) +
  ggplot2::ggtitle("K function") +
  ggplot2::ylab("K(r)") + ggplot2::xlab("r") +
  theme_qspatial()

}

