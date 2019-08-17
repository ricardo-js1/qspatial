#' mpattern
#'
#' Creates a visualization of point pattern data over a shapefile.
#'
#' @import ggplot2
#' @export


mpattern = function(shapefile,longitude = longitude, latitude = latitude,  title = NULL, pcolor = 'black', psize=.95){

    shapefile.sf = as(shapefile, "sf")

    ggplot2::ggplot() +
    ggplot2::geom_sf(data = shapefile.sf, col = "black") +
    ggplot2::geom_point(ggplot2::aes(x = longitude, y = latitude), size= psize, col = pcolor)+
    ggplot2::labs(title = title, x = 'Longitude', y = 'Latitude') +
    theme_qspatial()

}
