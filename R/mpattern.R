#' mpattern
#'
#' Creates a visualization of point pattern data over a shapefile.
#'


mpattern = function(shapefile, data, longitude = longitude, latitude = latitude,  title = NULL, pcolor = 'black', psize=.95, fill = 'white'){
  ggplot2::ggplot()+
    ggplot2::geom_polygon(data = shapefile, aes(x = long, y = lat, group = group), col = 'black', fill = fill)+
    ggplot2::geom_point(data = data, aes(x = longitude, y = latitude), size= psize, col = pcolor)+
    ggplot2::labs(title = title, x = 'Longitude', y = 'Latitude')
}
