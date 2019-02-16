dpattern = function(shapefile, data, palette = "RdYlBu",  title = NULL, sigma = NULL){
  ggplot2::ggplot()+
    ggplot2::geom_polygon(data = shapefile, aes(x = longitude, y = latitude, group = group), col = 'black', fill = 'white')+
    ggplot2::stat_density2d(aes(x = longitude, y = latitude, fill = ..level..),
                            geom = "polygon", data = data, h=0.009)+
    ggplot2::scale_fill_distiller(palette =  palette)+
    ggplot2::labs(title = title, x = 'Longitude', y = 'Latitude')
}
