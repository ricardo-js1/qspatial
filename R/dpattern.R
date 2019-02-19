dpattern = function(shapefile, longitude, latitude, palette = "RdYlBu",  title = NULL, sigma = 0.01){

    ggplot2::ggplot()+
    ggplot2::geom_polygon(data = shapefile, aes(x = long, y = lat, group = group),
                          col = 'black', fill = 'white')+
    ggplot2::stat_density2d(aes(x = longitude, y = latitude, fill = ..level..),
                            geom = "polygon", h = sigma)+
    ggplot2::scale_fill_distiller(palette =  palette)+
    ggplot2::labs(title = title, x = 'Longitude', y = 'Latitude')

}
