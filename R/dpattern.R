dpattern = function(shapefile, longitude, latitude, palette = "RdYlBu",  title = NULL, sigma = 0.01){

    shapefile.sf = as(shapefile, "sf")

    ggplot2::ggplot() +
    ggplot2::geom_sf(data = shapefile.sf, col = "black") +
    ggplot2::stat_density2d(ggplot2::aes(x = longitude, y = latitude, fill = ..level..),
                            geom = "polygon", h = sigma)+
    ggplot2::scale_fill_distiller(palette =  palette)+
    ggplot2::labs(title = title, x = 'Longitude', y = 'Latitude') +
    theme_qspatial()

}
