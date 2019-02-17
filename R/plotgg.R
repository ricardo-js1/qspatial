plotgg = function(shapefile = "shapefile", variable = "adata", maptitle = NULL,
                  continuous = TRUE){

  if(continuous == TRUE){

    cat("Use this to plot the map with ggplot2:
      ggplot(",shapefile,")+
      geom_polygon(aes(x = long, y = lat, group = group, fill ='",variable,"'), col =","black) +
      xlab(","Longitude) +
      ylab(Latitude) +
      scale_fill_viridis_c() +
      ggtitle('",maptitle,"')")

  } else {

    cat("Use this to plot the map with ggplot2:
         ggplot(",shapefile,")+
         geom_polygon(aes(x = long, y = lat, group = group, fill ='",variable,"'), col =","black) +
         xlab(","Longitude) +
         ylab(Latitude) +
         scale_fill_viridis_d() +
         ggtitle('",maptitle,"')")

  }
}

