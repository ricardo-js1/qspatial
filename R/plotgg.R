#' ggplot2 syntax generator
#'
#' @import ggplot2
#' @export
plotgg = function(shapefile = shapefile, adata = adata, maptitle = "Map Title",
                  continuous = TRUE, guide.title = "Guide Title"){

  if(continuous == TRUE){

    cat("Use this to plot the map with ggplot2:
         ggplot(put the resulting tidied shapefile here!) +
         geom_polygon(aes(x = long, y = lat, group = group, fill = '",deparse(substitute(adata)),"'), col = 'black') +
         ylab('Latitude') +
         scale_fill_viridis_c(name = '",guide.title,"') +
         ggtitle('",maptitle,"') +
         coord_map() +
         theme_qspatial()", sep = "")

  } else {

    cat("Use this to plot the map with ggplot2:
         ggplot(put the resulting tidied shapefile here!) +
         geom_polygon(aes(x = long, y = lat, group = group, fill = '",deparse(substitute(adata)),"'), col = 'black') +
         xlab('Longitude') +
         ylab('Latitude') +
         scale_fill_viridis_d(name = '",guide.title,"') +
         ggtitle('",maptitle,"') +
         coord_map() +
         theme_qspatial()", sep = "")

  }

    # Joining the data and converting the spatial object to a dataframe to be compatible with ggplot2
    warning.status <- getOption("warn")
    options(warn = -1)
    shapefile$adata = adata
    shapefile.df = broom::tidy(shapefile, regions = "id")
    shapefile$id = rownames(shapefile@data)
    shapefile.df = dplyr::left_join(shapefile.df, shapefile@data, by = "id")
    options(warn = warning.status)

    # Returning the fortified shapefile ready to be plotted with ggplot2
    return(shapefile.df)

}

