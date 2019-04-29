#' Area map function
#'
#' @import ggplot2
#' @import broom
#' @export
#'
areamap = function(shapefile = shapefile, adata = data, maptitle = NULL){
  if(length(shapefile) != length(adata)){stop("The length of the data vector must be the same as the number of polygons in the shapefile.")}

  # Joining the data and converting the spatial object to a dataframe to be compatible with ggplot2
  warning.status <- getOption("warn")
  options(warn = -1)
  shapefile$adata = adata
  shapefile.df = broom::tidy(shapefile, regions = "id")
  shapefile$id = rownames(shapefile@data)
  shapefile.df = dplyr::left_join(shapefile.df, shapefile@data, by = "id")
  options(warn = warning.status)

  # Plotting the map
  ggplot2::ggplot(shapefile.df) +
    ggplot2::geom_polygon(ggplot2::aes(x = long, y = lat, group = group, fill = adata), col = "black") +
    ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude") +
    ggplot2::scale_fill_viridis_c(name = "Frequency")+
    ggplot2::ggtitle(maptitle) +
    ggplot2::coord_map() +
    theme_qspatial()

}
