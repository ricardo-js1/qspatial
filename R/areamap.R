areamap = function(shapefile = shapefile, adata = data, maptitle = NULL){
  if(length(shapefile) != length(adata)){stop("The length of the data vector must be the same as the number of polygons in the shapefile.")}

  # Joining the data and converting the spatial object to a dataframe to be compatible with ggplot2
  shapefile$adata = adata
  shapefile.df = broom::tidy(shapefile, regions = "id")
  shapefile$id = rownames(rj@data)
  shapefile.df = dplyr::left_join(shapefile.df, shapefile@data, by = "id")

  # Plotting the map
  ggplot2::ggplot(shapefile.df) +
    ggplot2::geom_polygon(aes(x = long, y = lat, group = group, fill = adata), col = "black") +
    ggplot2::xlab("Latitude") + ggplot2::ylab("Longitude") +
    ggplot2::scale_fill_viridis_c()+
    ggplot2::ggtitle(maptitle)
}
