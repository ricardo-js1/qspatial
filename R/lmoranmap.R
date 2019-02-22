#' Quick visualization and analysis of areal data
#'
#' Teste teste
#'
#' @export

lmoranmap = function(shapefile = shapefile, adata = data, sign = 0.05){
  if(length(shapefile) != length(adata)){stop("The length of the data vector must be the same as the number of polygons in the shapefile.")}

  # Adding the data vector to the spatial object

  shapefile$adata = adata

  # Creating the neighborhood and the weights matrix using spdep's functions

  shapefile.nb = spdep::poly2nb(shapefile)
  weights.matrix = spdep::nb2listw(shapefile.nb)

  # Running Local Moran's I and adding the results to the spatial object

  lmoran = spdep::localmoran(adata, weights.matrix)
  shapefile$lmoran = lmoran[,1]
  shapefile$pmoran = lmoran[,5] < sign

  shapefile$scaled.data = scale(adata)
  shapefile$lagged.data = spdep::lag.listw(weights.matrix, shapefile$scaled.data)

  shapefile$Moran.Cat = factor(
    ifelse(shapefile$scaled.data > 0 & shapefile$lagged.data > 0, "High - High",
    ifelse(shapefile$scaled.data > 0 & shapefile$lagged.data < 0, "High - Low",
    ifelse(shapefile$scaled.data < 0 & shapefile$lagged.data > 0, "Low - High",
    ifelse(shapefile$scaled.data < 0 & shapefile$lagged.data < 0, "Low - Low",
    "Not Significant")))))

  # Converting the spatial object to a data.frame

  shapefile.df = broom::tidy(shapefile, regions = "id")
  shapefile$id = rownames(shapefile@data)
  shapefile.df = dplyr::left_join(shapefile.df, shapefile@data, by = "id")

  # Plotting the maps

  # The areal data
  m1 =  ggplot2::ggplot(shapefile.df) +
    ggplot2::geom_polygon(ggplot2::aes(x = long, y = lat, group = group, fill = adata), col = "black") +
    ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude") +
    ggplot2::scale_fill_viridis_c()+
    ggplot2::ggtitle("Data")

  # Local Moran's I results for each area
  m2 = ggplot2::ggplot(shapefile.df) +
    ggplot2::geom_polygon(aes(x = long, y = lat, fill = lmoran, group = group), col = "black") +
    ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude") +
    ggplot2::scale_fill_viridis_c()+
    ggplot2::ggtitle("Local Moran's I")

  # The areas with resulting p-values under the specified significance
  m3 = ggplot2::ggplot(shapefile.df) +
    ggplot2::geom_polygon(ggplot2::aes(x = long, y = lat, fill = pmoran, group = group), col = "black") +
    ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude") +
    ggplot2::ggtitle("Significant p-values")

  # Spdep Moran.plot categories
  m4 = ggplot2::ggplot(shapefile.df) +
    ggplot2::geom_polygon(ggplot2::aes(x = long, y = lat, fill = Moran.Cat, group = group), col = "black") +
    ggplot2::scale_fill_manual(values=c("red","pink","light blue","blue"))+
    ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude") +
    ggplot2::coord_fixed()+
    ggplot2::ggtitle("Moran's I categories")

  # Plotting and returning an object with the maps

  maps = gridExtra::grid.arrange(m1, m2, m3, m4, ncol = 2)
  plot(maps)
  return(maps)
}
