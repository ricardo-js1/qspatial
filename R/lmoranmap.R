#' Quick visualization and analysis of areal data
#'
#' @description
#' Creates a complete visualization and spatial autocorrelation analysis with Moran's I for areal data.
#' @usage
#' lmoranmap(shapefile, adata, sign = 0.05, knearest = FALSE, k = 3)
#'
#' @param shapefile A shapefile of the study region.
#' @param adata A vector with the data for each area unit.
#' @param sign The significance level for the Local Moran's I
#' @param knearest Logical argument to create the neighborhood using the k nearest neighbors.
#' @param k The number of neighbors used when knearest = TRUE. The default is 3.
#'
#' @details This function utilizes ggplot2 and spdep to create a complete visualization
#' and spatial autocorrelation analysis for areal data.
#'
#' The lenght of the areal data vector must be the same as the number of
#' polygons in the shapefile. The data will be merged into it using
#' the tidy function from the broom package so the maps can be created with
#' ggplot2. All the steps in this transformation are made automatically by
#' the function.
#'
#' A neighborhood must be created for the spatial analysis, the default method
#' utilizes the spdep's poly2nb function which considers area units that
#' share borders as neighbors. Since this can be a problem when the
#' shapefile contains islands there is a option to create the neighbors with
#' spdep's knearneigh function, which considers the k-nearest polygons as
#' neighbors.
#'
#' The function returns four maps: One for the areal data, one with Local Moran's I results,
#' one with Local Moran's significant p-values (where the significance can be changed via the
#' sign argument) and one with the categories made with spdep's moran.plot function.
#'
#' @examples
#'
#' # Loading the example data and the included shapefile
#'
#' dengue.data = dengue
#' rio = rioshapefile
#'
#' # The example data contains dengue counts between 2009 and 2013 for
#' # the Rio de Janeiro State. We can run the spatial analysis for each
#' # of these years.
#'
#' dengue2010 = lmoranmap(shapefile = rio, adata = dengue.data$x2010)
#'
#' # This will generate a grid containing a cloropleth map for the
#' # dengue counts, a map with the results from Local Moran's I, a
#' # map indicating which regions had significant results on the
#' # Local Moran's I and a map with the categories utilized on
#' # spdep's moran.plot function.
#'
#' # The significance level used here can be changed via the sign
#' # parameter on the function.
#'
#' # The neighborhood for the areal data is created by spdep's
#' # poly2nb function by default. Since this kind of neighborhood
#' # can be problematic for regions that are not contiguous as a
#' # whole it's also possible to create the neighborhood via the
#' # knearneigh function.
#'
#' dengue2010 = lmoranmap(shapefile = rio, adata = dengue.data$x2010, knearneigh = TRUE, k = 3)
#'
#' # By default it counts the 3 nearest polygons as neighbors, but
#' # this can be easily changed via the k parameter on the function.
#'
#' @export
#' @import sp

lmoranmap = function(shapefile = shapefile, adata = data, sign = 0.05, knearest = FALSE, k = 3){
  if(length(shapefile) != length(adata)){stop("The length of the data vector must be the same as the number of polygons in the shapefile.")}

  # Adding the data vector to the spatial object

  shapefile$adata = adata

  # Creating the neighborhood and the weights matrix using spdep's functions

  if(knearest == FALSE){

    # Contiguous neighbours

    shapefile.nb = spdep::poly2nb(shapefile)
    weights.matrix = spdep::nb2listw(shapefile.nb)

    } else {

    # K nearest neighbours

    coords = sp::coordinates(shapefile)
    ids = row.names(as(shapefile, "data.frame"))
    shapefile.nb = spdep::knn2nb(spdep::knearneigh(coords, k = k), row.names = ids)
    weights.matrix = spdep::nb2listw(shapefile.nb)

  }

  # Running Local Moran's I and adding the results to the spatial object

  lmoran = spdep::localmoran(adata, weights.matrix)
  shapefile$lmoran = lmoran[,1]
  shapefile$pmoran = lmoran[,5] <= sign
  shapefile$pmoran.sig = ifelse(shapefile$pmoran == "TRUE", "Significant", "Not Significant")

  shapefile$scaled.data = scale(adata)
  shapefile$lagged.data = spdep::lag.listw(weights.matrix, shapefile$scaled.data)

  shapefile$Moran.Cat = factor(
    ifelse(shapefile$scaled.data > 0 & shapefile$lagged.data > 0, "High - High",
    ifelse(shapefile$scaled.data > 0 & shapefile$lagged.data < 0, "High - Low",
    ifelse(shapefile$scaled.data < 0 & shapefile$lagged.data > 0, "Low - High",
    ifelse(shapefile$scaled.data < 0 & shapefile$lagged.data < 0, "Low - Low",
    "Not Significant")))))

  # Converting the spatial object to a data.frame

  warning.status <- getOption("warn")
  options(warn = -1)
  # The tidy function will make a warning each time it binds a character
  # and a factor vector, this will silence this warnings only for this section
  # of the code
  shapefile.df = broom::tidy(shapefile, regions = "id")
  shapefile$id = rownames(shapefile@data)
  shapefile.df = dplyr::left_join(shapefile.df, shapefile@data, by = "id")
  options(warn = warning.status)

  # Plotting the maps

  # The areal data
  m1 =  ggplot2::ggplot(shapefile.df) +
    ggplot2::geom_polygon(ggplot2::aes(x = long, y = lat, group = group, fill = adata), col = "black") +
    ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude") +
    ggplot2::scale_fill_viridis_c(name = "Frequency")+
    ggplot2::ggtitle("Areal Data") +
    theme_qspatial()


  # Local Moran's I results for each area
  m2 = ggplot2::ggplot(shapefile.df) +
    ggplot2::geom_polygon(aes(x = long, y = lat, fill = lmoran, group = group), col = "black") +
    ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude") +
    ggplot2::scale_fill_viridis_c(name = "Moran's I")+
    ggplot2::ggtitle("Local Moran's I") +
    theme_qspatial()

  # The areas with resulting p-values under the specified significance level
  m3 = ggplot2::ggplot(shapefile.df) +
    ggplot2::geom_polygon(ggplot2::aes(x = long, y = lat, fill = pmoran.sig, group = group), col = "black") +
    ggplot2::scale_fill_manual(values=c("white", "darkred"), name = "Spatial Dependence")+
    ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude") +
    ggplot2::ggtitle("Significant p-values") +
    theme_qspatial()

  # Spdep Moran.plot categories
  m4 = ggplot2::ggplot(shapefile.df) +
    ggplot2::geom_polygon(ggplot2::aes(x = long, y = lat, fill = Moran.Cat, group = group), col = "black") +
    ggplot2::scale_fill_manual(values=c("red","pink","light blue","blue"), name = "Moran's Categories")+
    ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude") +
    ggplot2::ggtitle("Moran's I categories") +
    theme_qspatial()

  # Plotting and returning an object with the maps

  maps = gridExtra::grid.arrange(m1, m2, m3, m4, ncol = 2)
  plot(maps)
  return(maps)
}

