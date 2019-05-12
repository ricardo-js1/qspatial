#' Area map function
#'
#' @description
#' Creates a choropleth map for areal data using ggplot2.
#'
#' @usage areamap(shapefile = shapefile, adata = adata,
#' maptitle = "Map Title", guidetitle = "Guide Title")
#'
#' @param shapefile A shapefile of the study region.
#' @param adata A vector with the areal data.
#' @param maptitle A title for the map.
#' @param guidetitle A title for the map guide.
#'
#' @details This function is made to work like sp's spplot function. It
#' creates a map for areal data but instead uses ggplot2 to do it.
#'
#' The function receives a shapefile from the study region and a vector
#' containing the areal data. The data in the vector must be ordered with
#' the same order of the polygons of the shapefile and have the same length
#' as the shapefile's polygons.
#'
#' When creating a map with ggplot2 it's necessary to transform the
#' SpatialPolygonsDataFrame object in a dataframe with broom's tidy
#' function. This function does all the necessary transformations and
#' returns the map.
#'
#' This function generates only a visualization of the spatial data.
#' Further analysis of the spatial data can be made with the lmoranmap
#' function.
#'
#' @examples
#' # Loading the example data and the included shapefile
#'
#' dengue.data = dengue
#' rio = rioshapefile
#'
#' # The example data contains dengue counts between 2009 and 2013 for
#' # the Rio de Janeiro State. To create the map for one of these years
#' # we just need to use the areamap function.
#'
#' dengue2010map = areamap(shapefile = rio, adata = dengue.data$´2010´,
#' maptitle = "Dengue counts for Rio de Janeiro in 2010",
#' guidetitle = "Frequency")
#'
#' @import ggplot2
#' @import broom
#' @export
#'
areamap = function(shapefile = shapefile, adata = data, maptitle = "Map Title", guidetitle = "Guide Title", lower = NULL, upper = NULL){

  adata = unlist(adata)

  if(length(shapefile) != length(adata)){stop("The length of the data vector must be the same as the number of polygons in the shapefile.")}

  # If guide limits not specified, it uses the minimum and the maximum for discrete data
  if(is.null(lower)){lower = min(adata)}
  if(is.null(upper)){upper = max(adata)}

  # Joining the data and converting the spatial object to a dataframe to be compatible with ggplot2
  warning.status <- getOption("warn")
  options(warn = -1)
  shapefile$adata = adata
  shapefile.df = broom::tidy(shapefile, regions = "id")
  shapefile$id = rownames(shapefile@data)
  shapefile.df = dplyr::left_join(shapefile.df, shapefile@data, by = "id")
  options(warn = warning.status)

  # Plotting the map

  if(is.numeric(adata)){

    ggplot2::ggplot(shapefile.df) +
      ggplot2::geom_polygon(ggplot2::aes(x = long, y = lat, group = group, fill = adata), col = "black") +
      ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude") +
      ggplot2::scale_fill_viridis_c(name = guidetitle, limits = c(lower, upper))+
      ggplot2::ggtitle(maptitle) +
      ggplot2::coord_map() +
      theme_qspatial()

  } else {

    ggplot2::ggplot(shapefile.df) +
      ggplot2::geom_polygon(ggplot2::aes(x = long, y = lat, group = group, fill = adata), col = "black") +
      ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude") +
      ggplot2::scale_fill_viridis_d(name = guidetitle, drop = FALSE)+
      ggplot2::ggtitle(maptitle) +
      ggplot2::coord_map() +
      theme_qspatial()

  }
}

