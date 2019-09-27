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
#' @param border Logical. If FALSE, set the borders between the polygons transparent.
#'
#' @details This function is made to work like sp's spplot function. It
#' creates a map for areal data but instead uses ggplot2 to do it.
#'
#' The function receives a shapefile from the study region and a vector
#' containing the areal data. The data in the vector must be ordered with
#' the same order of the polygons of the shapefile and have the same length
#' as the shapefile's polygons.
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
#' dengue2010map = areamap(shapefile = rio, adata = dengue.data$`2010`,
#' maptitle = "Dengue counts for Rio de Janeiro in 2010",
#' guidetitle = "Frequency")
#'
#' @import mapproj
#' @import ggplot2
#' @export
#'
areamap = function(shapefile = shapefile, adata = data, maptitle = "Map Title", guidetitle = "Guide Title", lower = NULL, upper = NULL, border = TRUE){

  adata = unlist(adata)

  if(border == "TRUE"){border.col = "black"} else {border.col = "transparent"}
  if(class(shapefile)[1] == "SpatialPolygonsDataFrame"){shapefile = as(shapefile, "sf")}

  # Plotting the map

  if(is.numeric(adata)){

    # If guide limits are not specified, it uses the minimum and the maximum for discrete data
    if(is.null(lower)){lower = min(adata)}
    if(is.null(upper)){upper = max(adata)}

    ggplot2::ggplot() +
      ggplot2::geom_sf(data = shapefile, aes(fill = adata), col = border.col) +
      ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude") +
      ggplot2::scale_fill_viridis_c(name = guidetitle, limits = c(lower, upper))+
      ggplot2::ggtitle(maptitle) +
      theme_qspatial()

  } else {

    ggplot2::ggplot() +
      ggplot2::geom_sf(data = shapefile, aes(fill = adata), col = border.col) +
      ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude") +
      ggplot2::scale_fill_viridis_d(name = guidetitle, drop = FALSE)+
      ggplot2::ggtitle(maptitle) +
      theme_qspatial()

  }
}

