#' A Wrapper for the areamap function
#'
#' @description Creates multiple choropleth maps for areal data using ggplot2.
#'
#' @usage multiplot(shapefile = shapefile, adata = data, maptitle = c("Map Title"), guidetitle = c("Guide Title"), lower = NULL, upper = NULL, nrow = NULL, ncol = NULL)
#'
#' @param shapefile A shapefile for the study region. It will be used for all the maps.
#' @param adata A dataframe for the areal data. The data can be numeric or factor.
#' @param maptitle A vector of map titles. It must have the same length as the number of maps created.
#' @param guidetitle A vector of guide titles. It must have the same length as the number of maps created.
#' @param lower The lower limit used in the guide for continuous data.
#' @param upper The upper limit used in the guide for continuous data.
#' @param nrow The number of rows in the final grid.
#' @param ncol The number of columns in the final grid.
#'
#' @details This is a wrapper for the areamap function, it will generate a gridExtra
#' grid containing up to four maps. This function will run the areamap for each of the
#' maps.
#'
#' The data must be organized the same way as if creating a single map. The data in each
#' variable must be in the same order of the shapefile's polygons and must have
#' the same length.
#'
#' The title of the maps and of the guides must be entered in a vector, they
#' will be used in the order they are entered.
#'
#' If a lower and upper limit are entered in the function they will be used
#' for all the maps that uses continuous data. Since this function will
#' likely be used to plot maps for the data across different times, it's a good
#' practice to standardize the guide's scale. In case the lower and upper limits
#' are not specified, the areamap function will use the minimum and maximum
#' values found in the data, which will result in different scales for each map.
#'
#' @export

multiplot = function(shapefile = shapefile, adata = data, maptitle = c("Map Title"), guidetitle = c("Guide Title"), lower = NULL, upper = NULL, nrow = NULL, ncol = NULL){
  n = length(adata)

  if(n > 4){stop("You can only plot up to 4 maps at once.")}

  if(n == 1){
    areamap(shapefile = shapefile, adata = adata[,1], maptitle = maptitle[1], guidetitle = guidetitle[1], lower = lower, upper = upper)
  }

  if(n == 2){
    m1 = areamap(shapefile = shapefile, adata = adata[,1], maptitle = maptitle[1], guidetitle = guidetitle[1], lower = lower, upper = upper)
    m2 = areamap(shapefile = shapefile, adata = adata[,2], maptitle = maptitle[2], guidetitle = guidetitle[2], lower = lower, upper = upper)
    gridExtra::grid.arrange(m1, m2, nrow = nrow, ncol = ncol)
  }

  if(n == 3){
    m1 = areamap(shapefile = shapefile, adata = adata[,1], maptitle = maptitle[1], guidetitle = guidetitle[1], lower = lower, upper = upper)
    m2 = areamap(shapefile = shapefile, adata = adata[,2], maptitle = maptitle[2], guidetitle = guidetitle[2], lower = lower, upper = upper)
    m3 = areamap(shapefile = shapefile, adata = adata[,3], maptitle = maptitle[3], guidetitle = guidetitle[3], lower = lower, upper = upper)
    gridExtra::grid.arrange(m1, m2, m3, nrow = nrow, ncol = ncol)
  }

  if(n == 4){
    m1 = areamap(shapefile = shapefile, adata = adata[,1], maptitle = maptitle[1], guidetitle = guidetitle[1], lower = lower, upper = upper)
    m2 = areamap(shapefile = shapefile, adata = adata[,2], maptitle = maptitle[2], guidetitle = guidetitle[2], lower = lower, upper = upper)
    m3 = areamap(shapefile = shapefile, adata = adata[,3], maptitle = maptitle[3], guidetitle = guidetitle[3], lower = lower, upper = upper)
    m4 = areamap(shapefile = shapefile, adata = adata[,4], maptitle = maptitle[4], guidetitle = guidetitle[4], lower = lower, upper = upper)
    gridExtra::grid.arrange(m1, m2, m3, m4, nrow = nrow, ncol = ncol)
  }

}
