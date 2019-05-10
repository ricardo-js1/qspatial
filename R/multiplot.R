multiplot = function(shapefile = shapefile, adata = data, maptitle = c("Map Title"), guidetitle = "Guide Title", lower = NULL, upper = NULL, nrow = NULL, ncol = NULL){
  n = length(adata)

  if(n > 4){stop("You can only plot up to 4 maps at once.")}

  if(n == 1){
    areamap(shapefile = shapefile, adata = adata[,1], maptitle = maptitle[1], guidetitle = guidetitle, lower = lower, upper = upper)
  }

  if(n == 2){
    m1 = areamap(shapefile = shapefile, adata = adata[,1], maptitle = maptitle[1], guidetitle = guidetitle, lower = lower, upper = upper)
    m2 = areamap(shapefile = shapefile, adata = adata[,2], maptitle = maptitle[2], guidetitle = guidetitle, lower = lower, upper = upper)
    gridExtra::grid.arrange(m1, m2, nrow = nrow, ncol = ncol)
  }

  if(n == 3){
    m1 = areamap(shapefile = shapefile, adata = adata[,1], maptitle = maptitle[1], guidetitle = guidetitle, lower = lower, upper = upper)
    m2 = areamap(shapefile = shapefile, adata = adata[,2], maptitle = maptitle[2], guidetitle = guidetitle, lower = lower, upper = upper)
    m3 = areamap(shapefile = shapefile, adata = adata[,3], maptitle = maptitle[3], guidetitle = guidetitle, lower = lower, upper = upper)
    gridExtra::grid.arrange(m1, m2, m3, nrow = nrow, ncol = ncol)
  }

  if(n == 4){
    m1 = areamap(shapefile = shapefile, adata = adata[,1], maptitle = maptitle[1], guidetitle = guidetitle, lower = lower, upper = upper)
    m2 = areamap(shapefile = shapefile, adata = adata[,2], maptitle = maptitle[2], guidetitle = guidetitle, lower = lower, upper = upper)
    m3 = areamap(shapefile = shapefile, adata = adata[,3], maptitle = maptitle[3], guidetitle = guidetitle, lower = lower, upper = upper)
    m4 = areamap(shapefile = shapefile, adata = adata[,4], maptitle = maptitle[4], guidetitle = guidetitle, lower = lower, upper = upper)
    gridExtra::grid.arrange(m1, m2, m3, m4, nrow = nrow, ncol = ncol)
  }

}
