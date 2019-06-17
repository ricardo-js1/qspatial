#' reshape
#'
#'
#'

reshape = function(shapefile, grouping_id, save_cols){

  shapefile_uni = maptools::unionSpatialPolygons(shapefile, grouping_id)
  shapefile_df = shapefile@data

  shapefile_df_a = aggregate(shapefile_df[,save_cols], list(grouping_id), sum)

  row.names(shapefile_df_a) = as.character(shapefile_df_a$Group.1)

  shapefile_new = sp::SpatialPolygonsDataFrame(shapefile_uni, shapefile_df_a)
  return(shapefile_new)
}


