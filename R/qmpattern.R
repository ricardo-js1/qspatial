#'  Quick mapping for point pattern data
#' @export
#'
#' Generates the visualization and analysis of first and second order properties of point pattern data.
#'
#' @param shapefile A shapefile of the study region.
#' @param longitude The name of the variable containing the longitude.
#' @param latitude The name of the variable containing the latitude.
#' @param fun A vector with two summary functions. "G" for nearest neighbor function, "F" for empty space function, "K" for Ripley's K function and "J" for the J function.
#' @param sigma Optional. The radius used to estimate the Intensity, if a radius is not chosen it will be calculated using the bw.diggle function.
#' @param nsim The number of simulations generated when computing the envelope. The default is 99.
#' @param palette The color pallete used in the Intensity plot.
#'
#' @details
#'
#' The function generates a quick visualization and analysis of point pattern data with
#' ggplot2 and spatstat's summary functions. The default option is to analyze using
#' the G and F functions, they work well for larger datasets and the simulation does not
#' take much time to complete. There are options included to pick two functions between
#' K, G, F and J summary functions, but it's important to notice that the runtime
#' of the K function is considerably bigger.
#'
#'
#'
#'


qmpattern = function(shapefile, longitude = longitude, latitude = latitude, fun = c("G", "F"), sigma = 0.01, nsim = 99, palette = "RdYlBu"){
  # Checking the number of chosen summary functions
  if(length(fun) != 2){stop("Must pick two summary functions.")}

  m1 = mpattern(shapefile = shapefile, longitude = longitude, latitude = latitude, title = "Point Pattern")
  m2 = dpattern(shapefile = shapefile, sigma = sigma, palette = palette, title = "Intensity")

  # First function
  if(fun[1] == "G"){m3 = gpattern(shapefile = shapefile, longitude = longitude, latitude = latitude, nsim = nsim)}
  if(fun[1] == "F"){m3 = fpattern(shapefile = shapefile, longitude = longitude, latitude = latitude, nsim = nsim)}
  if(fun[1] == "K"){m3 = jpattern(shapefile = shapefile, longitude = longitude, latitude = latitude, nsim = nsim)}
  if(fun[1] == "J"){m3 = kpattern(shapefile = shapefile, longitude = longitude, latitude = latitude, nsim = nsim)}

  # Second function
  if(fun[2] == "G"){m4 = gpattern(shapefile = shapefile, longitude = longitude, latitude = latitude, nsim = nsim)}
  if(fun[2] == "F"){m4 = fpattern(shapefile = shapefile, longitude = longitude, latitude = latitude, nsim = nsim)}
  if(fun[2] == "K"){m4 = jpattern(shapefile = shapefile, longitude = longitude, latitude = latitude, nsim = nsim)}
  if(fun[2] == "J"){m4 = kpattern(shapefile = shapefile, longitude = longitude, latitude = latitude, nsim = nsim)}

  # Plotting the maps
  maps = gridExtra::grid.arrange(m1, m2, m3, m4, ncol = 2, nrow = 2)
  plot(maps)
  return(maps)
}
