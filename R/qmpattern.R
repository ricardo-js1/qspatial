#'  Quick mapping for point pattern data
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
#' The function will return a grid with two maps and two summary function plots: A
#' map with the location of each registered event, an Intensity map, and two
#' plots with the simulated envelopes of the chosen summary functions.
#'
#' There is some degree of customization allowed for the maps. It is possible
#' to change the size and color of the points in the point pattern map and the
#' palette of the Intensity map. It's also possible to change the radius used
#' in the intensity estimate. A bigger value will find more ocurrences while
#' estimating which in turn will result in a higher intensity for the area.
#'
#' The summary functions interpretation is made the following way:
#'
#' For the G and the K functions, if the observed curve is above the
#' Complete Spatial Randomness (CSR) envelope it's a suggestion of
#' spatial clustering, if it's below it suggests a spatial regularity
#' behavior. The spatial behavior will be complete random if the
#' observed curve is inside the envelope.
#'
#' The F function behavior is inverse to the G function. If the curve
#' is above the CSR envelope it suggests a spatial regularity behavior,
#' if it's below it suggests spatial clustering. If it's inside the
#' simulated envelope, it's a sign that the spatial behavior of the data
#' is completely random.
#'
#' For the J function, the J(r) = 1 line marks the CSR behavior. If the
#' observed curve is above the envelope it suggests a spatial regularity
#' behavior, if it's below it suggests spatial clustering.
#'
#' By default the function always plots two of the summary functions, so
#' the user can validate the results with both of them.
#'
#' @examples
#'
#' # Loading the example data and the included shapefile
#'
#' accidents.data = acidentes
#' recife = recife
#'
#' # This example data contains a Recife city shapefile and the
#' # coordinates of transit accidents that happened in the city
#' # in 2016. With the shapefile and the latitude and longitude
#' # coordinates the qmpattern can generate a visualization of
#' # the point pattern data and check it's spatial behavior.
#'
#' accidents.map = qmpattern(shapefile = recife,
#' longitude = accidents.data$longitude, accidents.data$latitude,
#' fun = c("G", "F"), sigma = 0.01, nsim = 99)
#'
#'
#'
#' @import ggplot2
#' @import spatstat
#' @import sf
#' @import cowplot
#' @import isoband
#' @export

qmpattern = function(shapefile, longitude = longitude, latitude = latitude, fun = c("G", "F"), sigma = 0.01, nsim = 99, palette = "RdYlBu", psize = 0.75, pcolor = "black"){
  # Checking the number of chosen summary functions
  if(length(fun) != 2){stop("Must pick two summary functions.")}

  # Loading maptools package so as.owin function works
  if(!is.loaded('maptools')){library(maptools)}

  # Version of as.owin() that works 21-04-2020

  as.owin =  function(SP) {
    # require(spatstat)
    # if (!is.na(sp::is.projected(SP)) && !sp::is.projected(SP))
    #  stop("Only projected coordinates may be converted to spatstat class objects")
    # if (!requireNamespace("spatstat", quietly = TRUE))
    #  stop("package spatstat required for .SP2owin")
    pls <- slot(SP, "polygons")
    nParts <- sapply(pls, function(x) length(slot(x, "Polygons")))
    nOwin <- sum(nParts)
    if (nOwin == 1) {
      pl <- slot(pls[[1]], "Polygons")
      crds <- slot(pl[[1]], "coords")
      colnames(crds) <- c("x", "y")
      rD <- pl[[1]]@ringDir
      if (rD == 1) crds <- crds[nrow(crds):1,]
      crds <- crds[-nrow(crds),]
      res <- spatstat::owin(poly=list(x=crds[,1], y=crds[,2]))
    } else if (nOwin > 1) {
      opls <- vector(mode="list", length=nOwin)
      io <- 1
      for (i in seq(along=pls)) {
        pl <- slot(pls[[i]], "Polygons")
        for (j in 1:nParts[i]) {
          crds <- slot(pl[[j]], "coords")
          colnames(crds) <- c("x", "y")
          rD <- slot(pl[[j]], "ringDir") # sp:::.spFindCG(crds)$rD
          hole <- slot(pl[[j]], "hole")

          if (rD == -1 && hole) crds <- crds[nrow(crds):1,]
          else if (rD == 1 && !hole) crds <- crds[nrow(crds):1,]

          crds <- crds[-nrow(crds),]

          opls[[io]] <- list(x=crds[,1], y=crds[,2])
          io <- io+1
        }
      }
      #	if (exists(".spatstat_check") && !.spatstat_check)
      if (!spatstat::spatstat.options("checkpolygons"))
        res <- spatstat::owin(bbox(SP)[1,], bbox(SP)[2,], poly = opls,
                              check=FALSE)
      # 070718 added check avoidance
      else res <- spatstat::owin(poly=opls)
    } else stop("no valid polygons")
    res
  }

  ###

  m1 = mpattern(shapefile = shapefile, longitude = longitude, latitude = latitude, psize = psize, pcolor = pcolor, title = "Point Pattern")
  m2 = dpattern(shapefile = shapefile, longitude = longitude, latitude = latitude, sigma = sigma, palette = palette, title = "Intensity")

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
  maps = cowplot::plot_grid(m1, m3, m2, m4, align = "hv", axis = "tblr", nrow = 2)
  plot(maps)

}
