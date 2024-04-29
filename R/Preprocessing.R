#' Pre-processing stage
#'
#' Using a multiband raster that will later be used as a basis for polygon segmentation, the function masks
#' the unwanted pixel (rivers, lakes, roads, ...), smooth the layers, and normalize the value to range
#' in `[0,100]`
#'
#'
#' @param layers SpatRaster. Multiband raster to segment with \link{generic_region_merging}
#' @param smooth odd integer. 0 to skip the smoothing stage
#' @param masks List of SpatVector object that will be used to mask the raster
#' @export
#' @md
#' @examples
#' library(terra)
#'
#' f = system.file("extdata", "metrics.tif", package="pfif")
#' g = system.file("extdata", "roads.gpkg", package="pfif")
#' h = system.file("extdata", "stream.gpkg", package="pfif")
#' metrics = rast(f)
#' roads = vect(g)
#' stream = vect(h)
#'
#' plot(metrics)
#' plot(metrics[[1]])
#' plot(roads, add = TRUE)
#' plot(stream, add = TRUE)
#'
#' masks = list(roads, stream)
#'
#' layers = pre_processing(metrics, masks = masks)
#' plot(layers)
pre_processing = function(layers, smooth = 5, masks = NULL)
{
  onames = names(layers)

  if (smooth > 0)
  {
    cat("Smooth raster\n")
    terra::focal(layers, w = smooth, fun = "mean")
  }

  if (!is.null(masks))
  {
    cat("Masks\n")
    for (mask in masks)
    {
      if (terra::crs(mask) != terra::crs(layers))
        mask <- terra::project(mask, layers)

      layers <- terra::mask(layers, mask, inverse = T, touches = T)
    }
  }

  cat("Deal with missing data\n")
  layers[is.na(layers)] <- NA

  cat("Rescale in [0, 255]\n")
  layers = terra::stretch(layers, maxv = 255, minq = 0.01, maxq = 0.99)

  names(layers) = onames
  return(layers)
}
