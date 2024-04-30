#' Call OTB GenericRegionMerging.
#'
#' Call OTB GenericRegionMerging. OTB must be installed on your computer first.
#' \url{https://www.orfeo-toolbox.org/CookBook-7.0/index.html}
#'
#' @param input SpatRaster. Multiband raster to segment
#' @param thresh,spec,spat numeric. refer to paper or OTB GRM webpage for description of parameters
#' \url{https://www.orfeo-toolbox.org/CookBook-8.0/Applications/app_GenericRegionMerging.html}
#' @param method string. refer to paper or OTB GRM webpage for description of parameters
#' \url{https://www.orfeo-toolbox.org/CookBook-8.0/Applications/app_GenericRegionMerging.html}
#' @param otb_dir string. Directory location of OTB (where you installed OTB earlier).
#' Likely "C:/OTB/bin" on Windows
#' @param ofile string. The path where to save the output of GRM
#' @return the raster produced by OTB
#' @export
#' @md
#' @examples
#' \dontrun{
#' library(terra)
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
#'
#' ofile = paste0(tempdir(), "/grm.tif")
#' grm = generic_region_merging(layers, ofile, otb_dir = "/home/jr/Logiciels/OTB-8.1.2-Linux64/bin")
#'
#' color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = TRUE)]
#' plot(grm, col= color)
#'
#' polygons = terra::as.polygons(grm)
#'
#' plotRGB(layers)
#' plot(polygons, border = "red", add = TRUE)
#' }
generic_region_merging = function(input, ofile = tempfile(fileext = ".tif"), thresh = 10, spec = 0.1, spat = 0.5,  method = "bs", otb_dir = "C:/OTB/bin")
{
  ifile = input@cpp$filenames()
  ifile = unique(ifile)
  if (length(ifile) != 1 || !file.exists(ifile))
  {
    ifile = tempfile(fileext = ".tif")
    terra::writeRaster(input, ifile)
  }

  ifile = normalizePath(ifile, mustWork = FALSE)
  ofile = normalizePath(ofile, mustWork = FALSE)
  otb_dir = normalizePath(otb_dir, mustWork = FALSE)

  cmd <- paste0(otb_dir, "/otbcli_GenericRegionMerging -in ", ifile, " -out ", ofile, " -criterion ", method, " -threshold ", thresh, " -cw ", spec, " -sw ", spat)
  cat(cmd, "\n")
  system(cmd)

  cat("Masking the result")
  o = terra::rast(ofile)
  grm = terra::mask(o, input[[1]])

  terra::writeRaster(grm, ofile, overwrite = TRUE)
  grm = terra::rast(ofile)
  return(grm)
}
