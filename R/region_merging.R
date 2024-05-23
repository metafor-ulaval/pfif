#' Segmentation of a raster into polygons
#'
#' `generic_region_merging` calls the GenericRegionMerging software from Orfeo ToolBox (OTB).
#' OTB must be installed on your computer first. See \url{https://www.orfeo-toolbox.org/CookBook-7.0/index.html}
#'
#' @param input SpatRaster. Multiband raster to segment
#' @param thresh,spec,spat numeric. refer to paper or OTB GRM webpage for description of parameters
#' \url{https://www.orfeo-toolbox.org/CookBook-8.0/Applications/app_GenericRegionMerging.html}
#' @param method string. refer to paper or OTB GRM webpage for description of parameters
#' \url{https://www.orfeo-toolbox.org/CookBook-8.0/Applications/app_GenericRegionMerging.html}
#' @param otb_dir string. Directory location of OTB (where you installed OTB earlier).
#' Likely "C:/OTB/bin" on Windows
#' @param ofile string. The path where to save the output of GRM
#' @param simplify boolean. Simplify the output of the GRM
#' @return the raster produced by OTB
#' @export
#' @rdname segmentation
#' @md
#' @examples
#' \dontrun{
#' library(terra)
#' library(sf)
#'
#' f = system.file("extdata", "metrics.tif", package="pfif")
#' g = system.file("extdata", "roads.gpkg", package="pfif")
#' h = system.file("extdata", "stream.gpkg", package="pfif")
#' metrics = rast(f)
#' roads = vect(g)
#' stream = buffer(vect(h), 2)
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
#' otb_dir = "/home/jr/Logiciels/OTB-8.1.2-Linux64/bin"
#' polygons = generic_region_merging(layers, otb_dir = otb_dir)
#'
#' plotRGB(layers, r = 3, g = 1, b = 4)
#' plot(st_geometry(polygons), add = TRUE)
#' }
generic_region_merging = function(input, ofile = tempfile(fileext = ".tif"), thresh = 50, spec = 0.5, spat = 0.5,  method = "bs", simplify = TRUE, otb_dir = "C:/OTB/bin")
{
  ptr = tryCatch({ input@ptr }, error = function(e) { input@cpp })

  ifile = ptr$filenames()
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

  polygons = terra::as.polygons(grm)
  polygons$grm = NULL

  # Clean one pixel polygons
  areas = terra::expanse(polygons)
  small = areas < Reduce(`*`, terra::res(input))*1.5
  if (any(small))
  {
    polygons = terra::combineGeoms(polygons[!small], polygons[small])
    polygons$ID = 1:length(polygons)
    polygons = terra::aggregate(polygons, by = "ID")
    polygons$ID = NULL
    polygons$agg_n = NULL
  }

  polygons = sf::st_as_sf(polygons)

  if (simplify)
    polygons <- rmapshaper::ms_simplify(polygons, keep = 0.25,  keep_shapes = TRUE, weighting = 1)

  return(polygons)
}

# @param polygons sf object. Vector of polygons
# @param k integer Number of clusters
kmeans_region_merging = function(input, polygons, k = 10)
{
  polygons = sf::st_as_sf(polygons)

  data = lapply(1:terra::nlyr(input), function(i) { terra::extract(input[[i]], polygons, 'mean')[[2]] })
  data = do.call(cbind, data)
  data[is.nan(data)] = 0

  group = stats::kmeans(data, k)

  polygons$group = group$cluster
  sf::st_agr(polygons) <- "constant"
  grm_dissolved_poly = dplyr::group_by(polygons, group) |> dplyr::summarise()
  sf::st_agr(grm_dissolved_poly) <- "constant"
  grm_dissolved_poly = sf::st_cast(grm_dissolved_poly, "POLYGON")
  grm_dissolved_poly$group = NULL
  grm_dissolved_poly
}

kmeans_generic_region_merging = function(input, ofile = tempfile(fileext = ".tif"), thresh = 10, spec = 0.1, spat = 0.5,  method = "bs", k = 10, otb_dir = "C:/OTB/bin")
{
  polygons = generic_region_merging(input, ofile, thresh, spec, spat, method, otb_dir)
  if (k > 0) polygons = kmeans_region_merging(input, polygons, k)
  return(polygons)
}
