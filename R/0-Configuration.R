####################################
### SET CODE AND FILE PARAMETERS ###
####################################

PFIFGLOBAL = new.env()

#' Set global parameters
#'
#' Set global parameters to make working other functions
#'
#' When `root` points to the directory `RMF_EFI_layers/` it automatically searches for other files
#' including:
#' \itemize{
#' \item{RMF_EFI_layers/Polygons Inventory/RMF_PolygonForest.shp}
#' \item{RMF_EFI_layers/SPL100 metrics/RMF_20m_T130cm_p95.tif}
#' \item{RMF_EFI_layers/SPL100 metrics/RMF_20m_T130cm_2m_cov.tif}
#' \item{RMF_EFI_layers/SPL100 metrics/RMF_20m_T130cm_cv.tif}
#' \item{RMF_EFI_layers/Roads/RMF_roads.shp}}
#'
#' @param root string. Root directory with all data from the drive.
#' @param out_dir string. Output directory for files generated.
#' @param lc_f string. File location of 2018 VLCE 2.0 landcover data using 2018 because it is
#' the year of Romeo ALS acquisition. Download here: \link{https://opendata.nfis.org/mapserver/nfis-change_eng.html}
#' \link{https://opendata.nfis.org/downloads/forest_change/CA_forest_VLCE2_2018.zip}
#' @param otb_dir string. Directory location of OTB (where you installed OTB earlier). Likely "C:/OTB/bin"
#' on Windows.
#' @examples
#' root = "/media/jr/Alexandre/RMF_EFI_layers/"
#' out_dir <- '/home/jr/Documents/Ulaval/2024 PDR/Alexandre - ontario'
#' lc_f <- "/home/jr/Téléchargements/CA_forest_VLCE2_2018/CA_forest_VLCE2_2018.tif"
#' otb_dir <- "/home/jr/Logiciels/OTB-8.1.2-Linux64/bin"
#'
#' set_pfif_globals(root, out_dir, lc_f, otb_dir)
#' @export
#' @md
set_pfif_globals = function(root, out_dir, lc_f, otb_dir = "C:/OTB/bin")
{
  PFIFGLOBAL$root = root
  PFIFGLOBAL$out_dir = out_dir
  PFIFGLOBAL$lc_f = lc_f
  PFIFGLOBAL$otb_dir = otb_dir
  PFIFGLOBAL$fri <- paste0(root,  "Polygons Inventory/RMF_PolygonForest.shp")
  PFIFGLOBAL$p95_f <- paste0(root, "SPL100 metrics/RMF_20m_T130cm_p95.tif")
  PFIFGLOBAL$cc_f  <- paste0(root, "SPL100 metrics/RMF_20m_T130cm_2m_cov.tif")
  PFIFGLOBAL$cv_f  <- paste0(root, "SPL100 metrics/RMF_20m_T130cm_cv.tif")
  PFIFGLOBAL$roads_f <- paste0(root, "Roads/RMF_roads.shp")
  PFIFGLOBAL$spl <- paste0(out_dir, '/temp/spl_stack.tif')
  PFIFGLOBAL$spl_pt <- paste0(out_dir, '/temp/water_roads_polygons.shp')
  PFIFGLOBAL$statscsv <- paste0(out_dir, '/summary_stats.csv')
}
