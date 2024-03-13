#' Call OTB GRM
#'
#' Call OTB GenericRegionMerging. This takes a lot of time and uses a lot of memory. It may be more
#' advised to run it in command line outside R
#'
#' @param thresh,spec,spat numeric. refer to paper or OTB GRM webpage for description of parameters
#' \link{https://www.orfeo-toolbox.org/CookBook-8.0/Applications/app_GenericRegionMerging.html}
#'
#' @return the path to the segmented raster file
#' @export
#' @md
generic_region_merging = function(thresh = 10, spec = 0.1, spat = 0.5)
{
  #######################
  ###RUN GRM ALGORITHM###
  #######################

  # SET PARAMETERS
  rast_in <- PFIFGLOBAL$spl
  out_p <- stringr::str_c(out_dir, '/temp')
  out_dir <- PFIFGLOBAL$out_dir
  name_out <- stringr::str_c('grm_', thresh, '_', gsub(".", "", spec, fixed = TRUE), '_', gsub(".", "", spat, fixed = TRUE))

  ret = paste(out_p, "/", name_out, ".tif", sep = "")

  if (file.exists(ret))
  {
    msg = paste(basename(ret), "already exist. Do you want to continue?")
    yesno = utils::askYesNo(msg)
    if (!yesno) return(ret)
  }

  # create function to run generic region merging
  grm_otb <- function(otb_path = "", raster_in = "", out_path = "", name = "", method = "bs", thresh = "", spec = "0.5", spat = "0.5")
  {
    # JR: space escape
    otb_path <- gsub(" ", "\\ ", otb_path)
    raster_in <- gsub(" ", "\\ ", raster_in)
    out_path <- gsub(" ", "\\ ", out_path)

    # Set configuration
    conf <- paste("-in", raster_in, "-out", paste(out_path, "/", name, ".tif", sep = ""),"-criterion", method, "-threshold", thresh, "-cw", spec, "-sw", spat)

    # apply function in command line
    cmd = paste(otb_path, "/otbcli_GenericRegionMerging", " ", conf, sep = "")
    cat(cmd, "\n")
    system(cmd)

    # save configuration for further use
    write.table(x = conf, file = paste(out_path, "/", name, "_conf.txt", sep = ""), row.names = F, col.names = F)
  }

  # run grm
  grm_otb(otb_path = otb_dir, raster_in = rast_in, out_path = out_p, name = name_out, thresh = thresh, spec = spec, spat = spat)

  return(ret)
}
