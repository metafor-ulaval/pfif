#' Pre processing
#'
#' First step after `set_pfif_globals()`. It generates `spl_stack.tif` and `water_roads_polygons.shp`
#' in the `temp/` folder of the `out_dir` directory.
#'
#' @return The paths of the files written on disk
#' @export
#' @md
pre_processing = function()
{
  p95_f = PFIFGLOBAL$p95_f
  cc_f = PFIFGLOBAL$cc_f
  cv_f = PFIFGLOBAL$cv_f
  roads_f = PFIFGLOBAL$roads_f
  fri = PFIFGLOBAL$fri
  out_dir = PFIFGLOBAL$out_dir

  if (file.exists(PFIFGLOBAL$spl) && file.exists(PFIFGLOBAL$spl_pt))
  {
    msg = paste(basename(PFIFGLOBAL$spl), "and", basename(PFIFGLOBAL$spl_pt), "already exist. Do you want to continue?")
    yesno = utils::askYesNo(msg)
    if (!yesno) return(c(PFIFGLOBAL$spl, PFIFGLOBAL$spl_pt))
  }

  ###################################################
  ### LOAD MULTI BAND ALS RASTER FOR SEGMENTATION ###
  ###################################################

  cat("Load multi band ALS raster for segmentation\n")

  # stack rasters
  spl <- terra::rast(c(p95_f, cc_f, cv_f))

  # apply smoothing function on 5 cell square
  spl[[1]] <- terra::focal(spl[[1]], w = 5, fun = "mean")
  spl[[2]] <- terra::focal(spl[[2]], w = 5, fun = "mean")
  spl[[3]] <- terra::focal(spl[[3]], w = 5, fun = "mean")

  # create ALS template with all values equal to 1
  spl_temp <- spl[[1]]
  spl_temp[] <- 1

  ##################
  ### MASK ROADS ###
  ##################

  cat("Mask roads\n")

  # NOTE DE JR:
  # je ne comprend pas pourquoi faire un truc si compliqué alors que juste un buffer + mask
  # ca marche

  # load roads layer
  roads <- terra::vect(roads_f)

  # reproject to match lidar
  roads <- terra::project(roads, spl)

  # create roads polygon
  spl_r <- terra::mask(spl_temp, roads, touches = T)
  npix <- sum(terra::values(spl_r), na.rm = T)
  spl_r <- terra::as.polygons(spl_r)
  names(spl_r) <- 'POLYTYPE'
  spl_r$POLYTYPE <- 'RDS'
  spl_r$nbPixels <- npix

  # mask road pixels to NA
  spl <- terra::mask(spl, roads, inverse = T, touches = T)

  ###########################
  ### MASK WATER POLYGONS ###
  ###########################

  cat("Mask water polygons\n")

  # water polygons from the FRI are masked and re-added after segmentation

  # load photo interpreted polygons
  poly <- terra::vect(fri)

  # subset polygons that are WAT
  poly_sub <- poly[poly$POLYTYPE %in% c('WAT')]

  # reproject to match lidar
  poly_sub <- terra::project(poly_sub, spl)

  # loop through water polygons, mask raster, and vectorize
  # Time > 2 min

  # NOTE DE JR:
  # je ne comprend pas pourquoi faire un truc si compliqué alors que juste un buffer + mask
  # ca marche je crois. On a pas besoin de npix ni de POLYTYPE

  pb = txtProgressBar(min = 0, max = length(poly_sub), style = 3)
  for (i in 1:length(poly_sub))
  {
    setTxtProgressBar(pb, i)

    pt <- poly_sub$POLYTYPE[i] # # NOTE DE JR: inutile, tjs WAT
    if (i == 1)
    {
      spl_pt <- spl_temp |> terra::crop(poly_sub[i], snap = 'out') |> terra::mask(poly_sub[i], touches = T)
      npix <- sum(terra::values(spl_pt), na.rm = T)
      spl_pt <- terra::as.polygons(spl_pt)
      names(spl_pt) <- 'POLYTYPE'
      spl_pt$POLYTYPE <- pt
      spl_pt$nbPixels <- npix
    }
    else
    {
      if (evaluate::is.error(spl_temp |> terra::crop(poly_sub[i], snap = 'out') |> terra::mask(poly_sub[i], touches = T)) == F)
      {
        spl_hold <- spl_temp |> terra::crop(poly_sub[i], snap = 'out') |> terra::mask(poly_sub[i], touches = T)
        npix <- sum(terra::values(spl_hold), na.rm = T)
        spl_hold <- terra::as.polygons(spl_hold)
        names(spl_hold) <- 'POLYTYPE'
        spl_hold$POLYTYPE <- pt
        spl_hold$nbPixels <- npix
        spl_pt <- rbind(spl_pt, spl_hold)
      }
    }
  }
  close(pb)

  # reproject whole FRI to match lidar
  poly <- terra::project(poly, spl)

  # mask lidar outside of FRI
  spl <- terra::mask(spl, poly, inverse = F, touches = T)

  # mask WAT polygons
  spl <- terra::mask(spl, poly_sub, inverse = T, touches = T)

  ###############################################
  ### COMBINE ROAD AND WATER POLYGON DATASETS ###
  ###############################################

  cat("Combine road and water polygon datasets\n")

  spl_pt <- rbind(spl_pt, spl_r)

  ##########################################
  ### DEAL WITH MISSING DATA AND RESCALE ###
  ##########################################

  cat("Deal with missing data and rescale\n")

  # if any band is missing values set all to NA
  spl[is.na(spl[[1]])] <- NA
  spl[is.na(spl[[2]])] <- NA
  spl[is.na(spl[[3]])] <- NA

  # create function to rescale values from 0 to 100 using 1 and 99 percentile
  scale_100 <- function(x)
  {
    # calculate 1st and 99th percentile of input raster
    val <- terra::values(x, mat = F)
    perc <- stats::quantile(val, probs = c(0.01, 0.99), na.rm = T)

    # rescale raster using 1st and 99th %
    x <- (x - perc[1]) / (perc[2] - perc[1]) * 100

    #reset values below 0 and above 100
    x[x < 0] <- 0
    x[x > 100] <- 100

    return(x)
  }

  # rescale rasters from 0 to 100
  spl[[1]] <- scale_100(spl[[1]])
  spl[[2]] <- scale_100(spl[[2]])
  spl[[3]] <- scale_100(spl[[3]])

  cat("Write output to disk\n")

  # check if main dir exists and create
  if (dir.exists(out_dir) == F)
  {
    dir.create(out_dir)
  }

  # check if temp dir exists and create
  if (dir.exists(file.path(out_dir, 'temp')) == F)
  {
    dir.create(file.path(out_dir, 'temp'))
  }

  # write raster to tif
  terra::writeRaster(spl, filename = PFIFGLOBAL$spl, overwrite = T)

  # write spl_pt
  terra::writeVector(spl_pt, filename = PFIFGLOBAL$spl_pt,  overwrite = T)

  return(c(PFIFGLOBAL$spl, PFIFGLOBAL$spl_pt))
}
