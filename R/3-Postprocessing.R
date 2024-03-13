#' Post processing
#'
#' Post processing. Vectorize the GRM raster (as far as I can tell)
#'
#' @param grm string. path to the raster produced by generic raster merging
#' @export
#' @md
#' @return path to vectorized GRM written on disk
post_processing = function(grm)
{
  lc_f = PFIFGLOBAL$lc_f
  out_p <- stringr::str_c(PFIFGLOBAL$out_dir, '/temp')
  rast_in <- PFIFGLOBAL$spl
  name_out = tools::file_path_sans_ext(basename(grm))

  of_grm = paste(out_p, "/", name_out, ".tif", sep = "")
  of = stringr::str_c(out_dir, "/", name_out, ".shp")

  if (file.exists(of_grm) && file.exists(of))
  {
    msg = paste(basename(of_grm), "and", basename(of), "already exist. Do you want to continue?")
    yesno = utils::askYesNo(msg)
    if (!yesno) return(c(of_grm, of))
  }

  ###########################
  ### MASK MISSING VALUES ###
  ###########################

  cat("Mask missing values\n")

  spl = terra::rast(PFIFGLOBAL$spl)
  spl_pt = terra::vect(PFIFGLOBAL$spl_pt)

  # load grm raster
  p <- terra::rast(grm)

  # load seg raster
  mask <- terra::rast(rast_in)[[1]]

  # mask grm raster
  p <- terra::mask(p, mask)

  # write grm raster
  terra::writeRaster(p, of_grm, overwrite = T)

  # convert to vector based on cell value
  vec <- terra::as.polygons(p)

  # create table of number of pixels in each polygon
  num <- as.vector(terra::values(p))
  num_pix <- janitor::tabyl(num)

  # drop na row
  num_pix <- na.omit(num_pix)

  # get pixel ids from vector
  vec_dat <- tibble::tibble(id = terra::values(vec)[, 1])
  colnames(vec_dat) <- 'id'

  # loop through values and add to vector data
  vec_dat$nbPixels <- NA
  for (i in 1:NROW(vec_dat)) {
    vec_dat$nbPixels[i] <- num_pix$n[num_pix$num == vec_dat$id[i]]
  }

  # remove current column of data and add id
  # add nbPixels to vector
  vec <- vec[, -1]
  vec$id <- vec_dat$id
  vec$nbPixels <- vec_dat$nbPixels

  ##################################
  ### ADD PRE-ALLOCATED POLYGONS ###
  ##################################

  cat("Add pre-allocated polygons\n")

  # load polygon dataset
  p <- vec

  # reproject segmented polygons to ensure same crs
  p <- terra::project(p, spl_pt)

  # add non-FOR POLYTYPE polygons back in
  p2 <- rbind(p, spl_pt)

  #####################
  ### ADD LANDCOVER ###
  #####################

  cat("Add landcover\n")

  # load VLCE 2.0 landcover dataset
  lc <- terra::rast(lc_f)

  # project polygons to CRS of raster
  p_lc <- terra::project(p2, lc)

  # crop raster
  lc <- terra::crop(lc, p_lc)

  # convert to sf
  p_lcsf <- sf::st_as_sf(p_lc)

  # extract landcover values
  lc_vals <- exactextractr::exact_extract(lc, p_lcsf)

  # set landcover class key
  lc_key <- c(`0` = 'NA',
              `20` = 'Water',
              `31` = 'Snow/Ice',
              `32` = 'Rock/Rubble',
              `33` = 'Exposed/Barren Land',
              `40` = 'Bryoids',
              `50` = 'Shrubland',
              `80` = 'Wetland',
              `81` = 'Wetland-Treed',
              `100` = 'Herbs',
              `210` = 'Coniferous',
              `220` = 'Broadleaf',
              `230` = 'Mixed Wood')

  cat("Find dominant lc type in each polygon\n")

  # find dominant lc type in each polygon
  # if there are multiple modes keep them
  # apply over list
  # JR: long need progress bar
  lc_mode <- sapply(lc_vals, function(x){
    x$value <- dplyr::recode(x$value, !!!lc_key)
    x <- x |> dplyr::group_by(value) |> dplyr::summarize(sum = sum(coverage_fraction))
    m <- x$value[which(x$sum == max(x$sum))]
    # m <- get_mode2(x$value[x$coverage_fraction >= cov_frac])
    return(paste(m, collapse = " "))
  })

  # add to polygon dataset
  p2$dom_lc <- lc_mode

  # set landcover class key with single forested class
  lc_key_for <- c(`0` = 'NA',
                  `20` = 'Water',
                  `31` = 'Snow/Ice',
                  `32` = 'Rock/Rubble',
                  `33` = 'Exposed/Barren Land',
                  `40` = 'Bryoids',
                  `50` = 'Shrubland',
                  `80` = 'Wetland',
                  `81` = 'Forest',
                  `100` = 'Herbs',
                  `210` = 'Forest',
                  `220` = 'Forest',
                  `230` = 'Forest')

  cat("Find pixels with forest at least 50% of pixel\n")

  # find pixels with forest at least 50% of pixel
  # apply over list
  # JR: long need progress bar
  lc_dom_for <- sapply(lc_vals, function(x){
    x$value <- dplyr::recode(x$value, !!!lc_key_for)
    x <- x |> dplyr::group_by(value) |> dplyr::summarize(sum = sum(coverage_fraction))
    m <- x$value[which(x$sum == max(x$sum))]
    if((length(m) == 1) & (m == 'Forest')[1]){
      if(x$sum[x$value == m]/sum(x$sum) >= 0.5){
        return('Yes')
      }else{return('No')}
    }else{return('No')}
  })

  # add to polygon dataset
  p2$dom_for <- lc_dom_for

  ##############################
  ### ADD AREA AND PERIMETER ###
  ##############################

  cat("Add area and perimeter\n")

  # convert to sf
  p2_sf <- sf::st_as_sf(p2)

  # calculate perimeter
  p2$perim <- as.numeric(sf::st_perimeter(p2_sf))

  # calculate area
  p2$area <- as.numeric(sf::st_area(p2_sf))

  # write to file
  terra::writeVector(p2, of, overwrite = T)

  return(of)
}

