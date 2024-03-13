#' Performance
#'
#' @param p2 string. Path to the GRM vector
#' @export
#' @md
performance = function(p2)
{
  ofile  = PFIFGLOBAL$statscsv

  if (file.exists(ofile))
  {
    msg = paste(basename(ofile), "already exists. Do you want to continue?")
    yesno = utils::askYesNo(msg)
    if (!yesno) return(ofile)
  }

  name_out <- tools::file_path_sans_ext(basename(p2))
  out_dir <- PFIFGLOBAL$out_dir
  fri = PFIFGLOBAL$fri

  ##############################
  ### ADD AREA AND PERIMETER ###
  ##############################

  cat("Add area and perimeters\n")

  of_p2 = p2

  # convert to sf
  p2 <- terra::vect(p2)

  p2_sf <- sf::st_as_sf(p2)

  # calculate perimeter
  p2$perim <- as.numeric(sf::st_perimeter(p2_sf))

  # calculate area
  p2$area <- as.numeric(sf::st_area(p2_sf))

  # write to file
  file.remove(of_p2)
  terra::writeVector(p2, of_p2, overwrite = TRUE)

  ###########################################
  ### EXTRACT FINAL POLYGON SUMMARY STATS ###
  ###########################################

  cat("Extract final polygon summary stats\n")

  # create list of polygon files, names and parameters
  file <- stringr::str_c(out_dir, "/", name_out, ".shp")
  out_loc <- out_dir
  grm_input <- stringr::str_c(out_dir, '/temp/spl_stack.tif')
  name <- name_out

  # create standard error function
  se <- function(x) sd(x) / sqrt(length(x))

  # load file
  p <- terra::vect(file)

  # convert to sf
  p_sf <- sf::st_as_sf(p)

  # subset non masked WAT and RD polygons
  p2_sf <- sf::st_as_sf(p[is.na(p$POLYTYPE)])
  p2 <- as.data.frame(p[is.na(p$POLYTYPE)])

  # calculate perimeter to area ratio
  p2$p_to_a <- p2$perim / p2$area
  p2$p_to_a <- round(p2$p_to_a, 3)

  # calculate msi
  p2$msi <- p2$perim / sqrt(pi * p2$area)

  # load original raster input file
  ras <- terra::rast(grm_input)

  # rename bands
  names(ras) <- c('p95', 'cc', 'cv')

  # extract pixel values
  pvals <- exactextractr::exact_extract(ras, p2_sf)

  # calculate SSE
  sse <- sapply(
    pvals,
    FUN = function(x) {
      p95_mean <- mean(x$p95, na.rm = T)
      cc_mean <- mean(x$cc, na.rm = T)
      cv_mean <- mean(x$cv, na.rm = T)

      return(c(sum((x$p95 - p95_mean) ^ 2, na.rm = T),
               sum((x$cc - cc_mean) ^ 2, na.rm = T),
               sum((x$cv - cv_mean) ^ 2, na.rm = T)))
    }
  )

  # transpose
  sse <- t(sse)

  # calculate final sums
  sse <- colSums(sse)

  # unlist values
  pvals2 <- do.call(rbind, pvals)

  # calculate global mean values
  p95_mean <- mean(pvals2$p95, na.rm = T)
  cc_mean <- mean(pvals2$cc, na.rm = T)
  cv_mean <- mean(pvals2$cv, na.rm = T)

  rm(pvals2)

  # calculate SST
  sst <- sapply(
    pvals,
    FUN = function(x) {
      return(c(sum((x$p95 - p95_mean) ^ 2, na.rm = T),
               sum((x$cc - cc_mean) ^ 2, na.rm = T),
               sum((x$cv - cv_mean) ^ 2, na.rm = T)))
    }
  )

  # transpose
  sst <- t(sst)

  # calculate final sums
  sst <- colSums(sst)

  # calculate r2 values
  r2_p95 <- 1 - (sse[1] / sst[1]) %>% round(3)
  r2_cc <- 1 - (sse[2] / sst[2]) %>% round(3)
  r2_cv <- 1 - (sse[3] / sst[3]) %>% round(3)
  r2_all <- (sum(r2_p95, r2_cc, r2_cv) / 3) %>% round(3)

  # create dataframe with values wanted
  df <- data.frame(
    alg = name,
    min_pix = (min(p2$nbPixels)),
    max_pix = (max(p2$nbPixels)),
    mean_pix = (mean(p2$nbPixels)),
    med_pix = (median(p2$nbPixels)),
    num_poly = NROW(p2),
    mean_area = mean(p2$area),
    se_area = se(p2$area),
    sd_area = sd(p2$area),
    mean_perim = mean(p2$perim),
    se_perim = se(p2$perim),
    sd_perim = sd(p2$perim),
    mean_p_a = mean(p2$p_to_a),
    se_p_a = se(p2$p_to_a),
    sd_p_a = sd(p2$p_to_a),
    mean_msi = mean(p2$msi),
    se_msi = se(p2$msi),
    sd_msi = sd(p2$msi),
    r2_p95 = r2_p95,
    r2_cc = r2_cc,
    r2_cv = r2_cv,
    r2_all = r2_all
  )

  # round numeric columns
  df = dplyr::mutate_at(df, c(
      'min_pix',
      'max_pix',
      'mean_pix',
      'med_pix',
      'mean_area',
      'se_area',
      'sd_area',
      'mean_perim',
      'se_perim',
      'sd_perim'),
    function(x)round(x, 2)) |>
    dplyr:: mutate_at(c('mean_p_a',
                'se_p_a',
                'sd_p_a',
                'mean_msi',
                'se_msi',
                'sd_msi'),
              function(x) round(x, 4))

  #####################
  ### ADD FRI STATS ###
  #####################

  cat("Add FRI stats\n")

  # load interpreter derived polygons to extract statistics
  pfri <- terra::vect(fri)

  # convert to sf
  pfri_sf <- sf::st_as_sf(pfri)

  # calculate perimeter
  pfri$perim <- as.numeric(sf::st_perimeter(pfri_sf))

  # calculate area
  pfri$area <- as.numeric(sf::st_area(pfri_sf))

  # calculate nbPixels
  pfri$nbPixels <- pfri$area / 400

  # calculate perimeter to area ratio
  pfri$p_to_a <- pfri$perim / pfri$area
  pfri$p_to_a <- round(pfri$p_to_a, 3)

  # subset all non water/ucl polygons
  p2fri_sf <- sf::st_as_sf(pfri[!(pfri$POLYTYPE %in% c('WAT', 'UCL'))])
  p2fri <- as.data.frame(pfri[!(pfri$POLYTYPE %in% c('WAT', 'UCL'))])

  # calculate msi
  p2fri$msi <- p2fri$perim / sqrt(pi * p2fri$area)

  # load original raster input file
  ras <- terra::rast(grm_input)

  # rename bands
  names(ras) <- c('p95', 'cc', 'cv')

  # extract pixel values
  pvals <- exactextractr::exact_extract(ras, p2fri_sf)

  # calculate SSE
  sse <- sapply(
    pvals,
    FUN = function(x) {
      # subset values based on coverage fraction
      x = dplyr::filter(x, coverage_fraction >= 0.5)

      p95_mean <- mean(x$p95, na.rm = T)
      cc_mean <- mean(x$cc, na.rm = T)
      cv_mean <- mean(x$cv, na.rm = T)

      return(c(sum((x$p95 - p95_mean) ^ 2, na.rm = T),
               sum((x$cc - cc_mean) ^ 2, na.rm = T),
               sum((x$cv - cv_mean) ^ 2, na.rm = T)))
    }
  )

  # transpose
  sse <- t(sse)

  # calculate final sums
  sse <- colSums(sse)

  # unlist values
  pvals2 <- do.call(rbind, pvals)

  # subset values based on coverage fraction
  pvals2 = dplyr::filter(pvals2, coverage_fraction >= 0.5)

  # calculate global mean values
  p95_mean <- mean(pvals2$p95, na.rm = T)
  cc_mean <- mean(pvals2$cc, na.rm = T)
  cv_mean <- mean(pvals2$cv, na.rm = T)

  rm(pvals2)

  # calculate SST
  sst <- sapply(
    pvals,
    FUN = function(x) {
      # subset values based on coverage fraction
      x = dplyr::filter(x, coverage_fraction >= 0.5)

      return(c(sum((x$p95 - p95_mean) ^ 2, na.rm = T),
               sum((x$cc - cc_mean) ^ 2, na.rm = T),
               sum((x$cv - cv_mean) ^ 2, na.rm = T)))
    }
  )

  # transpose
  sst <- t(sst)

  # calculate final sums
  sst <- colSums(sst)

  # calculate r2 values
  r2_p95 <- 1 - (sse[1] / sst[1]) %>% round(3)
  r2_cc <- 1 - (sse[2] / sst[2]) %>% round(3)
  r2_cv <- 1 - (sse[3] / sst[3]) %>% round(3)
  r2_all <- (sum(r2_p95, r2_cc, r2_cv) / 3) %>% round(3)

  # create dataframe with values wanted
  ms_df <- data.frame(
    alg = 'FRI',
    min_pix = (min(p2fri$area / 400)),
    max_pix = (max(p2fri$area / 400)),
    mean_pix = (mean(p2fri$area / 400)),
    med_pix = (median(p2fri$area / 400)),
    num_poly = NROW(p2fri),
    mean_area = mean(p2fri$area),
    se_area = se(p2fri$area),
    sd_area = sd(p2fri$area),
    mean_perim = mean(p2fri$perim),
    se_perim = se(p2fri$perim),
    sd_perim = sd(p2fri$perim),
    mean_p_a = mean(p2fri$p_to_a),
    se_p_a = se(p2fri$p_to_a),
    sd_p_a = sd(p2fri$p_to_a),
    mean_msi = mean(p2fri$msi),
    se_msi = se(p2fri$msi),
    sd_msi = sd(p2fri$msi),
    r2_p95 = r2_p95,
    r2_cc = r2_cc,
    r2_cv = r2_cv,
    r2_all = r2_all
  )

  # round numeric columns
  ms_df = dplyr::mutate_at(ms_df, c('min_pix',
                'max_pix',
                'mean_pix',
                'med_pix'),
              function(x)
                round(x))|>
    dplyr::mutate_at(c(
      'mean_area',
      'se_area',
      'sd_area',
      'mean_perim',
      'se_perim',
      'sd_perim'
    ),
    function(x)
      round(x, 2)) %>%
    dplyr::mutate_at(c('mean_p_a',
                'se_p_a',
                'sd_p_a',
                'mean_msi',
                'se_msi',
                'sd_msi'),
              function(x)
                round(x, 4))

  # bind df
  df <- rbind(df, ms_df)

  # write df as csv
  utils::write.csv(df, file = ofile,  row.names = F)

  return(ofile)
}
