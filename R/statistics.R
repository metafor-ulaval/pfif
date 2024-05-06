#' Computes summary statistics about the polygons
#'
#' Computes summary statistics about the polygons to describe how they are spread, shaped, sized,
#' homogeneous and so on. By defaut it computes the polygon density (number of polygons per unit area).
#' Then it compute the Moran's index, the mean and standard deviation of the attributes of interest
#' recorded with the polygons. It can computes these indices on the 'area' of the polygon and on each
#' attribute provided by the user.
#'
#' @param polygons sf object with polygons
#' @param on character. A vector of attribute on which to compute the metrics
#' @export
#' @examples
#' library(sf)
#' f = system.file("extdata", "inventory.gpkg", package="pfif")
#' inventory = st_read(f)
#'
#' polygon_metrics(inventory)
#' polygon_metrics(inventory, on = c("area"))
polygon_statistics = function(polygons, on = NULL)
{
  holes = get_holes(polygons)
  polygons$area = as.numeric(sf::st_area(polygons))
  area = sum(polygons$area)/1e6
  D = nrow(polygons)/area # polygon density
  D_holes = nrow(holes) / area # hole density
  A_holes = sum(holes$area)


  res = data.frame(D = D, D_holes, A_holes)

  for (attr in on)
  {
    I_attr = morans_index(polygons, attr)
    mean_attr = mean(polygons[[attr]], na.rm = T)
    std_attr = sd(polygons[[attr]], na.rm = T)

    res[[paste0(attr, "_I")]] = I_attr
    res[[paste0(attr, "_mean")]] = mean_attr
    res[[paste0(attr, "_std")]] = std_attr
  }

  return(res)
}

morans_index = function(s, on, plot = FALSE)
{
  nb <- spdep::poly2nb(s, queen=TRUE)
  lw <- spdep::nb2listw(nb, style="W", zero.policy=TRUE)
  y <- s[[on]]

  if (plot)
  {
    y.lag <- spdep::lag.listw(lw, y)
    plot(y.lag ~ y, pch=16, asp=1, xlab = on, ylab = paste0("Lag ", on))
    graphics::abline(lm(y.lag ~ y), col="blue")
  }

  I <- spdep::moran(y, lw, length(nb), spdep::Szero(lw))[1]
  return(I$I)
}

get_holes <- function(x)
{
  sf::st_agr(x) = "constant"
  x <- sf::st_cast(x, "POLYGON")
  l <- as.data.frame(sf:::st_coordinates(x))
  l <- l[l$L1 > 1,]
  l <- split(l,list(l$L1, l$L2))
  l <- Filter(function(x) { nrow(x) > 0 }, l)
  l <- lapply(l, function(x)
  {
    sf::st_polygon(list(as.matrix(x[,1:2])))
  })
  h <- sf::st_as_sf(sf::st_sfc(l))
  sf::st_geometry(h) <- "geometry"
  h = sf::st_as_sf(h)
  h$area = sf::st_area(h)
  return( h )
}

