# pfif

Integration of Photo Interpreted and LiDAR Attributes into a Polygonal Forest Inventory Framework

## Installation

```r
remotes::install_github("metafor-ulaval/pfif")
```

## Example

``` r
library(pfif)
library(terra)
library(sf)

# load the data
f = system.file("extdata", "metrics.tif", package="pfif")
g = system.file("extdata", "roads.gpkg", package="pfif")
h = system.file("extdata", "stream.gpkg", package="pfif")
metrics = rast(f)
roads = vect(g)
stream = vect(h)

# visualize the data
plot(metrics)
plot(metrics[[1]])
plot(roads, add = T)
plot(stream, add = T)

# Preprocessing
masks = list(roads, stream)
layers = pre_processing(metrics, masks = masks)
plot(layers)

# Generic region merging
polygons = generic_region_merging(layers, otb_dir = "/home/jr/Logiciels/OTB-8.1.2-Linux64/bin")

plotRGB(layers, r = 1, g = 2, b = 4)
plot(st_geometry(polygons), border = "red", add = T)
```

