library(pfif)

root = "/media/jr/Alexandre/RMF_EFI_layers/"
out_dir <- '/home/jr/Documents/Ulaval/Projets Annexes/Alexandre - ontario'
lc_f <- "/home/jr/Téléchargements/CA_forest_VLCE2_2018/CA_forest_VLCE2_2018.tif"
otb_dir <- "/home/jr/Logiciels/OTB-8.1.2-Linux64/bin"

set_pfif_globals(root, out_dir, lc_f, otb_dir)

files = pre_processing()

grm = generic_region_merging()

grm2 = post_processing(grm)

stats = performance(grm2)

###########################################
### Summary Stats and Additional Tables ###
###########################################

library(readr)


df = read_csv(stats)

# Size
t1 <- df[, 1:6]

# change to ha
t1[, 2:5] <- round(t1[, 2:5] / 25, 2)

names(t1) <- c('Dataset', "Min Ha", 'Max Ha', "Mean Ha", "Median Ha", "Number of Polygons")
knitr::kable(t1, caption = "Table 1: Polygon Size Stats", label = NA)

# Area and Perim
t2 <- df[, c(1, 7:12)]

# change area to Ha
t2[,2:4] <- round(t2[,2:4] / 10000, 2)

names(t2) <- c("Dataset", "Mean Area (Ha)", "SE Area", "SD Area", "Mean Perimeter (m)", "SE Perimeter", "SD Perimeter")
knitr::kable(t2, caption = "Table 2: Polygon Area and Perimeter Stats", label = NA)

# Mean Shape Index
t3 <- df[, c(1, 16:18)]
names(t3) <- c("Dataset", "Mean Shape Index", "SE Shape Index", "SD Shape Index")
knitr::kable(t3, caption = "Table 3: Polygon Shape Index Stats", label = NA)

# R2
t4 <- df[,c(1, 19:22)]

# round
t4[,2:5] <- round(t4[,2:5], 2)

names(t4) <- c('Dataset', 'R2 P95', 'R2 Can Cov', 'R2 Coeff Var', 'R2 Overall')
knitr::kable(t4, caption = "Table 4: Polygon R2 Stats", label = NA)
