
library(gplots) # for heatmap2

source("../../utils.R")

# Load files
setwd("C:/Users/marty/Desktop/Imperial/summer_project/hpc_stuff/step1_10percent_unrestricted")

NMI_hmap_vanilla_10perc <- readRDS("intermediate_NMI_hmap_vanilla_10perc.rds")
NMI_hmap_calibrated_10perc <- readRDS("intermediate_NMI_hmap_calibrated_10perc.rds")
NMI_hmap_consensus_10perc <- readRDS("intermediate_NMI_hmap_consensus_10perc.rds")

NMI_sd_hmap_vanilla_10perc <- readRDS("intermediate_NMI_sd_hmap_vanilla_10perc.rds")
NMI_sd_hmap_calibrated_10perc <- readRDS("intermediate_NMI_sd_hmap_calibrated_10perc.rds")
NMI_sd_hmap_consensus_10perc <- readRDS("intermediate_NMI_sd_hmap_consensus_10perc.rds")

nc_hmap_vanilla_10perc <- readRDS("intermediate_nc_hmap_vanilla_10perc.rds")
nc_hmap_calibrated_10perc <- readRDS("intermediate_nc_hmap_calibrated_10perc.rds")
nc_hmap_consensus_10perc <- readRDS("intermediate_nc_hmap_consensus_10perc.rds")

# Imputing last value

NMI_hmap_vanilla_10perc[99,99] <- NMI_hmap_vanilla_10perc[99,98]
NMI_hmap_calibrated_10perc[99,99] <- NMI_hmap_calibrated_10perc[99,98]
NMI_hmap_consensus_10perc[99,99] <- NMI_hmap_consensus_10perc[99,98]

NMI_sd_hmap_vanilla_10perc[99,99] <- NMI_sd_hmap_vanilla_10perc[99,98]
NMI_sd_hmap_calibrated_10perc[99,99] <- NMI_sd_hmap_calibrated_10perc[99,98]
NMI_sd_hmap_consensus_10perc[99,99] <- NMI_sd_hmap_consensus_10perc[99,98]

nc_hmap_vanilla_10perc[99,99] <- nc_hmap_vanilla_10perc[99,98]
nc_hmap_calibrated_10perc[99,99] <- nc_hmap_calibrated_10perc[99,98]
nc_hmap_consensus_10perc[99,99] <- nc_hmap_consensus_10perc[99,98]


# Analysis

## Draw heatmaps for every matrix

heatmap.2(NMI_hmap_vanilla_10perc, dendrogram = "none", Rowv = NA, Colv = NA, na.color = "grey", trace= "none")
heatmap.2(NMI_hmap_calibrated_10perc, dendrogram = "none", Rowv = NA, Colv = NA, na.color = "grey", trace= "none")
heatmap.2(NMI_hmap_consensus_10perc, dendrogram = "none", Rowv = NA, Colv = NA, na.color = "grey", trace= "none")

heatmap.2(NMI_sd_hmap_vanilla_10perc, dendrogram = "none", Rowv = NA, Colv = NA, na.color = "grey", trace= "none")
heatmap.2(NMI_sd_hmap_calibrated_10perc, dendrogram = "none", Rowv = NA, Colv = NA, na.color = "grey", trace= "none")
heatmap.2(NMI_sd_hmap_consensus_10perc, dendrogram = "none", Rowv = NA, Colv = NA, na.color = "grey", trace= "none")

heatmap.2(nc_hmap_vanilla_10perc, dendrogram = "none", Rowv = NA, Colv = NA, na.color = "grey", trace= "none")
heatmap.2(nc_hmap_calibrated_10perc, dendrogram = "none", Rowv = NA, Colv = NA, na.color = "grey", trace= "none")
heatmap.2(nc_hmap_consensus_10perc, dendrogram = "none", Rowv = NA, Colv = NA, na.color = "grey", trace= "none")

dif <- NMI_hmap_consensus_10perc - NMI_hmap_vanilla_10perc
dif_no_nas <- NMI_hmap_consensus_10perc - NMI_hmap_vanilla_10perc
dif_no_nas[is.na(dif_no_nas)] <- 0

heatmap.2(dif_no_nas, dendrogram = "none",  Rowv = NA, Colv = NA, na.color = "grey", trace= "none")


mean(NMI_hmap_vanilla_10perc)
mean(NMI_hmap_calibrated_10perc)
mean(NMI_hmap_consensus_10perc)

mean(NMI_sd_hmap_vanilla_10perc)
mean(NMI_sd_hmap_calibrated_10perc)
mean(NMI_sd_hmap_consensus_10perc)

sum(nc_hmap_vanilla_10perc == 4)/ (nrow(nc_hmap_vanilla_10perc) * ncol(nc_hmap_vanilla_10perc))*100
sum(nc_hmap_calibrated_10perc == 4)/ (nrow(nc_hmap_calibrated_10perc) * ncol(nc_hmap_calibrated_10perc))*100
sum(nc_hmap_consensus_10perc == 4)/ (nrow(nc_hmap_consensus_10perc) * ncol(nc_hmap_consensus_10perc))*100

## Limit where identified number of clusters is 4 by calibrated

nc_hmap_consensus_10perc[is.na(nc_hmap_consensus_10perc)] <- 0

dif_no_nas <- ifelse(
  nc_hmap_consensus_10perc == 4,
  dif_no_nas,
  0
)

heatmap.2(dif_no_nas, dendrogram = "none",  Rowv = NA, Colv = NA, na.color = "grey", trace= "none")

## Exploring diff matrix


first_best_combo <- which(dif_no_nas == max(dif_no_nas), arr.ind = TRUE)
first_best_diff <- dif_no_nas[first_best_combo]
dif_no_nas[first_best_combo] <- 0

second_best_combo <- which(dif_no_nas == max(dif_no_nas), arr.ind = TRUE)
second_best_diff <- dif_no_nas[second_best_combo]
dif_no_nas[second_best_combo] <- 0

third_best_combo <- which(dif_no_nas == max(dif_no_nas), arr.ind = TRUE)
third_best_diff <- dif_no_nas[third_best_combo]
dif_no_nas[third_best_combo] <- 0

fourth_best_combo <- which(dif_no_nas == max(dif_no_nas), arr.ind = TRUE)
fourth_best_diff <- dif_no_nas[fourth_best_combo]
dif_no_nas[fourth_best_combo] <- 0

dif_no_nas[79,49]

