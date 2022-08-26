library(tidyverse)
library(gplots) # for heatmap2
library(NMI)

source("../../utils.R")

# Load files
setwd("C:/Users/marty/Desktop/Imperial/summer_project/hpc_stuff/step1_20percent_unrestricted")

NMI_hmap_vanilla_20perc <- readRDS("intermediate_NMI_hmap_vanilla_20perc.rds")
NMI_hmap_calibrated_20perc <- readRDS("intermediate_NMI_hmap_calibrated_20perc.rds")
NMI_hmap_consensus_20perc <- readRDS("intermediate_NMI_hmap_consensus_20perc.rds")

NMI_sd_hmap_vanilla_20perc <- readRDS("intermediate_NMI_sd_hmap_vanilla_20perc.rds")
NMI_sd_hmap_calibrated_20perc <- readRDS("intermediate_NMI_sd_hmap_calibrated_20perc.rds")
NMI_sd_hmap_consensus_20perc <- readRDS("intermediate_NMI_sd_hmap_consensus_20perc.rds")

nc_hmap_vanilla_20perc <- readRDS("intermediate_nc_hmap_vanilla_20perc.rds")
nc_hmap_calibrated_20perc <- readRDS("intermediate_nc_hmap_calibrated_20perc.rds")
nc_hmap_consensus_20perc <- readRDS("intermediate_nc_hmap_consensus_20perc.rds")

# All should have 1 NA in position 99,99. Need to replace impute it with adjacent value

nc_hmap_vanilla_20perc[99,99] <- nc_hmap_vanilla_20perc[99,98]
nc_hmap_calibrated_20perc[99,99] <- nc_hmap_calibrated_20perc[99,98]
nc_hmap_consensus_20perc[99,99] <- nc_hmap_consensus_20perc[99,98]

NMI_hmap_vanilla_20perc[99,99] <- NMI_hmap_vanilla_20perc[99,98]
NMI_hmap_calibrated_20perc[99,99] <- NMI_hmap_calibrated_20perc[99,98]
NMI_hmap_consensus_20perc[99,99] <- NMI_hmap_consensus_20perc[99,98]

NMI_sd_hmap_vanilla_20perc[99,99] <- NMI_sd_hmap_vanilla_20perc[99,98]
NMI_sd_hmap_calibrated_20perc[99,99] <- NMI_sd_hmap_calibrated_20perc[99,98]
NMI_sd_hmap_consensus_20perc[99,99] <- NMI_sd_hmap_consensus_20perc[99,98]

# Analysis

## Draw heatmaps for every matrix

heatmap.2(NMI_hmap_vanilla_20perc, dendrogram = "none", Rowv = NA, Colv = NA, na.color = "grey", trace= "none")
heatmap.2(NMI_hmap_calibrated_20perc, dendrogram = "none", Rowv = NA, Colv = NA, na.color = "grey", trace= "none")
heatmap.2(NMI_hmap_consensus_20perc, dendrogram = "none", Rowv = NA, Colv = NA, na.color = "grey", trace= "none")

heatmap.2(NMI_sd_hmap_vanilla_20perc, dendrogram = "none", Rowv = NA, Colv = NA, na.color = "grey", trace= "none")
heatmap.2(NMI_sd_hmap_calibrated_20perc, dendrogram = "none", Rowv = NA, Colv = NA, na.color = "grey", trace= "none")
heatmap.2(NMI_sd_hmap_consensus_20perc, dendrogram = "none", Rowv = NA, Colv = NA, na.color = "grey", trace= "none")

heatmap.2(nc_hmap_vanilla_20perc, dendrogram = "none", Rowv = NA, Colv = NA, na.color = "grey", trace= "none")
heatmap.2(nc_hmap_calibrated_20perc, dendrogram = "none", Rowv = NA, Colv = NA, na.color = "grey", trace= "none")
heatmap.2(nc_hmap_consensus_20perc, dendrogram = "none", Rowv = NA, Colv = NA, na.color = "grey", trace= "none")

#dif <- NMI_hmap_consensus_20perc - NMI_hmap_vanilla_20perc
dif_con_van <- NMI_hmap_consensus_20perc - NMI_hmap_vanilla_20perc
#dif_no_nas[is.na(dif_no_nas)] <- 0
heatmap.2(dif_con_van, dendrogram = "none",  Rowv = NA, Colv = NA, na.color = "grey", trace= "none")

dif_cal_van <- NMI_hmap_calibrated_20perc - NMI_hmap_vanilla_20perc
heatmap.2(dif_cal_van, dendrogram = "none",  Rowv = NA, Colv = NA, na.color = "grey", trace= "none")


# For table
mean(NMI_hmap_vanilla_20perc)
mean(NMI_hmap_calibrated_20perc)
mean(NMI_hmap_consensus_20perc)

mean(NMI_sd_hmap_vanilla_20perc)
mean(NMI_sd_hmap_calibrated_20perc)
mean(NMI_sd_hmap_consensus_20perc)

sum(nc_hmap_vanilla_20perc == 4)/ (nrow(nc_hmap_vanilla_20perc) * ncol(nc_hmap_vanilla_20perc))*100
sum(nc_hmap_calibrated_20perc == 4)/ (nrow(nc_hmap_calibrated_20perc) * ncol(nc_hmap_calibrated_20perc))*100
sum(nc_hmap_consensus_20perc == 4)/ (nrow(nc_hmap_consensus_20perc) * ncol(nc_hmap_consensus_20perc))*100


## Limit where identified number of clusters is 4 by calibrated

# nc_hmap_consensus_20perc[is.na(nc_hmap_consensus_20perc)] <- 0

dif_con_van <- ifelse(
  nc_hmap_vanilla_20perc == 4,
  dif_con_van,
  0
)

heatmap.2(dif_con_van, dendrogram = "none",  Rowv = NA, Colv = NA, na.color = "grey", trace= "none")


dif_cal_van <- ifelse(
  nc_hmap_vanilla_20perc == 4,
  dif_cal_van,
  0
)

heatmap.2(dif_cal_van, dendrogram = "none",  Rowv = NA, Colv = NA, na.color = "grey", trace= "none")

## Exploring diff matrix


first_best_combo <- which(dif_con_van == max(dif_con_van), arr.ind = TRUE)
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



