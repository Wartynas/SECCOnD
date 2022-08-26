library(tidyverse)
library(gplots) # for heatmap2
library(NMI)
library(lattice) # for levelplot
library(latex2exp) # for TeX

setwd("C:/Users/marty/Desktop/Imperial/summer_project/hpc_stuff/step1_1percent_unrestricted/3D")

source("../../../utils.R")

# Load main files

NMI_hmap_vanilla_1perc <- readRDS("step_1_1percent_unrestricted_3D_output/intermediate_NMI_hmap_vanilla_1perc.rds")
NMI_hmap_calibrated_1perc <- readRDS("step_1_1percent_unrestricted_3D_output/intermediate_NMI_hmap_calibrated_1perc.rds")
NMI_hmap_consensus_1perc <- readRDS("step_1_1percent_unrestricted_3D_output/intermediate_NMI_hmap_consensus_1perc.rds")

# Can calculate SD myself from the NMI cubes
# NMI_sd_hmap_vanilla_1perc <- readRDS("step_1_1percent_unrestricted_3D_output/intermediate_NMI_sd_hmap_vanilla_1perc.rds")
# NMI_sd_hmap_calibrated_1perc <- readRDS("step_1_1percent_unrestricted_3D_output/intermediate_NMI_sd_hmap_calibrated_1perc.rds")
# NMI_sd_hmap_consensus_1perc <- readRDS("step_1_1percent_unrestricted_3D_output/intermediate_NMI_sd_hmap_consensus_1perc.rds")

nc_hmap_vanilla_1perc <- readRDS("step_1_1percent_unrestricted_3D_output/intermediate_nc_hmap_vanilla_1perc.rds")
nc_hmap_calibrated_1perc <- readRDS("step_1_1percent_unrestricted_3D_output/intermediate_nc_hmap_calibrated_1perc.rds")
nc_hmap_consensus_1perc <- readRDS("step_1_1percent_unrestricted_3D_output/intermediate_nc_hmap_consensus_1perc.rds")

# Load rest of the files (nu_within from 0.94)

rest_NMI_hmap_vanilla_1perc <- readRDS("rest/intermediate_NMI_hmap_vanilla_1perc.rds")
rest_NMI_hmap_calibrated_1perc <- readRDS("rest/intermediate_NMI_hmap_calibrated_1perc.rds")
rest_NMI_hmap_consensus_1perc <- readRDS("rest/intermediate_NMI_hmap_consensus_1perc.rds")

# Can calculate SD myself from the NMI cubes
# NMI_sd_hmap_vanilla_1perc <- readRDS("step_1_1percent_unrestricted_3D_output/intermediate_NMI_sd_hmap_vanilla_1perc.rds")
# NMI_sd_hmap_calibrated_1perc <- readRDS("step_1_1percent_unrestricted_3D_output/intermediate_NMI_sd_hmap_calibrated_1perc.rds")
# NMI_sd_hmap_consensus_1perc <- readRDS("step_1_1percent_unrestricted_3D_output/intermediate_NMI_sd_hmap_consensus_1perc.rds")

rest_nc_hmap_vanilla_1perc <- readRDS("rest/intermediate_nc_hmap_vanilla_1perc.rds")
rest_nc_hmap_calibrated_1perc <- readRDS("rest/intermediate_nc_hmap_calibrated_1perc.rds")
rest_nc_hmap_consensus_1perc <- readRDS("rest/intermediate_nc_hmap_consensus_1perc.rds")

# Merging the files

NMI_hmap_vanilla_1perc[93:nrow(NMI_hmap_vanilla_1perc),,] <- rest_NMI_hmap_vanilla_1perc 
NMI_hmap_calibrated_1perc[93:nrow(NMI_hmap_calibrated_1perc),,] <- rest_NMI_hmap_calibrated_1perc
NMI_hmap_consensus_1perc[93:nrow(NMI_hmap_consensus_1perc),,] <- rest_NMI_hmap_consensus_1perc

nc_hmap_vanilla_1perc[93:nrow(nc_hmap_vanilla_1perc),,] <- rest_nc_hmap_vanilla_1perc
nc_hmap_calibrated_1perc[93:nrow(nc_hmap_calibrated_1perc),,] <- rest_nc_hmap_calibrated_1perc
nc_hmap_consensus_1perc[93:nrow(nc_hmap_consensus_1perc),,] <- rest_nc_hmap_consensus_1perc


# All should have 1 NA in position 99,99. Need to replace impute it with adjacent value

nc_hmap_vanilla_1perc[99,99,] <- nc_hmap_vanilla_1perc[99,98,]
nc_hmap_calibrated_1perc[99,99,] <- nc_hmap_calibrated_1perc[99,98,]
nc_hmap_consensus_1perc[99,99,] <- nc_hmap_consensus_1perc[99,98,]

NMI_hmap_vanilla_1perc[99,99,] <- NMI_hmap_vanilla_1perc[99,98,]
NMI_hmap_calibrated_1perc[99,99,] <- NMI_hmap_calibrated_1perc[99,98,]
NMI_hmap_consensus_1perc[99,99,] <- NMI_hmap_consensus_1perc[99,98,]

# NMI_sd_hmap_vanilla_1perc[99,99] <- NMI_sd_hmap_vanilla_1perc[99,98]
# NMI_sd_hmap_calibrated_1perc[99,99] <- NMI_sd_hmap_calibrated_1perc[99,98]
# NMI_sd_hmap_consensus_1perc[99,99] <- NMI_sd_hmap_consensus_1perc[99,98]

# Analysis


## Get means and SDs

mean_NMI_2D_vanilla <- apply(NMI_hmap_vanilla_1perc, MARGIN = c(1,2), FUN = mean) %>% round(digits = 3)
mean(mean_NMI_2D_vanilla)
mean_NMI_2D_cal <- apply(NMI_hmap_calibrated_1perc, MARGIN = c(1,2), FUN = mean) %>% round(digits = 3)
mean(mean_NMI_2D_cal)
mean_NMI_2D_con <- apply(NMI_hmap_consensus_1perc, MARGIN = c(1,2), FUN = mean) %>% round(digits = 3)
mean(mean_NMI_2D_con)

sd_NMI_2D_vanilla <- apply(NMI_hmap_vanilla_1perc, MARGIN = c(1,2), FUN = sd) %>% round(digits = 3)
sd_NMI_2D_cal <- apply(NMI_hmap_calibrated_1perc, MARGIN = c(1,2), FUN = sd) %>% round(digits = 3)
sd_NMI_2D_con <- apply(NMI_hmap_consensus_1perc, MARGIN = c(1,2), FUN = sd) %>% round(digits = 3)

mode_nc_2D_vanilla <- apply(nc_hmap_vanilla_1perc, MARGIN = c(1,2), FUN = getmode)
mode_nc_2D_cal <- apply(nc_hmap_calibrated_1perc, MARGIN = c(1,2), FUN = getmode)
mode_nc_2D_con <- apply(nc_hmap_consensus_1perc, MARGIN = c(1,2), FUN = getmode)


## Give rownames and colnames

rownames(mean_NMI_2D_vanilla) <- seq(0.02, 1, 0.01)
colnames(mean_NMI_2D_vanilla) <- seq(0.02, 1, 0.01)

rownames(mean_NMI_2D_cal) <- seq(0.02, 1, 0.01)
colnames(mean_NMI_2D_cal) <- seq(0.02, 1, 0.01)

rownames(mean_NMI_2D_con) <- seq(0.02, 1, 0.01)
colnames(mean_NMI_2D_con) <- seq(0.02, 1, 0.01)




rownames(sd_NMI_2D_vanilla) <- seq(0.02, 1, 0.01)
colnames(sd_NMI_2D_vanilla) <- seq(0.02, 1, 0.01)

rownames(sd_NMI_2D_cal) <- seq(0.02, 1, 0.01)
colnames(sd_NMI_2D_cal) <- seq(0.02, 1, 0.01)

rownames(sd_NMI_2D_con) <- seq(0.02, 1, 0.01)
colnames(sd_NMI_2D_con) <- seq(0.02, 1, 0.01)



rownames(mode_nc_2D_vanilla) <- seq(0.02, 1, 0.01)
colnames(mode_nc_2D_vanilla) <- seq(0.02, 1, 0.01)

rownames(mode_nc_2D_cal) <- seq(0.02, 1, 0.01)
colnames(mode_nc_2D_cal) <- seq(0.02, 1, 0.01)

rownames(mode_nc_2D_con) <- seq(0.02, 1, 0.01)
colnames(mode_nc_2D_con) <- seq(0.02, 1, 0.01)


## Draw heatmaps for every matrix

# heatmap.2
heatmap.2(mean_NMI_2D_vanilla, dendrogram = "none", Rowv = NA, Colv = NA, na.color = "grey", trace= "none")
heatmap.2(mean_NMI_2D_cal, dendrogram = "none", Rowv = NA, Colv = NA, na.color = "grey", trace= "none")
heatmap.2(mean_NMI_2D_con, dendrogram = "none", Rowv = NA, Colv = NA, na.color = "grey", trace= "none")

heatmap.2(sd_NMI_2D_vanilla, dendrogram = "none", Rowv = NA, Colv = NA, na.color = "grey", trace= "none")
heatmap.2(sd_NMI_2D_cal, dendrogram = "none", Rowv = NA, Colv = NA, na.color = "grey", trace= "none")
heatmap.2(sd_NMI_2D_con, dendrogram = "none", Rowv = NA, Colv = NA, na.color = "grey", trace= "none")

heatmap.2(mode_nc_2D_vanilla, dendrogram = "none", Rowv = NA, Colv = NA, na.color = "grey", trace= "none")
heatmap.2(mode_nc_2D_cal, dendrogram = "none", Rowv = NA, Colv = NA, na.color = "grey", trace= "none")
heatmap.2(mode_nc_2D_con, dendrogram = "none", Rowv = NA, Colv = NA, na.color = "grey", trace= "none")

# levelplot

# https://stackoverflow.com/questions/17584248/exact-axis-ticks-and-labels-in-r-lattice-xyplot
# https://stackoverflow.com/questions/30663252/change-font-in-colorkey-legend-in-r-levelplot
# https://stackoverflow.com/questions/21641186/r-levelplot-colorkey-label-and-color-range - very good resource about colorkey. How to scale delta NMI

my_levelplot <- function(matrix_2d, num_of_colors, legend_title, binary = FALSE) {
  if (binary) {
    levelplot(
      t(matrix_2d[c(nrow(matrix_2d):1) , ]), # need to transpose in order to have nu_within as rows and nu_between as columns
      at = c(0,0.5,1),
      col.regions = heat.colors(2), # smallest number of colors that does not fuck things up
      # xlab = TeX("$nu_{between}$"),
      # ylab = TeX("$nu_{within}$"),
      scales=list(
        y = list(at=seq(99,1,-5), labels= seq(0.02, 1, 0.05), cex=1.5),
        x = list(at=seq(99,1,-5), labels= seq(1, 0.02, -0.05) %>% round(2), rot = 90, cex=1.5)
      ),
      xlab = list(label = TeX("$nu_{between}$"), cex=2),
      ylab = list(label = TeX("$nu_{within}$"), cex=2),
      main = list(TeX("$p=1\\%$", italic = TRUE), cex = 3),
      colorkey=list(
        height=0.1,
        at = seq(0, 1, 0.5),
        labels = list(
          # labels = c(expression(paste(Delta, "NMI < 0")), expression(paste(Delta, "NMI > 0"))),
          labels = c("Worse", "Equal or better"),
          at = c(0.25, 0.75),
          cex = 2
        ),
        cex = 2,
        title= legend_title
      )
    )
  } else {
    
    levelplot(
      t(matrix_2d[c(nrow(matrix_2d):1) , ]), # need to transpose in order to have nu_within as rows and nu_between as columns
      col.regions = heat.colors(num_of_colors), # smallest number of colors that does not fuck things up
      # xlab = TeX("$nu_{between}$"),
      # ylab = TeX("$nu_{within}$"),
      scales=list(
        y = list(at=seq(99,1,-5), labels= seq(0.02, 1, 0.05), cex=2),
        x = list(at=seq(99,1,-5), labels= seq(1, 0.02, -0.05) %>% round(2), rot = 90, cex=2)
      ),
      xlab = list(label = TeX("$nu_{between}$"), cex=3),
      ylab = list(label = TeX("$nu_{within}$"), cex=3),
      main = list(TeX("$p=1\\%$", italic = TRUE), cex = 3),
      colorkey=list(
        title= list(legend_title, cex = 2),
        labels = list(cex = 2)
      )
    )
  }
}

# NMIs
my_levelplot(mean_NMI_2D_vanilla, 16, "NMI")
my_levelplot(mean_NMI_2D_cal, 16, "NMI")
my_levelplot(mean_NMI_2D_con, 16, "NMI")

# SDs

my_levelplot(sd_NMI_2D_vanilla, 16, "SD")
my_levelplot(sd_NMI_2D_cal, 16, "SD")
my_levelplot(sd_NMI_2D_con, 16, "SD")

# nc
my_levelplot(mode_nc_2D_vanilla, 16, "Number of communities")
my_levelplot(mode_nc_2D_vanilla[20:nrow(mode_nc_2D_vanilla),], 16, "Number of communities")
my_levelplot(mode_nc_2D_cal, 16, "Number of communities")
my_levelplot(mode_nc_2D_cal[20:nrow(mode_nc_2D_cal), ], 16, "Number of communities")
my_levelplot(mode_nc_2D_con, 16, "Number of communities")
my_levelplot(mode_nc_2D_con[20:nrow(mode_nc_2D_con), ], 16, "Number of communities")


# Comparing NMIs

## Vanilla vs cal
mask_vanilla_cal <- ifelse(mean_NMI_2D_vanilla < mean_NMI_2D_cal, 1,0)
my_levelplot(mask_vanilla_cal, 16, " ", TRUE)

vanilla_cal_pvalues <- matrix(1, 99,99)
for (i in 1:99) {
  for (j in 1:99) {
    if (mean(NMI_hmap_vanilla_1perc[i,j,]) != mean(NMI_hmap_calibrated_1perc[i,j,])) {
      vanilla_cal_pvalues[i,j] <- t.test(NMI_hmap_vanilla_1perc[i,j,], NMI_hmap_calibrated_1perc[i,j,])$p.value
    }
  }
}

vanilla_cal_pvalues <- ifelse(vanilla_cal_pvalues > 0.05, 1, 0)
my_levelplot(ifelse(vanilla_cal_pvalues | mask_vanilla_cal, 1,0), 16, " ", TRUE)

## Vanilla vs con

mask_vanilla_con <- ifelse(mean_NMI_2D_vanilla < mean_NMI_2D_con, 1,0)
my_levelplot(mask_vanilla_con, 16, " ", TRUE)

vanilla_con_pvalues <- matrix(1, 99,99)
for (i in 1:99) {
  for (j in 1:99) {
    if (mean(NMI_hmap_vanilla_1perc[i,j,]) != mean(NMI_hmap_consensus_1perc[i,j,])) {
      vanilla_con_pvalues[i,j] <- t.test(NMI_hmap_vanilla_1perc[i,j,], NMI_hmap_consensus_1perc[i,j,])$p.value
    }
  }
}

vanilla_con_pvalues <- ifelse(vanilla_con_pvalues > 0.05, 1, 0)
my_levelplot(ifelse(vanilla_con_pvalues | mask_vanilla_con, 1,0), 16, " ", TRUE)


#dif <- NMI_hmap_consensus_1perc - NMI_hmap_vanilla_1perc
dif_con_van <- NMI_hmap_consensus_1perc - NMI_hmap_vanilla_1perc
#dif_no_nas[is.na(dif_no_nas)] <- 0
heatmap.2(dif_con_van, dendrogram = "none",  Rowv = NA, Colv = NA, na.color = "grey", trace= "none")

dif_cal_van <- NMI_hmap_calibrated_1perc - NMI_hmap_vanilla_1perc
heatmap.2(dif_cal_van, dendrogram = "none",  Rowv = NA, Colv = NA, na.color = "grey", trace= "none")


# For table
mean(NMI_hmap_vanilla_1perc)
mean(NMI_hmap_calibrated_1perc)
mean(NMI_hmap_consensus_1perc)

mean(NMI_sd_hmap_vanilla_1perc)
mean(NMI_sd_hmap_calibrated_1perc)
mean(NMI_sd_hmap_consensus_1perc)

sum(nc_hmap_vanilla_1perc == 4)/ (nrow(nc_hmap_vanilla_1perc) * ncol(nc_hmap_vanilla_1perc))*100
sum(nc_hmap_calibrated_1perc == 4)/ (nrow(nc_hmap_calibrated_1perc) * ncol(nc_hmap_calibrated_1perc))*100
sum(nc_hmap_consensus_1perc == 4)/ (nrow(nc_hmap_consensus_1perc) * ncol(nc_hmap_consensus_1perc))*100


## Limit where identified number of clusters is 4 by calibrated

# nc_hmap_consensus_1perc[is.na(nc_hmap_consensus_1perc)] <- 0

dif_con_van <- ifelse(
  nc_hmap_vanilla_1perc == 4,
  dif_con_van,
  0
)

heatmap.2(dif_con_van, dendrogram = "none",  Rowv = NA, Colv = NA, na.color = "grey", trace= "none")


dif_cal_van <- ifelse(
  nc_hmap_vanilla_1perc == 4,
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



