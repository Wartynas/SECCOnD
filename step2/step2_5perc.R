
library(gplots) # for heatmap2
library(ggplot2)
library(latex2exp) # for TeX
library(ggsci)

source("../../utils.R")

# Load files
setwd("C:/Users/marty/Desktop/Imperial/summer_project/hpc_stuff/step2")

gt_nc_matrix <- readRDS("attempt_2/gt_nc_matrix.rds")
gt <- apply(gt_nc_matrix, 1, function(x) {getmode(x)}, simplify = TRUE)

# -----------

step2_NMI_vanilla <- readRDS("attempt_2/step_2_NMIs_vanilla.rds")
step2_NMI_calibrated <- readRDS("attempt_2/step_2_NMIs_calibrated.rds")
step2_NMI_consensus <- readRDS("attempt_2/step_2_NMIs_consensus.rds")

step2_NMI_95th_vanilla <- readRDS("attempt_2/step_2_NMI_95th_vanilla.rds")
step2_NMI_95th_calibrated <- readRDS("attempt_2/step_2_NMI_95th_calibrated.rds")
step2_NMI_95th_consensus <- readRDS("attempt_2/step_2_NMI_95th_consensus.rds")

step2_NMI_5th_vanilla <- readRDS("attempt_2/step_2_NMI_5th_vanilla.rds")
step2_NMI_5th_calibrated <- readRDS("attempt_2/step_2_NMI_5th_calibrated.rds")
step2_NMI_5th_consensus <- readRDS("attempt_2/step_2_NMI_5th_consensus.rds")


df_nmi <- data.frame(
  step2_NMI_vanilla = step2_NMI_vanilla,
  step2_NMI_calibrated = step2_NMI_calibrated,
  step2_NMI_consensus = step2_NMI_consensus,
  step2_NMI_95th_vanilla = step2_NMI_95th_vanilla,
  step2_NMI_95th_calibrated = step2_NMI_95th_calibrated,
  step2_NMI_95th_consensus = step2_NMI_95th_consensus,
  step2_NMI_5th_vanilla = step2_NMI_5th_vanilla,
  step2_NMI_5th_calibrated = step2_NMI_5th_calibrated,
  step2_NMI_5th_consensus = step2_NMI_5th_consensus
)

colors <- c("Louvain" = "#BC3C29FF", "SECCOnD, v1" = "#0072B5FF", "SECCOnD, v2" = "#E18727FF")

ggplot(df_nmi, aes(x = seq(0.01, 0.95, 0.01))) +
  geom_line(aes(y=step2_NMI_vanilla, color = "Louvain"), size = 2) +
  geom_line(aes(y=step2_NMI_calibrated, color = "SECCOnD, v1"), size = 2) +
  geom_line(aes(y=step2_NMI_consensus, color = "SECCOnD, v2"), size = 2) +
  # adding ribbons
  geom_ribbon(aes(ymax = step2_NMI_95th_vanilla, ymin = step2_NMI_5th_vanilla), alpha = 0.3, fill = "#BC3C29FF") +
  geom_ribbon(aes(ymax = step2_NMI_95th_calibrated, ymin = step2_NMI_5th_calibrated), alpha = 0.3, fill = "#0072B5FF") +
  geom_ribbon(aes(ymax = step2_NMI_95th_consensus, ymin = step2_NMI_5th_consensus), alpha = 0.3, fill = "#E18727FF") +
  theme_minimal() +
  xlab(expression(paste("\nComplexity parameter ", mu))) +
  ylab("NMI\n") +
  theme(
    axis.title.x = element_text(size = 16, vjust = -1),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size=12),
    legend.title = element_text(size=14)
  ) +
  ylim(0, 1.01) +
  xlim(0, 1.01) +
  labs(color = "Algorithms") +
  scale_color_manual(values = colors)
  


# -----------

step2_NMI_sd_vanilla <- readRDS("attempt_2/step_2_NMI_sd_vanilla.rds")
step2_NMI_sd_calibrated <- readRDS("attempt_2/step_2_NMI_sd_calibrated.rds")
step2_NMI_sd_consensus <- readRDS("attempt_2/step_2_NMI_sd_consensus.rds")

df_sd <- data.frame(
  step2_NMI_sd_vanilla = step2_NMI_sd_vanilla,
  step2_NMI_sd_calibrated = step2_NMI_sd_calibrated,
  step2_NMI_sd_consensus = step2_NMI_sd_consensus
)

ggplot(df_sd, aes(x = seq(0.01, 0.95, 0.01))) +
  geom_line(aes(y=step2_NMI_sd_vanilla, color = "Louvain"), size = 2) +
  geom_line(aes(y=step2_NMI_sd_calibrated, color = "SECCOnD, v1"), size = 2) +
  geom_line(aes(y=step2_NMI_sd_consensus, color = "SECCOnD, v2"), size = 2) +
  theme_minimal() +
  xlab(expression(paste("\nComplexity parameter ", mu))) +
  ylab("Standard deviation\n") +
  theme(
    axis.title.x = element_text(size = 16, vjust = -1),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size=12),
    legend.title = element_text(size=14)
  ) +
  ylim(0, 0.15) +
  xlim(0, 1.01) +
  labs(color = "Algorithms") +
  scale_color_manual(values = colors)

# SD using stability approach is smaller, except for this strange spike...

# --------------

colors_nc <- c("Louvain" = "#BC3C29FF", "SECCOnD, v1" = "#0072B5FF", "SECCOnD, v2" = "#E18727FF", "Ground truth" = "#20854EFF")

step2_nc_vanilla <- readRDS("step_2_nc_vanilla.rds")
step2_nc_calibrated <- readRDS("step_2_nc_calibrated.rds")
step2_nc_consensus <- readRDS("step_2_nc_consensus.rds")

df_nc <- data.frame(
  step2_nc_vanilla = step2_nc_vanilla,
  step2_nc_calibrated = step2_nc_calibrated,
  step2_nc_consensus = step2_nc_consensus,
  gt = gt
)


ggplot(df_nc, aes(x = seq(0.01, 0.95, 0.01))) +
  geom_line(aes(y=step2_nc_vanilla, color = "Louvain"), size = 2) +
  geom_line(aes(y=step2_nc_calibrated, color = "SECCOnD, v1"), size = 2) +
  geom_line(aes(y=step2_nc_consensus, color = "SECCOnD, v2"), size = 2) +
  geom_line(aes(y=gt, color = "Ground truth"), size = 2) +
  theme_minimal() +
  xlab(expression(paste("\nComplexity parameter ", mu))) +
  ylab("Number of communities\n") +
  theme(
    axis.title.x = element_text(size = 16, vjust = -1),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size=12),
    legend.title = element_text(size=14)
  ) +
  ylim(0, 230) +
  xlim(0, 1.01) +
  labs(color = "Algorithms") +
  scale_color_manual(values = colors_nc)

# --------------

# Analysis

## Draw heatmaps for every matrix

heatmap.2(NMI_hmap_vanilla_10perc, dendrogram = "none", Rowv = NA, Colv = NA, na.color = "grey", trace= "none")
heatmap.2(NMI_hmap_calibrated_10perc, dendrogram = "none", Rowv = NA, Colv = NA, na.color = "grey", trace= "none")
heatmap.2(NMI_hmap_consensus_10perc, dendrogram = "none", Rowv = NA, Colv = NA, na.color = "grey", trace= "none")

heatmap.2(step2_NMI_sd_vanilla, dendrogram = "none", Rowv = NA, Colv = NA, na.color = "grey", trace= "none")
heatmap.2(step2_NMI_sd_calibrated, dendrogram = "none", Rowv = NA, Colv = NA, na.color = "grey", trace= "none")
heatmap.2(NMI_sd_hmap_consensus_10perc, dendrogram = "none", Rowv = NA, Colv = NA, na.color = "grey", trace= "none")

heatmap.2(nc_hmap_vanilla_10perc, dendrogram = "none", Rowv = NA, Colv = NA, na.color = "grey", trace= "none")
heatmap.2(nc_hmap_calibrated_10perc, dendrogram = "none", Rowv = NA, Colv = NA, na.color = "grey", trace= "none")
heatmap.2(nc_hmap_consensus_10perc, dendrogram = "none", Rowv = NA, Colv = NA, na.color = "grey", trace= "none")

dif <- NMI_hmap_consensus_10perc - NMI_hmap_vanilla_10perc
dif_no_nas <- NMI_hmap_consensus_10perc - NMI_hmap_vanilla_10perc
dif_no_nas[is.na(dif_no_nas)] <- 0

heatmap.2(dif_no_nas, dendrogram = "none",  Rowv = NA, Colv = NA, na.color = "grey", trace= "none")

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

