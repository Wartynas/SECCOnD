library(focus)
library(igraph)
library(NMI)
library(pracma) # for linspace

# for parallelization
library("foreach")
library("doParallel")
library("ranger")
library("tidyverse")
library("doRNG")

# setwd("C:/Users/marty/Desktop/Imperial/summer_project")
setwd("/rds/general/user/mv321/home/thesis/")
source("utils.R")


SIMULATED_GRAPHS <- '/rds/general/user/mv321/home/thesis/simulated_benchmark_graphs/'
# SIMULATED_GRAPHS <- 'C:/Users/marty/Desktop/Imperial/summer_project/simulated_benchmark_graphs/'


simplify_edge_list <- function(edge_list) {
  edge_list <- apply(edge_list, 1, sort) %>% t() %>% as.data.frame() %>% distinct()
  return(edge_list)
}

## setting up parallelization

n_cores <- parallel::detectCores() - 1
print(paste0("The number of cores is ", n_cores))

my_cluster <- parallel::makeCluster( #create the cluster
  100,
  # setup_strategy = "sequential", # supposedly is a solution for: Cluster setup failed. 255 of 255 workers failed to connect
  type = "FORK", # change to PSOCK on windows
  outfile = "debug_step2.txt"
)
print(my_cluster)

doParallel::registerDoParallel(cl = my_cluster) #register it to be used by %dopar% or %dorng%


print(paste0("Is cluster registered? ", foreach::getDoParRegistered()))
print(paste0("How many workers available? ", foreach::getDoParWorkers()))


## 07.30 step 2 - testing algorithm on benchmark graphs (5 percent of edges to rewire)

# BENCHMARK_DIR <- "C:/Users/marty/Desktop/Imperial/summer_project/binary_networks/"
BENCHMARK_DIR <- "/rds/general/user/mv321/home/thesis/binary_networks/"
OUTPUTS_DIR <- paste0(BENCHMARK_DIR, "outputs/")
OUTPUT_NAMES <- c("community.dat", "network.dat", "statistics.dat")
NUM_OF_PERTURBATIONS <- 100 # at a fixed resolution
NUM_OF_NODES <- 1000
REALISATIONS_FOR_EVERY_MU <- 100
MAX_NC <- NUM_OF_NODES/2
PERCENT_OF_EDGES_TO_REWIRE <- 5
MU_grid <- seq(0.01, 0.95, 0.01)

gt_nc_matrix <- matrix(0, length(MU_grid), REALISATIONS_FOR_EVERY_MU)

step_2_NMIs_vanilla <- numeric()
step_2_NMIs_calibrated <- numeric()
step_2_NMIs_consensus <- numeric()

step_2_NMI_sd_vanilla <- numeric()
step_2_NMI_sd_calibrated <- numeric()
step_2_NMI_sd_consensus <- numeric()

step_2_NMI_5th_vanilla <- numeric()
step_2_NMI_5th_calibrated <- numeric()
step_2_NMI_5th_consensus <- numeric()

step_2_NMI_95th_vanilla <- numeric()
step_2_NMI_95th_calibrated <- numeric()
step_2_NMI_95th_consensus <- numeric()

step_2_nc_vanilla <- integer()
step_2_nc_calibrated <- integer()
step_2_nc_consensus <- integer()

step_2_nc_sd_vanilla <- numeric()
step_2_nc_sd_calibrated <- numeric()
step_2_nc_sd_consensus <- numeric()

# quantiles don't have to be symmetric, they are less affected by outliers.
# Show with a plot that Louvain stops increasing at some point about nc. It's a big limitation of Louvain.
# In the first step, where the nc = 4, sometimes heatmap
# Identify 3 regions of heatmap and show examples of networks from each side. Also think about putting some networks in the supplementary part.

# Create a directory for the today's date if it doesn't exist yet
date_dir <- paste0(OUTPUTS_DIR, Sys.Date())
if(!dir.exists(date_dir)) {
  dir.create(date_dir)
}


for (mu_idx in 1:length(MU_grid)){
  mu = MU_grid[mu_idx]
  print(paste0("mu parameter = ", mu))
  # set.seed(42)
  # NMI_vanilla_for_given_mu <- numeric()
  # NMI_calibrated_for_given_mu <- numeric()
  # NMI_hclust_for_given_mu <- numeric()
  # 
  # nc_vanilla_for_given_mu <- integer()
  # nc_calibrated_for_given_mu <- integer()
  # nc_hclust_for_given_mu <- integer()
  
  start <- Sys.time()

  gathered_output <- foreach (
    i= 1:REALISATIONS_FOR_EVERY_MU,
    .combine = "rbind",
    .packages = c("magrittr", "igraph", "dplyr", "NMI", "pracma", "focus")
  ) %dorng% {

    # while (
    #   file.exists(paste0(BENCHMARK_DIR, OUTPUT_NAMES[1])) |
    #   file.exists(paste0(BENCHMARK_DIR, OUTPUT_NAMES[2])) |
    #   file.exists(paste0(BENCHMARK_DIR, OUTPUT_NAMES[3]))
    #   ) {
    #   Sys.sleep(0.1)
    # }
    
    # 1. Creating a network
    # elcomm <- generate_benchmark_graph(
    #   num_of_nodes = NUM_OF_NODES,
    #   avg_deg = 30,
    #   max_deg = 50,
    #   mu = mu,
    #   t1 = 2,
    #   t2 = 3,
    #   c_min = 2,
    #   c_max = 50
    # )
    # gt_comms <- elcomm$comm
    # gt_el <- elcomm$el %>% simplify_edge_list()
    # gt_g <- graph_from_edgelist(as.matrix(gt_el), directed = FALSE)
    # gt_nc <- length(unique(gt_comms$V2))
    # 
    # # 1.1 CLEAN AFTER YOURSELF
    # output_dir <- paste0(
    #   date_dir,
    #   "/",
    #   length(dir(date_dir))+1
    # )
    # 
    # dir.create(output_dir)
    # 
    # # Move the files to the output dir
    # for (file in OUTPUT_NAMES) {
    #   file.rename(
    #     from = paste0(BENCHMARK_DIR, file),
    #     to = paste0(output_dir, "/" ,file)
    #   )
    # }
    
    
    # 1. Loading a network
    
    gt_comms <- read.table(paste0(SIMULATED_GRAPHS, mu, "/realisation_", i, "/community.dat"))
    gt_el <- read.table(paste0(SIMULATED_GRAPHS, mu, "/realisation_", i, "/network.dat")) %>% simplify_edge_list()
    gt_g <- graph_from_edgelist(as.matrix(gt_el), directed = FALSE)
    gt_nc <- length(unique(gt_comms$V2))
    print("Network loaded")
    
    # 2. Getting vanilla NMI and nc
    
    lvn <- cluster_louvain(gt_g)
    partitions <- as.vector(membership(lvn))
    lvn_comms <- cbind(gt_comms, partitions) %>% select(-V2)
    
    # ________________NMI_vanilla_for_given_mu <- c(NMI_vanilla_for_given_mu, NMI(gt_comms, lvn_comms)$value)
    # ________________nc_vanilla_for_given_mu <- c(nc_vanilla_for_given_mu, length(unique(partitions)))
    vanilla_nmi <- NMI(gt_comms, lvn_comms)$value
    vanilla_nc <- length(unique(partitions))
    
    
    # 3. Create the resolution vector here based on the maximum number of communities
    
    f <- function(res) {
      set.seed(1)
      length(unique(membership(cluster_louvain(gt_g, resolution = res)))) - MAX_NC
    } # set the seed inside the function?
    max_res <- round(mean(rep(uniroot(f, lower = 0, upper = 10000, tol = 0.000001, extendInt = "no")$root, 10000)),2)
    RESOLUTION <- linspace(0,max_res, 100)
    
    # 4. Creating a consensus matrix for every resolution value and storing them in con_matrices
    con_matrices <- array(NA, c(nrow(gt_comms), nrow(gt_comms), length(RESOLUTION)))
    res_perturb_nc <- array(0, c(length(RESOLUTION), NUM_OF_PERTURBATIONS))
    
    for (i in 1:length(RESOLUTION)) {
      comem_matrix <- matrix(0, nrow(gt_comms), nrow(gt_comms))
      for (k in 1:NUM_OF_PERTURBATIONS) {
        perturbed_network <- gt_g %>% rewire(keeping_degseq(niter = nrow(gt_el)/100*PERCENT_OF_EDGES_TO_REWIRE))
        perturbed_lvn <- cluster_louvain(perturbed_network, resolution = RESOLUTION[i])
        mem <- membership(perturbed_lvn)
        
        res_perturb_nc[i,k] <- length(unique(mem))  #storing number of clusters
        comem <- CoMembership(mem)
        comem_matrix <- comem_matrix + comem
      }
      con_matrix <- comem_matrix/(length(PERCENT_OF_EDGES_TO_REWIRE)*NUM_OF_PERTURBATIONS)
      con_matrices[,,i] <- con_matrix
    }
    
    # 5. Calculating consensus scores
    
    stab_scores <- numeric()
    
    for (i in 1:dim(con_matrices)[3]) {
      stab_score <- ConsensusScore(con_matrices[,,i], nc = getmode(res_perturb_nc[i,]), K = 100, linkage = "complete")
      stab_scores <- c(stab_scores, stab_score)
    }
    
    final_res <- RESOLUTION[which.max(stab_scores)]
    final_con <- con_matrices[,,which.max(stab_scores)]
    
    
    # 6. Applying Louvain on final_con and using final_res
    
    g <- Graph(final_con, weighted = TRUE, satellites = TRUE)
    lvn_output <- cluster_louvain(graph = g, weights = edge_attr(g)$weight, resolution = final_res)
    partitions <- as.vector(membership(lvn_output))
    lvn_comms <- cbind(gt_comms, partitions) %>% select(-V2)
    
    # ___________NMI_calibrated_for_given_mu <- c(NMI_calibrated_for_given_mu, NMI(gt_comms, lvn_comms)$value)
    # ___________nc_calibrated_for_given_mu <- c(nc_calibrated_for_given_mu, length(unique(partitions)))
    
    cal_nmi <- NMI(gt_comms, lvn_comms)$value
    cal_nc <- length(unique(partitions))
    
    
    # 7. Getting calibrated communities
    output_of_Clusters <- Clusters(final_con, getmode(res_perturb_nc[which.max(stab_scores),]))
    comm_memberships <- cbind(gt_comms, output_of_Clusters) %>% select(-V2)
    
    # NMI_hclust_for_given_mu <- c(NMI_hclust_for_given_mu, NMI(gt_comms, comm_memberships)$value)
    # nc_hclust_for_given_mu <- c(nc_hclust_for_given_mu, length(unique(output_of_Clusters)))
    
    con_nmi <- NMI(gt_comms, comm_memberships)$value
    con_nc <- length(unique(output_of_Clusters))
    
    c(vanilla_nmi, vanilla_nc, cal_nmi, cal_nc, con_nmi, con_nc, gt_nc) # this is what 1 core would return
    
  }
  end <- Sys.time()
  print(paste0("Mu value of ", mu, " took ", end-start))
  
  gt_nc_matrix[mu_idx,] <- gathered_output[,7] 
  
  saveRDS(gt_nc_matrix, "/rds/general/user/mv321/home/thesis/step_2_output/intermediate_gt_nc_matrix.rds")
  
  step_2_NMIs_vanilla <- c(step_2_NMIs_vanilla, mean(gathered_output[,1]))
  step_2_NMIs_calibrated <- c(step_2_NMIs_calibrated, mean(gathered_output[,3]))
  step_2_NMIs_consensus <- c(step_2_NMIs_consensus, mean(gathered_output[,5]))
  
  step_2_NMI_sd_vanilla <- c(step_2_NMI_sd_vanilla, sd(gathered_output[,1]))
  step_2_NMI_sd_calibrated <- c(step_2_NMI_sd_calibrated, sd(gathered_output[,3]))
  step_2_NMI_sd_consensus <- c(step_2_NMI_sd_consensus, sd(gathered_output[,5]))
  
  step_2_NMI_5th_vanilla <- c(step_2_NMI_5th_vanilla, as.numeric(quantile(gathered_output[,1], c(0.05)))) # adding as.numeric to remove the name attribute
  step_2_NMI_5th_calibrated <- c(step_2_NMI_5th_calibrated, as.numeric(quantile(gathered_output[,3], c(0.05))))
  step_2_NMI_5th_consensus <- c(step_2_NMI_5th_consensus, as.numeric(quantile(gathered_output[,5], c(0.05))))
  
  step_2_NMI_95th_vanilla <- c(step_2_NMI_95th_vanilla, as.numeric(quantile(gathered_output[,1], c(0.95)))) # adding as.numeric to remove the name attribute
  step_2_NMI_95th_calibrated <- c(step_2_NMI_95th_calibrated, as.numeric(quantile(gathered_output[,3], c(0.95))))
  step_2_NMI_95th_consensus <- c(step_2_NMI_95th_consensus, as.numeric(quantile(gathered_output[,5], c(0.95))))
  
  step_2_nc_vanilla <- c(step_2_nc_vanilla, getmode(gathered_output[,2]))
  step_2_nc_calibrated <- c(step_2_nc_calibrated, getmode(gathered_output[,4]))
  step_2_nc_consensus <- c(step_2_nc_consensus, getmode(gathered_output[,6]))
  
  step_2_nc_sd_vanilla <- c(step_2_nc_sd_vanilla, sd(gathered_output[,2]))
  step_2_nc_sd_calibrated <- c(step_2_nc_sd_calibrated, sd(gathered_output[,4]))
  step_2_nc_sd_consensus <- c(step_2_nc_sd_consensus, sd(gathered_output[,6]))
}

saveRDS(step_2_NMIs_vanilla, "/rds/general/user/mv321/home/thesis/step_2_output/step_2_NMIs_vanilla.rds")
saveRDS(step_2_NMIs_calibrated, "/rds/general/user/mv321/home/thesis/step_2_output/step_2_NMIs_calibrated.rds")
saveRDS(step_2_NMIs_consensus, "/rds/general/user/mv321/home/thesis/step_2_output/step_2_NMIs_consensus.rds")

saveRDS(step_2_NMI_sd_vanilla, "/rds/general/user/mv321/home/thesis/step_2_output/step_2_NMI_sd_vanilla.rds")
saveRDS(step_2_NMI_sd_calibrated, "/rds/general/user/mv321/home/thesis/step_2_output/step_2_NMI_sd_calibrated.rds")
saveRDS(step_2_NMI_sd_consensus, "/rds/general/user/mv321/home/thesis/step_2_output/step_2_NMI_sd_consensus.rds")

saveRDS(step_2_NMI_5th_vanilla, "/rds/general/user/mv321/home/thesis/step_2_output/step_2_NMI_5th_vanilla.rds")
saveRDS(step_2_NMI_5th_calibrated, "/rds/general/user/mv321/home/thesis/step_2_output/step_2_NMI_5th_calibrated.rds")
saveRDS(step_2_NMI_5th_consensus, "/rds/general/user/mv321/home/thesis/step_2_output/step_2_NMI_5th_consensus.rds")

saveRDS(step_2_NMI_95th_vanilla, "/rds/general/user/mv321/home/thesis/step_2_output/step_2_NMI_95th_vanilla.rds")
saveRDS(step_2_NMI_95th_calibrated, "/rds/general/user/mv321/home/thesis/step_2_output/step_2_NMI_95th_calibrated.rds")
saveRDS(step_2_NMI_95th_consensus, "/rds/general/user/mv321/home/thesis/step_2_output/step_2_NMI_95th_consensus.rds")

saveRDS(step_2_nc_vanilla, "/rds/general/user/mv321/home/thesis/step_2_output/step_2_nc_vanilla.rds")
saveRDS(step_2_nc_calibrated, "/rds/general/user/mv321/home/thesis/step_2_output/step_2_nc_calibrated.rds")
saveRDS(step_2_nc_consensus, "/rds/general/user/mv321/home/thesis/step_2_output/step_2_nc_consensus.rds")

saveRDS(step_2_nc_sd_vanilla, "/rds/general/user/mv321/home/thesis/step_2_output/step_2_nc_sd_vanilla.rds")
saveRDS(step_2_nc_sd_calibrated, "/rds/general/user/mv321/home/thesis/step_2_output/step_2_nc_sd_calibrated.rds")
saveRDS(step_2_nc_sd_consensus, "/rds/general/user/mv321/home/thesis/step_2_output/step_2_nc_sd_consensus.rds")

saveRDS(gt_nc_matrix, "/rds/general/user/mv321/home/thesis/step_2_output/gt_nc_matrix.rds")


# df <- data.frame(MU_grid[1:66], step_2_NMIs_vanilla, step_2_NMIs_calibrated, step_2_NMIs_hclust)
# 
# ggplot(df, aes(x = MU_grid[1:66])) +
#   geom_line(aes(y=step_2_NMIs_vanilla), size = 1, col = "red") +
#   geom_line(aes(y=step_2_NMIs_calibrated), size = 1, col = "green") +
#   geom_line(aes(y=step_2_NMIs_hclust), size = 1, col = "blue")


# Why am I doing the second step:
# 1) Power law distribution of degrees and community sizes
# 2) Reduced complexity because of one parameter
# 3) Try out dynamic number of clusters
# 4) Community sizes are never the same