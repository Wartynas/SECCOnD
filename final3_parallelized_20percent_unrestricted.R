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

setwd("/rds/general/user/mv321/home/thesis/")
source("utils.R")

# parallelization stuff

n_cores <- parallel::detectCores() - 1
print(paste0("The number of cores is ", n_cores))

my_cluster <- parallel::makeCluster( #create the cluster
  100,
  # setup_strategy = "sequential", # supposedly is a solution for: Cluster setup failed. 255 of 255 workers failed to connect
  type = "FORK", # change to PSOCK on windows
  outfile = "debug_20percent.txt"
)
print(my_cluster)

doParallel::registerDoParallel(cl = my_cluster) #register it to be used by %dopar% or %dorng%


foreach::getDoParRegistered() # check if it is registered (optional)
foreach::getDoParWorkers() # how many workers are available? (optional)


# Setting constants
NUM_OF_PERTURBATIONS <- 100 # the number of perturbations at a fixed resolution parameter.
PERCENT_OF_EDGES_TO_REWIRE <- 20 # - try 1, 5, 10 and 20.
PK = c(25,25,25,25) # composition of the network
MAX_NC <- sum(PK)/2
NU_WITHIN <- seq(0.64,1,0.01)
NU_BETWEEN <- seq(0.02,1,0.01)
NUM_OF_REALISATIONS <- 100

# Total number of possible edges between clusters will always be equal or greater than within clusters? Check this hypothesis.

NMI_hmap_vanilla_20perc <- array(NA, dim = c(length(NU_WITHIN), length(NU_BETWEEN), NUM_OF_REALISATIONS))
NMI_hmap_calibrated_20perc <- array(NA, dim = c(length(NU_WITHIN), length(NU_BETWEEN), NUM_OF_REALISATIONS))
NMI_hmap_consensus_20perc <- array(NA, dim = c(length(NU_WITHIN), length(NU_BETWEEN), NUM_OF_REALISATIONS))

NMI_sd_hmap_vanilla_20perc <- array(NA, dim = c(length(NU_WITHIN), length(NU_BETWEEN), NUM_OF_REALISATIONS))
NMI_sd_hmap_calibrated_20perc <- array(NA, dim = c(length(NU_WITHIN), length(NU_BETWEEN), NUM_OF_REALISATIONS))
NMI_sd_hmap_consensus_20perc <- array(NA, dim = c(length(NU_WITHIN), length(NU_BETWEEN), NUM_OF_REALISATIONS))

nc_hmap_vanilla_20perc <- array(NA, dim = c(length(NU_WITHIN), length(NU_BETWEEN), NUM_OF_REALISATIONS))
nc_hmap_calibrated_20perc <- array(NA, dim = c(length(NU_WITHIN), length(NU_BETWEEN), NUM_OF_REALISATIONS))
nc_hmap_consensus_20perc <- array(NA, dim = c(length(NU_WITHIN), length(NU_BETWEEN), NUM_OF_REALISATIONS))

resolution_matrix <- array(NA, dim = c(length(NU_WITHIN), length(NU_BETWEEN), NUM_OF_REALISATIONS))


for(n in 1:length(NU_WITHIN)) {
  for(m in 1:length(NU_BETWEEN)) {
    nu_with = NU_WITHIN[n]
    nu_betw = NU_BETWEEN[m]
    print(paste0("nu_within = ", nu_with, ", nu_between = ", nu_betw))
    # if (check_if_community_structure_is_likely(PK, nu_with, nu_betw) < 1) {
    #   print(paste0("Skipping nu_within = ", nu_with, ", nu_between = ", nu_betw, " because community structure is not likely."))
    #   break
    # } else {
    set.seed(42) # setting the seed before the realisation for loop to grant bigger control over reproducibility.
    # NMIs_for_given_nu_pair_vanilla <- numeric()
    # NMIs_for_given_nu_pair_calibrated <- numeric()
    # NMIs_for_given_nu_pair_consensus <- numeric()
    # 
    # nc_for_given_nu_pair_vanilla <- integer()
    # nc_for_given_nu_pair_calibrated <- integer()
    # nc_for_given_nu_pair_consensus <- integer()
    start <- Sys.time()
    gathered_output <- foreach (
      i= 1:NUM_OF_REALISATIONS,
      .combine = "rbind",
      .packages = c("magrittr", "igraph", "dplyr", "NMI", "pracma", "focus")
      ) %dorng% {
      
      # 1. Creating a network
      
      adj <- SimulateAdjacency(pk = PK, topology = "random", nu_within = nu_with, nu_between = nu_betw)
      gt_g = Graph(adj, satellites = TRUE)
      gt_comms <- create_community_dataframe_from_pk(PK)
      gt_el <- as_edgelist(gt_g, names = TRUE) %>% as.data.frame()
      
      
      # 2. Applying louivain method, calculating NMI and storing that into a vector
      
      lvn <- cluster_louvain(gt_g)
      partitions <- as.vector(membership(lvn))
      lvn_comms <- cbind(gt_comms, partitions) %>% select(-V2)
      
      #_________NMIs_for_given_nu_pair_vanilla <- c(NMIs_for_given_nu_pair_vanilla, NMI(gt_comms, lvn_comms)$value)
      #_________nc_for_given_nu_pair_vanilla <- c(nc_for_given_nu_pair_vanilla, length(unique(partitions)))
      vanilla_nmi <- NMI(gt_comms, lvn_comms)$value
      vanilla_nc <- length(unique(partitions))
      
      
      # 3. Create the resolution vector here based on the maximum number of communities
      
      f <- function(res) {
        set.seed(1)
        length(unique(membership(cluster_louvain(gt_g, resolution = res)))) - MAX_NC
      }
      max_res <- round(mean(rep(uniroot(f, lower = 0, upper = 10000, tol = 0.000001, extendInt = "no")$root, 10000)),2)
      RESOLUTION <- linspace(0,max_res, 100)
      
      
      # 4. Creating many consensus matrices
      
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
      
      
      # 6. Running louvain on calibrated consensus matrix + calibrated resolution and consensus community detection
      
      #Calibrated
      g <- Graph(final_con, weighted = TRUE, satellites = TRUE)
      lvn_output <- cluster_louvain(graph = g, weights = edge_attr(g)$weight, resolution = final_res)
      partitions <- as.vector(membership(lvn_output))
      lvn_comms <- cbind(gt_comms, partitions) %>% select(-V2)
      
      #_________NMIs_for_given_nu_pair_calibrated <- c(NMIs_for_given_nu_pair_calibrated, NMI(gt_comms, lvn_comms)$value)
      #_________nc_for_given_nu_pair_calibrated <- c(nc_for_given_nu_pair_calibrated, length(unique(partitions)))
      cal_nmi <- NMI(gt_comms, lvn_comms)$value
      cal_nc <- length(unique(partitions))
      
      # Using the calibrated consensus matrix and Clusters function
      output_of_Clusters <- Clusters(final_con, getmode(res_perturb_nc[which.max(stab_scores),]))
      lvn_comms <- cbind(gt_comms, output_of_Clusters) %>% select(-V2)
      #_________NMIs_for_given_nu_pair_consensus <- c(NMIs_for_given_nu_pair_consensus, NMI(gt_comms, lvn_comms)$value)
      #_________nc_for_given_nu_pair_consensus <- c(nc_for_given_nu_pair_consensus, length(unique(output_of_Clusters)))
      con_nmi <- NMI(gt_comms, lvn_comms)$value
      con_nc <- length(unique(output_of_Clusters))
      
      c(vanilla_nmi, vanilla_nc, cal_nmi, cal_nc, con_nmi, con_nc, final_res) # this is what 1 core would return
    }
    end <- Sys.time()
    print(paste0("Combination of nu_within = ", nu_with, " and nu_between = ", nu_betw, " took ", end-start))
    
    NMI_hmap_vanilla_20perc[n,m,] <- gathered_output[,1]
    NMI_hmap_calibrated_20perc[n,m,] <- gathered_output[,3]
    NMI_hmap_consensus_20perc[n,m,] <- gathered_output[,5]
    
    nc_hmap_vanilla_20perc[n,m,] <- gathered_output[,2]
    nc_hmap_calibrated_20perc[n,m,] <- gathered_output[,4]
    nc_hmap_consensus_20perc[n,m,] <- gathered_output[,6]
    
    resolution_matrix[n,m,] <- gathered_output[,7]
    
    saveRDS(NMI_hmap_vanilla_20perc, "/rds/general/user/mv321/home/thesis/step_1_20percent_unrestricted_3D_output/intermediate_NMI_hmap_vanilla_20perc.rds")
    saveRDS(NMI_hmap_calibrated_20perc, "/rds/general/user/mv321/home/thesis/step_1_20percent_unrestricted_3D_output/intermediate_NMI_hmap_calibrated_20perc.rds")
    saveRDS(NMI_hmap_consensus_20perc, "/rds/general/user/mv321/home/thesis/step_1_20percent_unrestricted_3D_output/intermediate_NMI_hmap_consensus_20perc.rds")
    
    saveRDS(nc_hmap_vanilla_20perc, "/rds/general/user/mv321/home/thesis/step_1_20percent_unrestricted_3D_output/intermediate_nc_hmap_vanilla_20perc.rds")
    saveRDS(nc_hmap_calibrated_20perc, "/rds/general/user/mv321/home/thesis/step_1_20percent_unrestricted_3D_output/intermediate_nc_hmap_calibrated_20perc.rds")
    saveRDS(nc_hmap_consensus_20perc, "/rds/general/user/mv321/home/thesis/step_1_20percent_unrestricted_3D_output/intermediate_nc_hmap_consensus_20perc.rds")
    
    saveRDS(resolution_matrix, "/rds/general/user/mv321/home/thesis/step_1_20percent_unrestricted_3D_output/intermediate_resolution_matrix_20perc.rds")
    # }
  }
}

saveRDS(NMI_hmap_vanilla_20perc, "/rds/general/user/mv321/home/thesis/step_1_20percent_unrestricted_3D_output/NMI_hmap_vanilla_20perc.rds")
saveRDS(NMI_hmap_calibrated_20perc, "/rds/general/user/mv321/home/thesis/step_1_20percent_unrestricted_3D_output/NMI_hmap_calibrated_20perc.rds")
saveRDS(NMI_hmap_consensus_20perc, "/rds/general/user/mv321/home/thesis/step_1_20percent_unrestricted_3D_output/NMI_hmap_consensus_20perc.rds")

saveRDS(nc_hmap_vanilla_20perc, "/rds/general/user/mv321/home/thesis/step_1_20percent_unrestricted_3D_output/nc_hmap_vanilla_20perc.rds")
saveRDS(nc_hmap_calibrated_20perc, "/rds/general/user/mv321/home/thesis/step_1_20percent_unrestricted_3D_output/nc_hmap_calibrated_20perc.rds")
saveRDS(nc_hmap_consensus_20perc, "/rds/general/user/mv321/home/thesis/step_1_20percent_unrestricted_3D_output/nc_hmap_consensus_20perc.rds")

saveRDS(resolution_matrix, "/rds/general/user/mv321/home/thesis/step_1_20percent_unrestricted_3D_output/resolution_matrix_20perc.rds")



