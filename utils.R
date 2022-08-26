
ConsensusScore <- function(coprop, nc, K = K, linkage = "complete") {
  # Clustering on the consensus matrix
  sh_clust <- hclust(as.dist(1 - coprop), method = linkage)
  
  # Identifying stable clusters
  theta <- cutree(sh_clust, k = nc)
  # print(table(theta))
  
  # Probability that items i and j belong to the same cluster
  N <- length(theta) * (length(theta) - 1) / 2
  p_unif <- sum(table(theta) * (table(theta) - 1)) / (2 * N) # P(i) * P(j | i)
  # print(p_unif)
  # print(sum(coprop[upper.tri(coprop)])/N)
  # p_unif=sum(coprop[upper.tri(coprop)])/N
  # print(p_unif)
  # print(N)
  
  # Calculating log-likelihood for observed consensus matrix
  loglik <- 0
  for (i in 1:(nrow(coprop) - 1)) {
    for (j in (i + 1):nrow(coprop)) {
      if (theta[i] == theta[j]) {
        loglik <- loglik + pbinom(round(coprop[i, j] * K) - 1, size = K, prob = p_unif, lower.tail = FALSE, log = TRUE)
      } else {
        loglik <- loglik + pbinom(round(coprop[i, j] * K), size = K, prob = p_unif, lower.tail = TRUE, log = TRUE)
      }
    }
  }
  
  # Calculating numbers of within and between cluster pairs
  n_1 <- p_unif * N
  n_3 <- (1 - p_unif) * N
  
  # Calculating log-likelihood for least stable (uniform) consensus matrix
  p_1 <- pbinom(round(p_unif * K) - 1, size = K, prob = p_unif, lower.tail = FALSE, log = TRUE)
  p_3 <- pbinom(round(p_unif * K), size = K, prob = p_unif, lower.tail = TRUE, log = TRUE)
  worst_score <- (n_1 * p_1 + n_3 * p_3)
  
  # Calculating log-likelihood for most stable (binary) consensus matrix
  p_1 <- dbinom(K, size = K, prob = p_unif, log = TRUE)
  p_3 <- dbinom(0, size = K, prob = p_unif, log = TRUE)
  best_score <- (n_1 * p_1 + n_3 * p_3)
  
  # Calculating consensus score as likelihood ratio
  score <- (-loglik + worst_score) / (-best_score + worst_score) # ( x - min ) / ( max - min )
  
  return(score)
}


HierarchicalClustering <- function(xdata, nc = NULL, scale = TRUE, rows = TRUE, ...) {
  # Storing extra arguments
  extra_args <- list(...)
  
  # Transposing for clustering of columns
  if (!rows) {
    xdata <- t(xdata)
  }
  
  # Scaling the data
  if (scale) {
    xdata <- scale(xdata)
  }
  
  # Re-formatting nc
  if (!is.null(nc)) {
    if (is.vector(nc)) {
      nc <- cbind(nc)
    }
  } else {
    nc <- cbind(seq(1, nrow(xdata)))
  }
  
  # Extracting relevant extra arguments (dist)
  tmp_extra_args <- MatchingArguments(extra_args = extra_args, FUN = stats::dist)
  tmp_extra_args <- tmp_extra_args[!names(tmp_extra_args) %in% c("x")]
  
  # Computing pairwise distances
  mydistance <- do.call(stats::dist, args = c(list(x = xdata), tmp_extra_args))
  
  # Extracting relevant extra arguments (hclust)
  tmp_extra_args <- MatchingArguments(extra_args = extra_args, FUN = stats::hclust)
  tmp_extra_args <- tmp_extra_args[!names(tmp_extra_args) %in% c("d")]
  
  # Running hierarchical clustering
  myclust <- do.call(stats::hclust, args = c(list(d = mydistance), tmp_extra_args))
  
  # Initialisation of array storing co-membership matrices
  adjacency <- array(NA, dim = c(nrow(xdata), nrow(xdata), nrow(nc)))
  
  # Defining clusters
  mygroups <- do.call(stats::cutree, args = list(tree = myclust, k = nc))
  if (is.null(dim(mygroups))) {
    mygroups <- cbind(mygroups)
  }
  for (i in 1:nrow(nc)) {
    adjacency[, , i] <- CoMembership(groups = mygroups[, i])
  }
  
  return(list(comembership = adjacency))
}


ClusteringAlgo <- function(xdata,
                           Lambda = NULL, nc,
                           implementation = HierarchicalClustering, ...) {
  # Making sure none of the variables has a null standard deviation
  mysd <- rep(NA, ncol(xdata))
  for (j in 1:ncol(xdata)) {
    mysd[j] <- stats::sd(xdata[, j])
  }
  if (any(mysd == 0)) {
    for (k in which(mysd == 0)) {
      xdata[, k] <- xdata[, k] + stats::rnorm(n = nrow(xdata), sd = min(mysd[mysd != 0]) / 100)
    }
  }
  
  # Applying user-defined function for variable selection
  out <- do.call(implementation, args = list(xdata = xdata, nc = nc, Lambda = Lambda, ...))
  
  if ("weight" %in% names(out)) {
    beta_full <- out$weight
    
    # Setting the beta coefficient to zero for predictors with always the same value (null standard deviation)
    if (any(mysd == 0)) {
      selected[, which(mysd == 0)] <- 0
      if (length(dim(beta_full)) == 2) {
        beta_full[, which(mysd == 0)] <- 0
      }
      if (length(dim(beta_full)) == 3) {
        beta_full[, which(mysd == 0), ] <- 0
      }
    }
  } else {
    beta_full <- matrix(1, nrow = length(nc), ncol = ncol(xdata))
    rownames(beta_full) <- paste0("s", seq(0, nrow(beta_full) - 1))
    colnames(beta_full) <- colnames(xdata)
  }
  
  return(list(comembership = out$comembership, weight = beta_full))
}


create_community_dataframe_from_pk <- function(pk) {
  V1 <- paste0("var", 1:sum(pk))
  V2 <- integer()
  for(i in 1:length(pk)) {
    V2 <- c(V2, rep(i, pk[i]))
  }
  return(data.frame(V1 = V1, V2 = V2))
}


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


# Based on the pk, this function calculates the maximum theoretical mu_between so that community structure would be theoretically possible.
get_max_possible_mu_between <- function (pk) {
  all_nodes <- sum(pk)
  
  total <- all_nodes*(all_nodes-1)/2
  total_within <- 0
  total_between <- 0
  for (el in pk) {
    total_within <- total_within + el*(el-1)/2
    total_between <- total_between + el*(all_nodes-el)
  }
  total_between <- total_between/2
  print(paste0("Total possible edges = ", total))
  print(paste0("Total possible edges within clusters = ", total_within))
  print(paste0("Total possible edges between clusters = ", total_between))
  print(total == total_within + total_between)
  res = round(total_within/total_between, 2)
  # print(paste0("Maximum possible mu_between = ", res, ". For the community structure to be possible, mu_within should be at least ", 1/res, " times higher than mu_between."))
  return(res)
}
## Instead of focusing on the total number of within/between edges, put a contrain on expected in.out degree of a node!!!


check_if_community_structure_is_likely <- function(pk, nu_within, nu_between) {
  all_nodes <- sum(pk)
  ratios <- numeric()
  for (el in pk) {
    rest <- all_nodes - el
    total_in_edges <- el*(el-1)/2
    kin <- total_in_edges *nu_within/el
    
    total_out_edges <- el*rest
    kout <- total_out_edges * nu_between/el
    
    ratio <- kin/(kout*(el/rest)) # el/rest is a scaling constant to account for the fact that rest >> el
    # print(paste0("Ratio of kin and scaled version of kout is ", ratio))
    ratios <- c(ratios, ratio)
  }
  return(mean(ratios))
}




## Given a calibrated consensus matrix, this function returns the cluster membership vector.
Clusters <- function(stable_con_matrix, nc) {
  # Extracting stable clusters from hierarchical clustering
  shclust <- stats::hclust(as.dist(1 - stable_con_matrix), method = "complete")
  mymembership <- stats::cutree(shclust, k = nc)
  
  return(mymembership)
}


CommunityScore <- function(stability_score, gt_nc, nc, alpha) {
  return((1/(abs(gt_nc-nc)+1))^(1/alpha)*stability_score)
}

