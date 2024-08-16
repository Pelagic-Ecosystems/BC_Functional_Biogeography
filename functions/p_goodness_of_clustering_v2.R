# Goodness-of-clustering evaluation
# Patrick Pata
# Modified August 4, 2023 
#
# Removed the first set of indices which require the community matrix for evaluation. Now only the dissimilarity matrix is used.

require(fpc)

goc_index <- function(multiclust, distmat) {
  ceval <- matrix(NA, nrow = ncol(multiclust), ncol = 4)

  for ( k in seq(ncol(multiclust)) ) {
    clust <- multiclust[,k]
    
    # Common indices from clusterstats() of fpc package
    cstat <- cluster.stats(distmat, clust, silhouette = TRUE)
    
    ceval[k,1] <- cstat$dunn # higher better
    ceval[k,2] <- cstat$ch # higher better
    ceval[k,3] <- cstat$avg.silwidth # higher better
    ceval[k,4] <- cstat$pearsongamma # A version of hubert's gamma coefficient... higher better
  }
  
  ceval <- as.data.frame( cbind(c(2:max(multiclust)), ceval) )
  colnames(ceval) <- c("Num_Clusters", "DunnIndex","CalinskiHarabaz",
                       "Silhoutte","PearsonGamma") 
  
  return(ceval)
}

