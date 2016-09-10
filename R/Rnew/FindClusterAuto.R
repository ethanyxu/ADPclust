FindClusterAuto <- function(distm, f, delta, ac, nclust, f.cut = c(0.1, 0.2, 0.3)){
    center.list <- FindCentersAuto(f, delta, ac, f.cut)
    
    if(ac == 1){
        center.list <- FindCentersAutoV(f, delta, f.cut = c(0.1, 0.2, 0.3), nclust, rm.dup = TRUE)
    }else if(ac == 2){
        center.list <- FindCentersAutoD(f, delta, nclust, rm.dup = TRUE)
    }else{
        stop(paste("Wrong ac. Must be either 1 or 2. Got ", ac))
    }

    if(length(center.list) == 0){
        stop("Failed to find any centers")
    }

    cluster.list <- lapply(center.list, function(x) FindClustersGivenCenters(distm, centers = x))
    sils <- lapply(cluster.list, function(x) FindSilhouette(distm, clusters = x))
    sils <- unlist(sils)

    winner.i <- which.max(sils)

    ans <- list()
    ans['cluster'] <- cluster.list[[winner.i]]
    ans['centers'] <- center.list[[winner.i]]
    ans['silhouette'] <- sils[[winner.i]]
    ans['nclust'] <- length(centers)
    return(ans)
}
