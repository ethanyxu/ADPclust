FindSilhouette <- function(distm, clusters){
    ans <- mean(cluster::silhouette(clusters, distm)[,3])
    return(ans)
}
