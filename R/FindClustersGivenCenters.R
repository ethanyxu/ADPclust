##' Find cluster assignments from given centers and distance matrix. Each point is assigned to the center that has the shortest Euclidean distance. 
##'
##' @title Find cluster assignments given centers and distance matrix
##' @param distm distance matrix
##' @param centers vector of integers that gives the indices of centers. Duplications will be silently dropped.
##' @export
##' @return Cluster assignments. A vector of the same length as the number of observations. 
FindClustersGivenCenters <- function(distm, centers){
    if(!inherits(distm, 'dist')) stop("arg distm must be a dist class")
    if(!is.numeric(centers)) stop("arg centers must be numeric. Got ", class(centers))
    if(length(centers) == 0) stop("arg centers must have length > 1")
    if(!all(centers == floor(centers))) stop("arg centers must be a (vector of) integer(s). Got ", centers)
    if(anyDuplicated(centers)) stop("Duplications in centers not allowed")
    if(anyNA(centers)) stop("NA in centers not allowed")
    if(min(centers) <= 0) stop("min(centers) <= 0")
    if(max(centers) > attr(distm, 'Size')) stop("max(centers) larger than number of observations in distm")

    centers <- unique(centers)
    if(length(centers) <= 1) stop('length of unique(centers) must be greater than 1')
    distm <- as.matrix(distm)
    dist.to.centers <- distm[, centers]
    clusters <- apply(dist.to.centers, 1, FUN = which.min)
    return(clusters)
}
