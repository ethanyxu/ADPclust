FindClustersGivenCenters <- function(distm, centers){
    if(!inherits(d, 'distm')){
        stop("distm must be a dist class")
    }
    if(!inherits(centers, 'integer')){
        stop("distm must be a (vector of) integer(s)")
    }
    if(anyDuplicated(centers)){
        stop("Duplications in centers not allowed")
    }
    if(anyNA(centers)){
        stop("NA in centers not allowed")
    }
    if(min(centers) <= 0){
        stop("min(centers) <= 0")
    }
    if(max(centers) > attr(distm, 'Size')){
        stop("max(centers) larger than number of observations in distm")
    }

    distm <- as.matrix(distm)
    dist.to.centers <- distm[, centers]
    clusters <- apply(dist.to.centers, 1, FUN = which.min)
    return(clusters = clusters)
}
