##' @title Find knn density
##' @param distm distance matrix of class 'dist'
##' @param k number of neighboring data points used to estimate density
##' @export
##' @return density vector of every points. 

findKnnDensity <- function(distm, k){
    distmat <- as.matrix(distm)
    n <- nrow(distmat)
    if (k >= n) {stop(" k is larger than n.", "k = ", k, ", n = ", n, ".")}
    v.d <- pi ^ (1/2) / gamma(1/2 + 1)  # Constant in the case of dimension 1
    r.k <- apply(distmat, 1, sort)[k+1, ]
    k / (n * v.d * r.k)
}