##' Automatically find cluster assignment that maximizes silouette. 
##'
##' @title Automatically find cluster assignment that maximizes silouette.
##' @param distm distance matrix
##' @param f vector of local distance f(x)
##' @param delta vector of minimal distances to higher ground delta(x)
##' @param ac type of auto center picking from the f(x) vs. delta(x) plot. Only takes two values c(1, 2). If ac == 1 centers points with highest delta(x)'s to the right of vertical line at f(x) = f.cut. If ac == 2 centers are points above diagonal line. 
##' @param nclust number of clusters. Can be a vector. 
##' @param f.cut f(x) cuts to use when ac == 1. Must be values between 0 and 1. Can be a vector.
##' @export
##' 
##' @return list of four elements: clusters, centers, silhouette and nclust.
##' @author Ethan Xu
FindClustersAuto <- function(distm,
                             f, 
                             delta, 
                             ac = 1, 
                             nclust = 2:10, 
                             f.cut = c(0.1, 0.2, 0.3)){
    if(!inherits(distm, 'dist')) stop("arg distm must inherit dist class. Got ", class(distm))
    if(!is.numeric(f)) stop("arg f must be numeric. Got ", class(f))
    if(!is.numeric(delta)) stop("arg delta must be numeric. Got ", class(delta))
    if(attr(distm, 'Size') != length(f)) stop("length of f (", length(f),") not equal to number of observations in distm (", attr(distm, 'Size'), ")")
    if(attr(distm, 'Size') != length(delta)) stop("length of delta (", length(delta),") not equal to number of observations in distm (", attr(distm, 'Size'), ")")
    if(!all.equal(nclust, as.integer(nclust))) stop('arg nclust must all be integers. Got ', class(nclust))
    if(min(nclust) <= 0) stop('nclust must be positive integers')
    if(!is.numeric(f.cut)) stop('arg f.cut must be numeric. Got ', class(f.cut))
    if(length(f.cut) == 0) stop('arg f.cut is empty: ', f.cut)    
    if(min(f.cut) < 0) stop('arg f.cut must be between 0 - 1. Got', f.cut)
    if(max(f.cut) >= 1) stop('arg f.cut must be between 0 - 1. Got', f.cut)  
    if(length(ac) != 1) stop('arg ac must have length 1. Got', ac)

    if(ac == 1){
        center.list <- FindCentersAutoV(f, delta, f.cut = f.cut, nclust = nclust, rm.dup = TRUE)
    }else if(ac == 2){
        center.list <- FindCentersAutoD(f, delta, nclust = nclust, rm.dup = TRUE)
    }else{
        stop("Wrong ac. Must be either 1 or 2. Got ", ac)
    }

    if(length(center.list) == 0){
        stop("Failed to find any centers")
    }
    cluster.list <- lapply(center.list, function(x) FindClustersGivenCenters(distm, centers = x))
    sils <- lapply(cluster.list, function(x) FindSilhouette(distm, clusters = x))
    sils <- unlist(sils)

    winner.i <- which.max(sils)

    ans <- list()
    ans[['clusters']] <- cluster.list[[winner.i]]
    ans[['centers']] <- center.list[[winner.i]]
    ans[['silhouette']] <- sils[[winner.i]]
    ans[['nclust']] <- length(ans[['centers']])
    return(ans)
}
