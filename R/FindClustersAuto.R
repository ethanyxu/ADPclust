##' This is the subroutine that automatically finds cluster assignments from given f and delta by testing various parameter settings and find the one that maximizes the silhouette. 
##'
##' @title Automatically find cluster assignment given f and delta.
##' @param distm the distance matrix
##' @param f vector of local distance f(x). See the help of adpclust() for details.
##' @param delta vector of minimal distances to higher ground delta(x). See the help of adpclust() for details.
##' @param ac type of auto selection. The valid options are 1 and 2. See the help of adpclust() for details.
##' @param nclust number of clusters to test. Either a single integer or a vector of integers.
##' @param f.cut number between (0, 1) or numeric vector of numbers between (0, 1). Data points whose f values are larger than f.cut with large delta values are selected as centers. The default is c(0.1, 0.2, 0.3). See the help of FindCentersAutoV() for more details.
##' @export
##' 
##' @return list of four elements: 
##' \itemize{
##' \item{clusters}{ Cluster assignments. A vector of the same length as the number of observations.}
##' \item{centers:}{ Indices of the clustering centers.}
##' \item{silhouette:}{ Silhouette score from the final clustering result.}
##' \item{nclust:}{ Number of clusters.}
##' }
##' @author Ethan Xu
FindClustersAuto <- function(distm,
                             f, 
                             delta, 
                             ac = 1, 
                             nclust = 2:10, 
                             f.cut = c(0.1, 0.2, 0.3)){
    # -------------------------------------------------------------------------    
    # Check arguments
    # -------------------------------------------------------------------------           
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
    
    # -------------------------------------------------------------------------
    # Find centers
    # -------------------------------------------------------------------------
    if(ac == 1){
        center.list <- FindCentersAutoV(f, delta, f.cut = f.cut, 
                                        nclust = nclust, rm.dup = FALSE)
    }else if(ac == 2){
        center.list <- FindCentersAutoD(f, delta, nclust = nclust)
    }else{
        stop("Wrong ac. Must be either 1 or 2. Got ", ac)
    }
    
    if(length(center.list) == 0){
        stop("Failed to find any centers")
    }
    
    # -------------------------------------------------------------------------
    # Cluster
    # -------------------------------------------------------------------------
    cluster.list <- lapply(center.list, 
                           function(x){
                               a <- FindClustersGivenCenters(distm, centers = x)
                               attributes(a) <- attributes(x)
                               return(a)
                           })
    sils.list <- lapply(cluster.list, 
                   function(x){
                       a <- FindSilhouette(distm, clusters = x)
                       attributes(a) <- attributes(x)
                       return(a)
                   })
    
    sils.vector <- unlist(sils.list)
    
    winner.i <- which.max(sils.vector)
    
    ans <- list()
    ans[['clusters']] <- cluster.list[[winner.i]]
    ans[['centers']] <- center.list[[winner.i]]
    ans[['silhouette']] <- sils.list[[winner.i]]
    ans[['nclust']] <- length(ans[['centers']])
    ans[['tested.sils']] <- sils.list
    return(ans)
}
