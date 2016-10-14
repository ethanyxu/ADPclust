##' @importFrom graphics abline axis par plot text points segments 
NULL
##' @importFrom stats complete.cases
NULL

##' Plot the f vs. delta plot with selected centroids.
##'
##' @title Visualize the result of adpclust()
##' @param x an object of class "adpclust". Result of adpclust().
##' @param cols vector of colors used to distinguish different clusters. Recycled if necessary.
##' @param to.plot string vector that indicate which plot(s) to show. The two options are 'cluster.sil' (nclust vs. silhouette) and 'fd' (f vs. delta).
##' @param ... Not used.
##' @return NULL
##'
##' @export
##'
##' @examples
##' ## Load a data set with 3 clusters
##' data(clust3)
##' ## Automatically select cluster centroids
##' ans <- adpclust(clust3, centroids = "auto")
##' plot(ans)
##' plot(ans, to.plot = "fd")
##' plot(ans, to.plot = "cluster.sil")
##' plot(ans, to.plot = c("cluster.sil", "fd")) #Default


plot.adpclust <- function(x,
                          cols = "default",
                          to.plot = c("cluster.sil", "fd"),
                          ...)
{
    nclusters <- sils <- NULL # Null out to remove "no visible binding for global variable" note from R check.
    if(!inherits(x, 'adpclust')) stop('arg x must inherit adpclust. Got ', class(x))
    if(cols == "default") cols = defCol()
    if(!all(to.plot %in% c("cluster.sil", "fd"))) stop('to.plot must be "cluster.sil" and/or "fd".')
    
    # Recycle colors 
    if((temp <- ceiling(x$nclust / length(cols))) > 1)
        cols <- rep(cols, temp)[1:x$nclust]
    
    f <- x[['f']]
    delta <- x[['delta']]
    centers <- x[['centers']]
    
    par(mfrow = c(1, length(unique(to.plot))))
    ##--------------------
    ## nclust vs. silouette
    ##--------------------
    if("cluster.sil" %in% to.plot){
        tried <- data.frame(nclusters = NA, sils = NA)
        for(i in 1:length(x$tested)){
            tried[i, 'nclusters'] <- x$tested[[i]][['nclust']]
            tried[i, 'sils'] <- x$tested[[i]][['sil']]
        }
        tried <- tried[complete.cases(tried), ]
        tried <- dplyr::group_by(tried, nclusters)
        tried <- dplyr::summarize(tried, best.sil = max(sils))
        plot(tried, type = "b", xlab = "number of clusters",
             ylab = "silhouette", xaxt = "n", 
             main = "# cluster vs silhouette")
        abline(v = x$nclust, col = "red", lty = 2)
        axis(1, at = tried$nclusters, labels = tried$nclusters)            
    }
    
    ##--------------------
    ## f vs delta
    ##--------------------
    if("fd" %in% to.plot){
        plot(f, delta, xlab = "f(x)", ylab = "delta(x)", 
             main = "f(x) vs delta(x) \n chosen centers")
        f.range <- range(f)
        delta.range <- range(delta)
        points(f[centers], delta[centers], 
               col = cols, pch = 19, cex = 1.2)
        text(f[centers], delta[centers], labels = centers, cex = 0.6, pos = 1)
        if(x[['selection.type']] == 'auto'){
            if(length(attr(centers, 'f.cut')) > 0){
                abline(v = attr(centers, 'f.cut.value'), col = "red", lty = 2)
            }else{
                segments(f.range[1], delta.range[2],
                         f.range[2], delta.range[1], col = "red", lty = 2)
            }
        }        
    }
}

