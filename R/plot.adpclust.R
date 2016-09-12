##' Depends on the settings of adpclust, draw figures showing silhouette vs. number of clusters, f vs. delta with selected centroids, and original data (projected to the first two principal components if dim > 2) colored by cluster assignments. 
##'
##' @title Visualize the result of adpclust()
##' @param x an object of class "adpclust". Result of adpclust().
##' @param cols vector of colors used to distinguish different clusters. Recycled if necessary.
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

plot.adpclust <- function(x,
                          cols = "default",
                          ...)
{
    if(!inherits(x, 'adpclust')) stop('arg x must inherit adpclust. Got ', class(x))
    if(cols == "default") cols = defCol()
    
    f <- x[['f']]
    delta <- x[['delta']]
    centers <- x[['centers']]
    
    ##--------------------
    ## f vs delta (best one)
    ##--------------------
    graphics::plot(f, delta, xlab = "f(x)", ylab = "delta(x)", 
                   main = "f(x) vs delta(x) \n chosen centers")
    f.range <- range(f)
    delta.range <- range(delta)
    graphics::points(f[centers], delta[centers], 
                     col = cols, pch = 19, cex = 1.2)
    graphics::text(f[centers], delta[centers], labels = centers, cex = 0.6, pos = 1)    
    if(x[['selection.type']] == 'auto'){
        if(length(attr(centers, 'f.cut') > 0)){
            graphics::abline(v = attr(centers, 'f.cut.values'), col = "red", lty = 2)
        }else{
            graphics::segments(f.range[1], delta.range[2],
                               f.range[2], delta.range[1], col = "red", lty = 2)
        }
    }
}

