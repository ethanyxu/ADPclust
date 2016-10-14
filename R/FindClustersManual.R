#' Plot the f vs. delta plot, then wait for the user to select centers of clusters by left clicking the points. In general points with both large f and large delta are good candidates of cluster centroids. Selected centers are highlighted. Press ESC to end the selection.
#'
#' @title User-interactive routine to find clusters
#' 
#' @param distm distance matrix.
#' @param f vector of local densities f(x). Same length of the number of observations.
#' @param delta vector of distances to the closest high ground delta(x). Same length of the number of observations.
#' 
#' @export
#' @return a list of the following items:
#' \itemize{
##' \item{clusters}{ Cluster assignments. A vector of the same length as the number of observations.}
#' \item{centers:}{ Indices of the clustering centers.}
#' \item{silhouette:}{ Silhouette score from the final clustering result.}
#' \item{nclust:}{ Number of clusters.}
#' }
#' 
#' @examples
#' data(clust3)
#' distm <- FindDistm(clust3, normalize = TRUE)
#' \dontrun{
#' fd <- FindFD(distm, 2, "mnorm")
#' ans <- FindClustersManual(distm, fd$f, fd$delta)
#' names(ans)
#' ans$centers
#' }

FindClustersManual <- function(distm, f, delta){
    if(!inherits(distm, 'dist')) stop('arg distm must inherit \'dist\'. Got: ', class(distm))
    if(!inherits(f, 'numeric')) stop('arg f must be numeric. Got: ', class(f))
    if(!inherits(delta, 'numeric')) stop('arg delta must be numeric. Got: ', class(delta))    
    if(length(f) != length(delta)){
        stop('lengths of f and delta are different. length(f) = ', length(f), '; length(delta) = ', length(delta))        
    } 
    
    mycols <- defCol()
    
    ## Plot f(x) vs delta(x) plot. Click to select centerss
    # dev.new(width = 12, height = 6)
    graphics::par(mfrow = c(1,1), mar = c(7,4,4,3), mgp = c(3,1,0))
    graphics::plot(f, delta, 
                   main = "Decision Plot",
                   xlab = "",
                   ylab = paste0(expression(delta), "(x)"))
    
    graphics::mtext("f(x)\n Select centroids by left clicking \nPress 'ESC' to end selection", side = 1, line = 4)
    cat("Waiting user selection of centroids on the density-distance plot.\n")
    centers <- PickCenter(f, delta, col = mycols, labelcex = 0.6) # indices of centers
    frange <- range(f); drange <- range(delta)
    graphics::rect(xleft =  frange[1] + 0.3 * (frange[2] - frange[1]),
                   ybottom = drange[1] + 0.4 * (drange[2] - drange[1]),
                   xright = frange[1] + 0.7 * (frange[2] - frange[1]),
                   ytop = drange[1] + 0.6 * (drange[2] - drange[1]), col = "white")
    graphics::text(mean(frange), mean(drange), labels = "Selection Finished.")
    
    if(length(centers) < 2) stop("Select at least two centers.")
    
    ## Assign pts to clusters
    # 'rdist' finds distances between pts to centers; "euclidean distance";
    clusters <- FindClustersGivenCenters(distm, centers)
    silhouette <- FindSilhouette(distm, clusters)
    
    ##------------------------------------
    ## Return
    ##------------------------------------
    ans <- list()
    ans[['clusters']] <- clusters
    ans[['centers']] <- centers
    ans[['silhouette']] <- silhouette
    ans[['nclust']] <- length(ans[['centers']])
    return(ans)
}



