##' A wrapper of the dist() method, with the option to rescale the data with standard deviation of each dimension before calculating the distance matrix. NOTE: If fdelta='mnorm' is passed to adpclust(), then the distm is calculated from rescaled data internally, i.e. distm <- FindDistm(x, normalize = TRUE).
##'
##' @title Find the distance matrix from data.
##' @param x data
##' @param normalize boolean. Normalize data before calculating distance?
##' @param method passed to 'dist()'
##' @export
##' @return distance matrix of class dist.
##' @author Ethan Xu
##‘
##' @examples 
##' # Load a data set 
##' data(clust3)
##' # Specify distm instead of data
##' distm.euclidean <- FindDistm(clust3, method = 'euclidean')
##' distm.gower <- FindDistm(clust3, method = 'gower')
##' distm.rf <- FindDistm(clust3, method = 'rf')

FindDistm <- function(x, normalize = FALSE, method = 'euclidean', 
                      args = list(), verbose = TRUE){
    ## Check arguments
    if(!inherits(x, 'data.frame') && !inherits(x, 'matrix')) stop('arg x must be data frame or matrix')
    if(nrow(x) == 0) stop('x is empty. Cannot calculate distance matrix.')
    if(!inherits(normalize, 'logical')) stop('arg normalize must be boolean')
    
    if(method == 'gower'){
        # Calculate gower distances using cluster::daisy()
        if(verbose) message("Calculating distance matrix with gower method using cluster::daisy()")
        distm <- do.call(cluster::daisy, args = c(list(x=x), args))
        if(verbose) message("Done")
    }else if(method %in% c('rf', 'randomForest')){
        # Calculate random forest distance
        if(verbose) message("Calculating distance matrix with random forest proximity using randomForest::randomForest()")
        rf <- do.call(randomForest::randomForest, 
                     args = c(list(x=x, proximity=TRUE), args))
        distm <- as.dist(1 - rf$proximity)
        if(verbose) message("Done")        
    }else{
        ## Calculate other types of distances using stat::dist
        if(verbose) message("Calculating distance matrix using stat::dist()")
        if(normalize){
            sds <- apply(x, 2, stats::sd)
            distm <- stats::dist(scale(x, center = FALSE, scale = sds), method = method, upper = TRUE)
        }else{
            distm <- stats::dist(x, upper = TRUE, method = method)
        }
        if(verbose) message("Done")
    }
    return(distm)
}
data(clust3)
distm <- FindDistm(clust3, method = 'euclidean')
distm.gower <- FindDistm(clust3, method = 'gower')
distm.rf <- FindDistm(clust3, method = 'rf')

