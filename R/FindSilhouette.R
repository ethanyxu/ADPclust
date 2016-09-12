FindSilhouette <- function(distm, clusters){
    if(!inherits(distm, 'dist')) stop("arg distm must inherit dist class. Got: ", class(distm))
    if(!all(clusters == floor(clusters))) stop('arg clusters must all be integers')
    if(length(clusters) != attr(distm, 'Size')) stop('length of clusters', length(clusters),
                                                     'not equal to number of observations in distm', attr(distm, 'Size'))
    ans <- mean(cluster::silhouette(x = clusters, dist = distm)[,3])
    return(ans)
}
