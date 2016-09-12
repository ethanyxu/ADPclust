##' Summarizes results from the adpclust function from the ADPclust package.
##'
##' @title Summary of adpclust
##' @param object object with class "adpclust". Return of adpclust()
##' @param ... other arguments. NOT used in theis version. 
##' @return NULL
##' @export

summary.adpclust <- function(object, ...){
    cat("-- ADPclust Procedure -- \n\n")
    cat("Number of obs.: \t", length(object[['clusters']]), "\n")
    cat("Centroids selection: \t", object[['selection.type']], "\n")
    cat("Number of clusters: \t", length(object[['centers']]), "\n")
    cat("Avg. Silhouette: \t", object[['silhouette']], "\n")
    cat("Elements in list: \t", paste0(names(object)), sep = " $")
    cat("\nf(x): \n")
    print(summary(object[['f']]))
    cat("\ndelta(x): \n")
    print(summary(object[['delta']]))

    invisible(NULL)
}

