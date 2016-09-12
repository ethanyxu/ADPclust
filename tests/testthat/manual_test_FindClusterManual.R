library(ADPclust)
context("Test FindClusterManual")

IsSameClustering <- function(c1, c2){
    if(length(c1) != length(c2)) return(FALSE)
    c1.codes <- unique(c1)
    c2.codes <- unique(c2)
    if(!setequal(c1.codes, c2.codes)) return(FALSE)
    for(c1.code in c1.codes){
        one.cluster <- which(c1 == c1.code)
        pts.in.c2 = c2[one.cluster]
        r <- range(pts.in.c2)
        if(r[1] != r[2]) return(FALSE)
    }
    return(TRUE)
}

load("clust3_test.RData") # clust3.{x, distm, h, f, delta, clusters}
# load("./tests/testthat/clust3_test.RData") # clust3.{x, distm, h, f, delta, clusters}
a <- FindClustersManual(distm = clust3.distm, f = clust3.f, delta = clust3.delta)
# Select 3 centers in the top right corner
IsSameClustering(a[['clusters']], clust3.clusters)
setequal(a[['centers']], c(2, 49, 84))
