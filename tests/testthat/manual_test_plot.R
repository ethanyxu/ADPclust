library(ADPclust)
# load("clust3_test.RData") # clust3.{x, distm, h, f, delta, clusters}
load("./tests/testthat/clust3_test.RData") # clust3.{x, distm, h, f, delta, clusters}
a <- adpclust(distm = clust3.distm, p = 2)
plot(a)
a <- adpclust(distm = clust3.distm, centroids = 'user', p = 2)
plot(a)
