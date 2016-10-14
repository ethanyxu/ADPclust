library(ADPclust)
context("Test FindClustersAuto")

# FindClustersAuto <- function(distm, f, delta, ac = 1, nclust = 2:10, f.cut = c(0.1, 0.2, 0.3)){}
load("clust3_test.RData") # clust3.{x, distm, h, f, delta, clusters}
# load("./tests/testthat/clust3_test.RData") # clust3.{x, distm, h, f, delta, clusters}
test_that("FindClustersAuto output", {
    a <- FindClustersAuto(clust3.distm, clust3.f, clust3.delta)
    expect_is(a, 'list')
    expect_equal(names(a), c('clusters', 'centers', 'silhouette', 'nclust', 'tested.sils'))
    expect_equal(all(a$clusters == clust3.clusters), TRUE)
    expect_equal(a$nclust, 3)
    expect_equal(all(a$centers == c(2, 49, 84)), TRUE)
    
    a <- FindClustersAuto(clust3.distm, clust3.f, clust3.delta, ac = 2)
    expect_is(a, 'list')
    expect_equal(names(a), c('clusters', 'centers', 'silhouette', 'nclust', 'tested.sils'))
    
    c.got <- a$clusters
    c.expected <- clust3.clusters
    c1 <- unique(c.got)
    c2 <- unique(c.expected)

    expect_equal(length(c1), length(c2))
    expect_equal(setequal(c1, c2), TRUE)
    for(c.code in c1){
        one.cluster <- which(c.got == c.code)
        c.code2 = c.expected[one.cluster]
        r <- range(c.code2)
        expect_equal(r[1], r[2])
    }
    expect_equal(setequal(a$centers,c(2, 49, 84)), TRUE)
})
