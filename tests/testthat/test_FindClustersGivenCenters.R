library(ADPclust)
context("Test FindClustersGivenCenters")

cluster1 <- data.frame(a = seq(0, 1, length.out = 30), 
                       b = sample(seq(0, 1, length.out = 30), 30))
cluster2 <- data.frame(a = seq(5, 6, length.out = 30),
                       b = sample(seq(5, 6, length.out = 30), 30)) 
x <- rbind(cluster1, cluster2)
distm <- dist(x)

test_that("Testing FindClustersGivenCenters", {
    a <- FindClustersGivenCenters(distm, centers = c(15, 45))
    expect_equal(all(a[1:30] == 1), TRUE)
    expect_equal(all(a[31:60] == 2), TRUE)    
})
