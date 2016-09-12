library(ADPclust)
context("Test FindCentersAutoV")

# FindCentersAutoV <- function(f, delta, f.cut = c(0.1, 0.2, 0.3), nclust, rm.dup = TRUE)

test_that("Testing FindCentersAutoV output format", {
    load("data_fd.RData")
    a <- FindCentersAutoV(f, delta, f.cut = 0.1, nclust = 3, rm.dup = TRUE)
    expect_is(a, 'list')
    expect_equal(length(a), 1)

})

test_that("Testing FindCentersAutoV output values",{
    load("data_fd.RData") # load sample f and delta
    a <- FindCentersAutoV(f, delta, f.cut = 0.1, nclust = 3, rm.dup = TRUE)
    expect_equal(setequal(a[[1]], c(16, 18, 100)), TRUE)    
    a <- FindCentersAutoV(f, delta, f.cut = 0.1, nclust = 4, rm.dup = TRUE)
    expect_equal(setequal(a[[1]], c(16, 18, 80, 100)), TRUE)            
    a <- FindCentersAutoV(f, delta, f.cut = 0.5, nclust = 2, rm.dup = TRUE)
    expect_equal(setequal(a[[1]], c(80, 100)), TRUE)
    expect_error(FindCentersAutoV(f, delta, f.cut = 0.99, nclust = 5, rm.dup = TRUE), 
                 'Only (1) points to the right of f.cut (99.01), but nclust = 5',
                 fixed = TRUE)
    # Test multiple f.cut
    a <- FindCentersAutoV(f, delta, f.cut = c(0.1, 0.2, 0.3), nclust = 2, rm.dup = FALSE)    
    expect_equal(length(a), 3)
    expect_equal(setequal(a[[1]], c(18, 100)), TRUE)
    expect_equal(setequal(a[[2]], c(80, 100)), TRUE)
    expect_equal(setequal(a[[3]], c(80, 100)), TRUE)    
    a <- FindCentersAutoV(f, delta, f.cut = c(0.1, 0.2, 0.3), nclust = 2, rm.dup = TRUE)
    expect_equal(length(a), 2)
    expect_equal(setequal(a[[1]], c(18, 100)), TRUE)    
    expect_equal(setequal(a[[2]], c(80, 100)), TRUE)    
    a <- FindCentersAutoV(f, delta, f.cut = seq(0.1, 0.3, 0.01), nclust = 2, rm.dup = TRUE)    
    expect_equal(length(a), 2)
    expect_equal(setequal(a[[1]], c(18, 100)), TRUE)    
    expect_equal(setequal(a[[2]], c(80, 100)), TRUE)        
    a <- FindCentersAutoV(f, delta, f.cut = seq(0.1, 0.3, 0.01), nclust = 2, rm.dup = FALSE)    
    expect_equal(length(a), 21)
    for(ia in a) expect_equal(length(ia), 2)
    
    # Test multiple nclust
    a <- FindCentersAutoV(f, delta, f.cut = 0.05, nclust = 1:5, rm.dup = FALSE)
    expect_equal(length(a), 5)
    for(i in 1:5){
        expect_equal(setequal(a[[i]], c(100, 18, 16, 10, 6)[1:i]), TRUE)
    }
    a <- FindCentersAutoV(f, delta, f.cut = c(0.05, 0.2), nclust = 1:5, rm.dup = FALSE)   
    expect_equal(length(a), 10)
})