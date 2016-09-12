library(ADPclust)
context("Test IsDup")

test_that("Testing IsDup", {
    x.list <- list(x1 = 1:10, x2 = 1:5, x3 = c(1,6,9))
    x.empty.list <- list()
    x1 <- c(5,3,2)
    x2 <- 1:10    
    x3 <- list(1:10)
    x4 <- x2
    attributes(x4) <- list(a = "test")
    expect_equal(IsDup(x.list, x1), FALSE)
    expect_equal(IsDup(x.list, x2), TRUE)
    expect_equal(IsDup(x.list, x3), FALSE)    
    expect_equal(IsDup(x.list, x4), TRUE)        
    expect_equal(IsDup(x.empty.list, x), FALSE)
})
