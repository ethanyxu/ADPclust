# Introduction
ADPclust (Fast Clustering Using Adaptive Density Peak Detection) is a non-iterative procedure that clusters high dimensional data by finding cluster centers from estimated density peaks. It incorporates multivariate local Gaussian density estimation. The number of clusters as well as bandwidths can either be selected by the user or selected automatically through an internal clustering criterion.

### Most recent version: 0.6.5

# References
* **Vignette:** http://hal.case.edu/~yifan/ADPclust.html
* **CRAN release** (0.6.3)**:** https://cran.r-project.org/web/packages/ADPclust/index.html 
* **Journal paper:** 
  * Xiao-Feng Wang, and Yifan Xu, (2015) "Fast Clustering Using Adaptive Density Peak Detection." *Statistical Methods in Medical Research, doi:10.1177/0962280215609948* ([PubMed Link](http://www.ncbi.nlm.nih.gov/pubmed/26475830))
  * Alex Rodriguez, and Alessandro Laio, (2014) "Clustering by fast search and find of density peaks." *Science 344, no. 6191 (2014): 1492-1496*

# Installation
Install the most recent version from github:
```R
## In R do:
## Skip this line if you already have devtools installed
install.packages("devtools")
library(devtools)
install_github("ethanyxu/ADPclust")
library(ADPclust)
```

OR install the released version from CRAN
```R
## In R do:
install.packages("ADPclust")
library(ADPclust)
```

# Simple Examples
Run on a preloaded data set:
```R
library(ADPclust)
data(clust3)
# Automatic clustering
ans <- adpclust(clust3)
plot(ans)
summary(ans)

# Manual centroids selection
adpclust(clust3, centroids = "user")
```
For more examples please see the [Vignette](http://hal.case.edu/~yifan/ADPclust.html).
