##' Calculate bandwidth h using various methods.
##'
##' @title Calculate Bandwidth h
##' @param dat 
##' @param h 
##' @param fdelta 
##' @param htype 
##' @param centroids 
##' @return 

checkh <- function(dat, h, fdelta, htype, centroids){
    n <- nrow(dat)
    p <- ncol(dat)

    if(centroids == "user"){
        #---------------------
        # user
        #---------------------

        if(is.null(h)){
            if(fdelta == "mnorm"){
                if(htype == "AMISE"){
                    h <- AMISE(p, n)
                    cat("h missing. Using AMISE bandwidth.\n")
                }else if(htype == "ROT"){
                    h <- ROT(p, n)
                    cat("h missing. Using ROT bandwidth.\n")
                }else
                    stop("h is not given. Please provide a nonnegative value to h; OR set htype ='AMISE' or htype = 'ROT'.")
            }else{
                stop("h is not given. Please provide a nonnegative value to h")
            }
        }else{
            if(is.numeric(h) && h > 0)
                 htype <- "User specified"
            else{
                if(fdelta == "mnorm")
                    stop("h must be a nonnegative number; OR leave h = NULL, and set htype ='AMISE' or htype = 'ROT'.")
                else
                    stop("h must be a nonnegative number")
            }
        }
    }else if(centroids == "auto"){
        #---------------------
        # auto
        #---------------------
        if(is.null(h)){
            if(fdelta == "mnorm"){
                if(htype == "AMISE"){
                    hstar <- AMISE(p, n)
                    h <- seq(hstar / 3, hstar * 3, length.out = 10)
                }
                else if(htype == "ROT"){
                    hstar <- ROT(p, n)
                    h <- seq(hstar / 3, hstar * 3, length.out = 10)
                    ## Fine
                }else{
                    stop("h is missing. htype is not 'AMISE' or 'ROT'.")
                }
            }else{
                stop("h is missing.")
            }
        }else{
            if(is.numeric(h) && all(h > 0))
                 htype <- "User specified"
            else{
                if(fdelta == "mnorm")
                    stop("h must be numberic or NULL. If h = NULL, htype must be one of 'AMISE' or 'ROT'. You can also set h = AMISE(p, n) or h = ROT(p, n), where p and n are dimension and number of observations.")
                else
                    stop("h must be a nonnegative number")
            }
        }
    }else
        stop("centroids only takes two options: 'user' or 'auto'.")
    return(list(h = h, htype = htype))
}
