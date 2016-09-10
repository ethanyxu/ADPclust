FindFD <- function(distm, h, fdelta){

    ## Find f(x)
    if(fdelta == "unorm")
        f <- 1/(h * sqrt(2 * pi)) * rowSums(exp(-(distm/h)^2/2))
    else if(fdelta == "weighted")
        f <- rowSums(exp(-(distm/h)^2))
    else if(fdelta == "count")
        f <- rowSums(distm < h) - 1
    else if(fdelta == "mnorm"){
        f <- rowSums(exp(-(distm / h) ^ 2 / 2))
    }else{
        stop("Wrong fdelta, try 'unorm', 'weighted', 'count' or 'mnorm' (recommended).")
    }

    ## Find delta(x)
    if(fdelta == "count"){
        f1 <- rank(f, ties.method = "first") # Break ties in f
        delta <- apply(distm / outer(f1, f1, FUN = ">"), 2, min, na.rm = TRUE)
        loc.max <- which.max(delta)
        delta[loc.max] <- max(delta[-loc.max]) # Equation in the Matlab code
    }else if(fdelta == "mnorm"){
        f.order <- order(f, decreasing = TRUE)
        delta <- rep(NA, n)
        delta[f.order[1]] <- Inf
        for(i in 2:length(f.order)){
            delta[f.order[i]] <- min(distm[f.order[i], f.order[1:(i - 1)]])
        }
        delta[f.order[1]] <- max(delta[-f.order[1]])
    }else{
        delta <- apply(distm / outer(f, f, FUN = ">"), 2, min, na.rm = TRUE)
        loc.max <- which.max(delta)
        delta[loc.max] <- max(delta[-loc.max]) # Equation in the Matlab code
    }

    return(list(f = f, delta = delta))
}
