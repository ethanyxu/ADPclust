##' Find bandwidth h
##'
##' @title Find bandwidth h
##' @param p number of variables
##' @param n number of observations
##' @param htype methods to calculate h. One of the following (case insensitive) c("amise", "rot")
##' @return bandwidth h.
##' @author Ethan Xu
FindH <- function(p, n, htype){
    if(!is.numeric(p)) stop('arg p must be numeric')
    if(length(p) != 1) stop('arg p must be scalar')    
    if(!is.numeric(n)) stop('arg n must be numeric')
    if(length(n) != 1) stop('arg n must be scalar')        
    if(!inherits(htype, 'character')) stop('arg htype not character')
    
    if(tolower(htype) == "amise"){
        return(AMISE(p, n))    
    }else if(tolower(htype) == "rot"){
        return(ROT(p, n))
    }else{
        stop("htype can only take two options 'amise' or 'rot'")
    }
}
