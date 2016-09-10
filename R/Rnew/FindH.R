FindH <- function(p, n, htype){
    if(tolower(htype) == "amise"){
        return(AMISE(p, n))    
    }else if(tolower(htype) == "rot"){
        return(ROT(p, n))
    }else{
        stop("htype can only take two options 'amise' or 'rot'")
    }
}
