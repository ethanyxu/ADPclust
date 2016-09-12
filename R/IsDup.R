IsDup <- function(x.list, y){
    if(!inherits(x.list, "list")) stop("Expecting a list")

    dup <- FALSE
    for(x in x.list){
        if(setequal(x, y)){
            dup <- TRUE
            break
        }
    }
    return(dup)
}
