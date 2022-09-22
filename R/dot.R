setClass("dot", slots = c(ignore='numeric'))

setMethod("show", "dot", function(object){cat('".[x,y]" returns the commutator xy-yx\n')})
dot_error <- function(...){
    print(new("dot"))
    stop("Bracket needs two arguments", call. = FALSE)
}

setMethod("[", signature(x="dot",i="missing",j="missing"),function(x, i, j, drop){dot_error()})
setMethod("[", signature(x="dot",i="ANY",j="missing"),function(x, i, j, drop){dot_error()})
setMethod("[", signature(x="dot",i="missing",j="ANY"),function(x, i, j, drop){dot_error()})
setMethod("[", signature(x="dot",i="matrix",j="matrix"),function(x, i, j, drop){i%*%j-j%*%i})

setMethod("[", signature(x="dot",i="function",j="function"),function(x, i, j, drop){function(z){i(j(z))-j(i(z))}})

setMethod("[", signature(x="dot",i="ANY",j="ANY"),function(x, i, j, drop){
    out <- i*j - j*i
    if(drop){out <- drop(out)}
    return(out)
} )

`jacobi` <- function(x,y,z){
    jj <- new("dot")
    jj[x,jj[y,z]] + jj[y,jj[z,x]] + jj[z,jj[x,y]]
    ## user: .[x,.[y,z]] + .[y,.[z,x]] + .[z,.[x,y]]

}

`ad` <- function(x){
    function(y){
        .[as.freealg(x),as.freealg(y)]
    }
}
