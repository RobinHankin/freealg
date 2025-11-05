#' @export
setClass("dot", slots = c(ignore='numeric'))

setMethod("show", "dot", function(object){cat('".[x,y]" returns the commutator xy-yx\n')})

#' @export
dot_error <- function(...){
    print(new("dot"))
    stop("Bracket needs two arguments", call. = FALSE)
}

#' @export
setMethod("[", signature(x="dot",i="missing",j="missing"),function(x, i, j, drop){dot_error()})

#' @export
setMethod("[", signature(x="dot",i="ANY",j="missing"),function(x, i, j, drop){dot_error()})

#' @export
setMethod("[", signature(x="dot",i="missing",j="ANY"),function(x, i, j, drop){dot_error()})

#' @export
setMethod("[", signature(x="dot",i="matrix",j="matrix"),function(x, i, j, drop){i%*%j-j%*%i})

#' @export
setMethod("[", signature(x="dot",i="function",j="function"),function(x, i, j, drop){function(z){i(j(z))-j(i(z))}})

#' @export
setMethod("[", signature(x="dot",i="ANY",j="ANY"),function(x, i, j, drop){
    out <- i*j - j*i
    if(drop){out <- drop(out)}
    return(out)
} )

#' @export
`jacobi` <- function(x,y,z){
    jj <- new("dot")
    jj[x,jj[y,z]] + jj[y,jj[z,x]] + jj[z,jj[x,y]]
    ## user: .[x,.[y,z]] + .[y,.[z,x]] + .[z,.[x,y]]

}

#' @export
`ad` <- function(x){
    function(y){
        new("dot")[as.freealg(x),as.freealg(y)]
    }
}
