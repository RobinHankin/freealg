setClass("dot", slots = c(ignore='numeric'))

setMethod("show", "dot", function(object){cat('".[x,y]" returns the commutator xy-yx\n')})

setMethod("[", signature(x="dot",i="ANY",j="missing"),function(x, i, j, drop){stop()})
setMethod("[", signature(x="dot",i="missing",j="ANY"),function(x, i, j, drop){stop()})
setMethod("[", signature(x="dot",i="matrix",j="matrix"),function(x, i, j, drop){i%*%j-j%*%i})

setMethod("[", signature(x="dot",i="ANY",j="ANY"),function(x, i, j, drop){
    out <- i*j - j*i
    if(drop){out <- drop(out)}
    return(out)
} )
