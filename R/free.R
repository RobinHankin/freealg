`freealg` <- function(words,coeffs){ # formal
  stopifnot(is_ok_free(words,coeffs))
  out <- simplify(words,coeffs)  # simplify() is defined in
                                 # RcppExports.R; it returns a list

  class(out) <- "freealg"   # this is the only time class() is set to "freealg"
  return(out)
}

`words` <- function(x){x[[1]]}
`coeffs` <- function(x){x[[2]]}  # accessor methods end here

`as.freealg` <- function(x,...){
  if(is.freealg(x)){
    return(x)
  } else if(is.list(x)){
    return(freealg(x[[1]],x[[2]]))
  } else if(is.numeric(x)){
    return(numeric_to_freealg(x))
  } else {
    stop("not recognised")
  }
}

`numeric_to_freealg` <- function(x){
  stopifnot(length(x)==1)
  freealg(list(numeric(0)),x)
  }

`is.zero` <- function(x){  length(coeffs(x))==0 }

`is.constant` <- function(x){
  jj <- words(x)
  (length(jj)==1) & identical(jj[[1]],integer(0))
}

`rfalg` <- function(n=5,r=9){
  freealg(replicate(n,sample(r,1+rgeom(r,0.2),replace=TRUE)),sample(n))
  }

`is.freealg` <- function(x){inherits(x,"freealg")}

`is_ok_free` <- function(words,coeffs){
    if( (length(words)==0) && (length(coeffs)==0)){
        return(TRUE)  # zero element
    }

    if(identical(words,list()) && length(coeffs)==1){
      return(TRUE)
      }
  stopifnot(unlist(lapply(words,is.numeric)))
  stopifnot(is.numeric(coeffs))

  stopifnot(length(words)==length(coeffs))
  return(TRUE)
}

rfalg <- function(n=7, distinct=3, maxsize=4, include.negative=FALSE){
  distinct <- seq_len(distinct)
  if(include.negative){distinct <- c(distinct,-distinct)}
  freealg(replicate(n,sample(distinct,min(1+rgeom(1,1/maxsize),maxsize),replace=TRUE),simplify=FALSE), seq_len(n)+17)
}
