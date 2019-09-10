`freealg` <- function(words,coeffs){
  stopifnot(is_ok_free(words,coeffs))
  out <- simplify(words,coeffs)  # simplify() is defined in
                                 # RcppExports.R; it returns a list

  class(out) <- "freealg"   # this is the only time class() is set to "freealg"
  return(out)
}

`words` <- function(x){x[[1]]}
`coeffs` <- function(x){x[[2]]}  # accessor methods end here

`is.free` <- function(x){inherits(x,"freealg")}

`is_ok_free` <- function(words,coeffs){
    if( (length(words)==0) && (length(coeffs)==0)){
        return(TRUE)  # zero element
    }
  stopifnot(unlist(lapply(words,is.numeric)))
  stopifnot(is.numeric(coeffs))

  stopifnot(length(words)==length(coeffs))
  return(TRUE)
}


