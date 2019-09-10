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

`numeric_to_free` <- function(x){
  stopifnot(length(x)==1)
  freealg(list(numeric(0)),x)
  }

`is.zero` <- function(x){  length(coeffs(x))==0 }

`is.constant` <- function(x){
  jj <- words(x)
  (length(jj)==1) & identical(jj[[1]],integer(0))
}

"constant" <- function(x){UseMethod("constant")}
"constant<-" <- function(x, value){UseMethod("constant<-")}

`constant.freealg` <- function(x){
  wanted <- sapply(words(x),function(x){length(x)==0})
  if(any(wanted)){
    out <- coeffs(x)[wanted]
  } else {
    out <- 0
  }
  return(out)
}

`constant<-.freealg` <- function(x,value){
  wanted <- sapply(words(x),function(x){length(x)==0})
  if(any(wanted)){
    co <- coeffs(x)
    co[wanted] <- value
    w <- words(x)
    } else {
      co <- c(coeffs(x),value)
      w <- c(words(x),list(numeric(0)))
    }
  freealg(w,co)
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

`rfalg` <- function(n=7, distinct=3, maxsize=4, include.negative=FALSE){
  distinct <- seq_len(distinct)
  if(include.negative){distinct <- c(distinct,-distinct)}
  freealg(replicate(n,sample(distinct,min(1+rgeom(1,1/maxsize),maxsize),replace=TRUE),simplify=FALSE), seq_len(n))
}

`print.freealg` <- function(x,...){
  cat("free algebra element algebraically equal to\n")
  out <- ""
  for(i in seq_along(words(x))){
    co <- coeffs(x)[i]
    if(co>0){
      pm <- " + " # pm = plus or minus
    } else {
      pm <- " - "
    }
    jj <- words(x)[i][[1]]
    if(length(jj)>0){mulsym <- "*"} else {mulsym <- ""}
    if(any(jj<0)){jj[jj<0] <- 26-jj[jj<0]}
    jj <- c(letters,LETTERS)[jj]
    jj <- paste(jj,collapse="")

    out <- paste(out, pm, co, mulsym, jj, sep="")
  }
  cat(out)
  cat("\n")
  return(x)
}
