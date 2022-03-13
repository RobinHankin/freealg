`freealg` <- function(words,coeffs){ # formal
  stopifnot(is_ok_free(words,coeffs))
  out <- lowlevel_simplify(words,coeffs)  # simplify() is defined in
                                 # RcppExports.R; it returns a list

  class(out) <- "freealg"   # this is the only time class() is set to "freealg"
  return(out)
}

`words` <- function(x){disord(x[[1]],hashcal(x))}
`coeffs` <- function(x){disord(x[[2]],hashcal(x))} # accessor methods end here

`coeffs<-` <- function(x,value){UseMethod("coeffs<-")}
`coeffs<-.freealg` <- function(x,value){
  if(is.zero(x)){return(x)}
  jj <- coeffs(x)
  if(is.disord(value)){
    stopifnot(consistent(words(x),value))
    jj <- value
  } else {
    jj[] <- value  # the meat
  }
  freealg(words(x),jj)
}

`as.freealg` <- function(x,...){
  if(is.freealg(x)){
    return(x)
  } else if(is.list(x)){
    return(freealg(x[[1]],x[[2]]))
  } else if(is.numeric(x) &&(length(x)==1)){
    return(numeric_to_free(x))
  } else if(is.numeric(x) &&(length(x) > 1)){
    return(vector_to_free(x))
  } else if(is.character(x)){
    return(natural_char_to_freealg(x))
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

`constant.numeric` <- function(x){numeric_to_free(x)}

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
      co <- c(elements(coeffs(x)),value)
      w <- c(elements(words(x)),list(numeric(0)))
    }
  freealg(w,co)
}

`is.freealg` <- function(x){inherits(x,"freealg")}

`is_ok_free` <- function(words,coeffs){
    if(is.disord(words) | is.disord(coeffs)){
        stopifnot(disordR::consistent(words,coeffs))
    }
    
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
  SHRT_MAX <- 32767
  SHRT_MAXo2 <- round(SHRT_MAX/2)
  

  if(isTRUE(getOption("usecaret"))){
      symbols <- c(letters,paste(letters,"^-1",sep=""))
  } else {
      symbols <- c(letters,LETTERS)
  }
  n <- 26
  
  out <- ""
  w <- elements(words(x))
  eco <- elements(coeffs(x))
  for(i in seq_along(w)){
    co <- eco[i]
    if(co>0){
      pm <- " + " # pm = plus or minus
    } else {
      pm <- " - "
    }
    co <- capture.output(cat(abs(co)))
    jj <- w[i][[1]]
    if(length(jj)>0){mulsym <- "*"} else {mulsym <- ""}
    if(any(jj<0)){jj[jj<0] <- n-jj[jj<0]}
    ss <- symbols[jj]
    wanted <- jj>SHRT_MAX
    if(any(wanted)){# (da)
        ss[wanted] <- paste("(d",symbols[jj[wanted]-SHRT_MAX],")",sep="")
    }

    wanted <- (jj>SHRT_MAXo2) & (jj<SHRT_MAX) # (dA)
    if(any(wanted)){
        ss[wanted] <- paste("(d",symbols[26-jj[wanted]+SHRT_MAX],")",sep="")
    }

    ss <- paste(ss,collapse="")
    out <- paste(out, pm, co, mulsym, ss, sep="")
  }
  if(is.zero(x)){out <- "0"}
  cat(paste(strwrap(out, getOption("width")), collapse="\n"))
  cat("\n")
  return(x)
}

`vector_to_free` <- function(v,coeffs){
  if(missing(coeffs)){coeffs <- rep(1,length(v))}
  freealg(as.list(v),coeffs)
}

`string_to_freealg` <- function(string){
  string <- gsub("^\\+","",string)  # strip initial "+"
  string <- gsub("\\*","",string)   # strip all "*"
  minus <- length(grep("^-",string))>0
  if(minus){
    sign <- (-1)
    string <- gsub("^-","",string)
  } else {
    sign <- +1
  }

  if(length(grep("[0-9]",string))>0){
    coeff <- as.numeric(gsub("[a-z]|[A-Z]","",string))
    string <- gsub("[0-9]","",string)
  } else {
    coeff <- 1
  }
  out <- match(strsplit(string,"")[[1]], c(letters,LETTERS))
  if(any(out>26)){out[out>26] <- 26-out[out>26]}
  freealg(list(out),coeffs=sign*coeff)
}

`char_to_freealg` <- function(ch){ Reduce(`+`,lapply(ch,string_to_freealg))  }

`natural_char_to_freealg` <- function(string){
  string <- paste(string, collapse = " ")
  string <- gsub(" ","",string)  # strip spaces
  string <- gsub("\\+"," +",string) # 'A+B" -> "A +B"
  string <- gsub("\\-", " -",string) # "A-B" -> "A -B"
  char_to_freealg(strsplit(string," ")[[1]]) }

setGeneric("deriv")
`deriv.freealg` <- function(expr, r, ...){
    if(is.character(r)){
        rn <- numeric(length(r))
        if(length(r)==1){r <- strsplit(r,"")[[1]]}
        rn <- sapply(r,function(x){which(x==c(letters,LETTERS))}) # a=1,b=2...
        wanted <- rn>26
        rn[wanted] <- 26-rn[wanted]  # A=-1,B=-2,...
    } else {
        rn <- r
    }
    
    jj <- lowlevel_diffn(expr[[1]],expr[[2]],rn)
    return(freealg(jj[[1]],jj[[2]]))
}

`horner` <- function(P, v){
  P <- as.freealg(P)
  Reduce(v, right=TRUE, f=function(a,b){b*P + a})
}

`subsu` <- function(S1,S2,r){
    S1 <- as.freealg(S1)
    S2 <- as.freealg(S2)
    if(is.character(r) & (nchar(r)==1)){r <- which(letters==r)}
    out <- lowlevel_subs(S1[[1]],S1[[2]],S2[[1]],S2[[2]],as.integer(round(r[1])))
    freealg(out[[1]],out[[2]])
}

`subs` <- function(S, ...){
    sb  <- list(...)
    v <- names(sb)
    out <- S
    for (i in seq_along(sb)) {
      out <- subsu(out, sb[[i]],v[i])
    }
    return(out)
}

`linear` <- function(x,power=1){
    a <- seq_along(x)
    jj <- cbind(a,power)
    freealg(sapply(a,function(i){rep(jj[i,1],jj[i,2])},simplify=FALSE),x)
}

`pepper` <- function(v){
    if(is.character(v)){
        v <- match(unlist(strsplit(v,"")),letters)
    }
    mv <- partitions::multiset(v)
    freealg(split(mv,col(mv)),rep(1,ncol(mv)))
}

`degree` <- function(x,n){
    if(missing(n)){stop("argument 'n' missing ... maybe you meant degrees()?")}
    coeffs(x)[!(degrees(x) %in% n)] <- 0
    return(x)
}

`degrees` <- function(x){
    disord(unlist(lapply(words(x),length)),hashcal(x))
}

`degree<-` <- function(x, n, value){
    coeffs(x)[degrees(x) %in% n] <- value
    return(x)
}
