\name{deriv}
\alias{deriv}
\alias{aderiv}
\alias{deriv.freealg}
\alias{deriv_freealg}
\title{Differentiation of \code{freealg} objects}
\description{
  Differentiation of \code{freealg} objects
}
\usage{
\method{deriv}{freealg}(x, r, ...)
}
\arguments{
  \item{x}{Object of class \code{freealg}}
  \item{r}{Integer vector.  Elements denote variables to differentiate
    with respect to.  If \code{r} is a character vector, it is
    interpreted as \code{a=1,b=2,...,z=26}; if of length 1,
    \dQuote{\code{aab}} is interpreted as \code{c("a","a","b")}}
  \item{...}{Further arguments, currently ignored}
}
\author{Robin K. S. Hankin}
\details{

  Experimental function \code{deriv(S,v)} returns
      \eqn{\frac{\partial^r S}{\partial v_1\partial v_2\ldots\partial
      v_r}}{d^rS/dv1...dvr}.  The Leibniz product rule

  \deqn{\left(u\cdot v\right)'=uv'+u'v}{omitted; see latex}

  operates even if (as here) \eqn{u,v} do not commute.  For example, if
  we wish to differentiate \eqn{aaba} with respect to \eqn{a}, we would
  write \eqn{f(a) = aaba} and then

  \deqn{f(a+\delta a) = (a+\delta a)(a+\delta a)b(a+\delta a)}

  and working to first order we have

  \deqn{f(a+\delta a) -f(a)= (\delta a)aba + a(\delta a)ba + aab(\delta
  a).}  In the package:
  
  \preformatted{

    > deriv(as.freealg("aaba"),"a")
    free algebra element algebraically equal to
    + 1*aab(da) + 1*a(da)ba + 1*(da)aba
}

  A term of a \code{freealg} object can include negative values which
  correspond to negative powers of variables.  Thus:

  \preformatted{
    > deriv(as.freealg("AAAA"),"a")       
    free algebra element algebraically equal to
    - 1*AAAA(da)A - 1*AAA(da)AA - 1*AA(da)AAA - 1*A(da)AAAA
  }
  
  (see also the examples).  Vector \code{r} may include negative
  integers which mean to differentiate with respect to the inverse of
  the variable:

  \preformatted{
    > deriv(as.freealg("3abcbCC"),"C")
    free algebra element algebraically equal to
    + 3*abcbC(dC) + 3*abcb(dC)C - 3*abc(dC)cbCC
  }

  It is possible to perform repeated differentiation by passing a
  suitable value of \code{r}.  For
  \eqn{\frac{\partial^2}{\partial a\partial c}}{omitted}:

  \preformatted{
    > deriv(as.freealg("aaabAcx"),"ac")
    free algebra element algebraically equal to
    - 1*aaabA(da)A(dc)x + 1*aa(da)bA(dc)x + 1*a(da)abA(dc)x + 1*(da)aabA(dc)x
  }

  The infinitesimal indeterminates (\dQuote{\code{da}} etc) are
  represented by \code{SHRT_MAX+r}, where \code{r} is the integer for
  the symbol, and \code{SHRT_MAX} is the maximum short integer.  This
  includes negative \code{r}.  So the maximum number for any symbol is
  \code{SHRT_MAX}.  Inverse elements such as \code{A}, being represented
  by negative integers, have differentials that are \code{SHRT_MAX-r}.

  Function \code{deriv()} calls helper function \code{lowlevel_diffn()}
  which is documented at \code{Ops.freealg.Rd}.

  A vignette illustrating this concept and furnishing numerical
  verification of the code in the context of matrix algebra is given at
  \code{inst/freealg_matrix.Rmd}.

}
\examples{

deriv(as.freealg("4*aaaabaacAc"),1)

x <- rfalg()
deriv(x, 1:3)

y <- rfalg(7, 7, 17, TRUE)

deriv(y,1:5) - deriv(y,sample(1:5)) # should be zero

}
\keyword{symbolmath}
