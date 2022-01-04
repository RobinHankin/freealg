\name{deriv}
\alias{deriv}
\alias{aderiv}
\alias{deriv.freealg}
\alias{deriv_freealg}
\title{Differentiation of \code{freealg} objects}
\description{\loadmathjax
  Differentiation of \code{freealg} objects
}
\usage{
\method{deriv}{freealg}(expr, r, ...)
}
\arguments{
  \item{expr}{Object of class \code{freealg}}
  \item{r}{Integer vector.  Elements denote variables to differentiate
    with respect to}
  \item{...}{Further arguments, currently ignored}
}
\author{Robin K. S. Hankin}
\details{

  Function \code{deriv(S,v)} returns \mjeqn{\frac{\partial^r S}{\partial
      v_1\partial v_2\ldots\partial v_r}}{d^rS/dv1...dvr}.

  The Leibniz product rule

  \mjdeqn{\left(u\cdot v\right)'=uv'+u'v}{omitted; see latex}

  operates even if (as here) \eqn{u,v} do not commute.  For example, if
  we wish to differentiate \eqn{aaba} with respect to \eqn{a}, we would
  write \eqn{f(a) = aaba} and then

  \deqn{f(a+\delta a) = (a+\delta a)(a+\delta a)b(a+\delta a)}

  and working to first order we have

  \deqn{f(a+\delta a) -f(a)= (\delta a)aba + a(\delta a)ba + aab(\delta a).}

  A term of a \code{freealg} object can include negative values which
  correspond to negative powers of variables.  Thus:

  \preformatted{
    > deriv(as.freealg("aaaa"),1)
    free algebra element algebraically equal to
    + 1*aaa (da) + 1*aa (da) a + 1*a (da) aa + 1* (da) aaa
    > 

    > deriv(as.freealg("A"),1)       # d(a^-1)/da = -a^-2
    free algebra element algebraically equal to
    - 1*AA
  }
  
  (see also the examples).  Vector \code{r} may include negative
  integers which mean to differentiate with respect to the inverse of
  the variable:

  \preformatted{
    > deriv(as.freealg("AAAA"),-1)    # d(a^-4)/d(a^-1) = 4a^-3
    free algebra element algebraically equal to
    + 4*AAA
    > deriv(as.freealg("aaa"),-1)     # d(a^3)/d(a^-1) = 3a^4
    free algebra element algebraically equal to
    - 3*aaaa
    > 
  }

  The infinitesimal indeterminates (\dQuote{\code{da}} etc) are
  represented by \code{SHRT_MAX+r}, where \code{r} is the integer for
  the symbol, and \code{SHRT_MAX} is the maximum short integer.  So the
  maximum number for any symbol is \code{SHRT_MAX}.
  
  Function \code{deriv()} calls helper function \code{lowlevel_diffn()}
  which is documented at \code{Ops.freealg.Rd}.

}
\examples{

x <- rfalg()
deriv(x,1:3)

y <- rfalg(7,7,17,TRUE)

deriv(y,1:5)-deriv(y,sample(1:5)) # should be zero


}
\keyword{symbolmath}
