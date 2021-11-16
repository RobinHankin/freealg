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

  operates even if (as here) \eqn{u,v} do not commute.

  A term of a \code{freealg} object can include negative values which
  correspond to negative powers of variables.  Thus:
\preformatted{
  > deriv(as.freealg("aaaa"),1)   #   d(a^4)/da = 4a^3
free algebra element algebraically equal to
+ 4*aaa


> deriv(as.freealg("A"),1)       # d(a^-1)/da = -a^-2
free algebra element algebraically equal to
 - 1*AA
}

(see also the examples).  Vector \code{r} may include negative integers
which mean to differentiate with respect to the inverse of the variable:


\preformatted{
> deriv(as.freealg("AAAA"),-1)    # d(a^-4)/d(a^-1) = 4a^-3
free algebra element algebraically equal to
 + 4*AAA
> deriv(as.freealg("aaa"),-1)     # d(a^3)/d(a^-1) = 3a^4
free algebra element algebraically equal to
 - 3*aaaa
> 
}


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
