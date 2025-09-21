\name{linear}
\alias{linear}
\title{Simple free algebra objects}
\description{
  Create simple free algebra objects including linear expressions.  For
  example:

\preformatted{
> linear(1:3)
free algebra element algebraically equal to
+ 1*a + 2*b + 3*c
> linear(1:3, power=5)
free algebra element algebraically equal to
+ 1*aaaaa + 2*bbbbb + 3*ccccc
>
}

}
\usage{
linear(x, power=1)
}
\arguments{
  \item{x}{Numeric vector of terms}
  \item{power}{Integer vector of powers}
}
\author{Robin K. S. Hankin}
\note{

  It is instructive to compare the functionality documented here with
  their \CRANpkg{mvp} equivalents.  Many of the functions documented at
  \code{mvp::special.Rd} do not make sense in the context of the free
  algebra.  Function \code{mvp::product()}, for example, imposes an
  order on the expansion.

  Function \code{constant()} is documented at \code{constant.Rd}, but is
  listed below for convenience.

}
\seealso{\code{\link{constant}}, \code{\link{zero}}}
\examples{
linear(1:3)         
linear(1:3, power=5)
linear(1:3, power=3:1)
}
\keyword{symbolmath}
