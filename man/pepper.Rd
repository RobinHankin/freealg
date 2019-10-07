\name{pepper}
\alias{pepper}
\title{Combine variables in every possible order}
\description{
  Given a list of variables, return a free algebra object corresponding
  the sum terms which have those variables combined in every possible
  order.  
}
\usage{
pepper(v)
}
\arguments{
  \item{v}{Variables to combine.  If a character string, coerce to
    variable numbers}
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{linear}}}
\examples{
pepper(c(1,2,2,2,3))
pepper("pepper")
}
\keyword{symbolmath}
