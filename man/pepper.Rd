\name{pepper}
\alias{pepper}
\title{Combine variables in every possible order}
\description{
  Given a list of variables, construct every term comprising only those
  variables; function \code{pepper()} returns a free algebra object
  equal to the sum of these terms.

  The function is named for a query from an exam question set by Sarah
  Marshall in which she asked how many ways there are to arrange the
  letters of word \dQuote{pepper}, the answer being \eqn{\left({6\atop
  1\,2\,3}\right)=\frac{6!}{1!2!3!}=60}{6!/(1!2!3!)=60}.

  Function \code{multiset()} in the \CRANpkg{partitions} package gives
  related functionality; for the record, one way to reproduce
  \code{pepper("pepper")} would be

  \preformatted{
    apply(matrix(c("p","e","r")[multiset(c(1,1,1,2,2,3))],nrow=6),2,paste,collapse="")
  }

  
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
pepper(c(1, 1, 1, 1, 1, 1, 2))  # 6 a's and 1 b
pepper(c(1, 2, 2, 2, 3))        # 1 a, 3 b's and 1 c
pepper("pepper")
}
\keyword{symbolmath}
