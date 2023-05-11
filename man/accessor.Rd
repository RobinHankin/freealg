\name{accessor}
\alias{accessor}
\alias{accessors}
\alias{words}
\alias{coeffs}
\alias{coeffs<-}
\alias{coeffs<-.freealg}
\alias{coefficients}
\title{Accessor methods for freealg objects}
\description{Accessor methods for free algebra objects}
\usage{
words(x)
coeffs(x)
coeffs(x) <- value
}
\arguments{
  \item{x}{Object of class \code{freealg}}
  \item{value}{Numeric vector of length 1}
}
\details{
Access or set the different parts of a \code{freealg} object.  The
constant term is technically a coefficient but is documented under
\code{constant.Rd}.
}
\author{Robin K. S. Hankin}
\note{

There is an extended discussion of this issue in the \code{mvp} object
at \code{accessor.Rd}.

}
\seealso{\code{\link{constant}}}
\examples{
a <- rfalg()
a
coeffs(a)
words(a)  # NB: hash is identical to that of coeffs(a)

coeffs(a) <- 7   # replacement methods work 
a
coeffs(a)  # 
}