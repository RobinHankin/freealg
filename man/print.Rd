\name{print}
\alias{print.freealg}
\title{Print freealg objects}
\description{Print methods for free algebra objects}
\usage{
\method{print}{freealg}(x,...)
}
\arguments{
  \item{x}{Object of class \code{freealg} in the print method}
  \item{...}{Further arguments, currently ignored}
}
\author{Robin K. S. Hankin}
\note{

The print method does not change the internal representation of a
\code{freealg} object, which is a two-element list, the first of which
is a list of integer vectors representing words, and the second is a
numeric vector of coefficients.

The print method has special dispensation for length-zero freealg objects
but these are not handled entirely consistently.

}
\seealso{\code{\link{char_to_freealg}}}  
\examples{

rfalg()


}