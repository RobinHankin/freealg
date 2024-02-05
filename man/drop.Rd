\name{drop}
\alias{drop}
\alias{drop,freealg-method}
\title{Drop redundant information}
\description{Coerce constant free algebra objects to numeric}
\usage{drop(x)}
\arguments{\item{x}{Free algebra object}}
\details{
  If its argument is a constant freealg object, coerce to
  numeric.  Modelled on \code{base::drop()}.

}
\author{Robin K. S. Hankin}
\seealso{\code{\link{constant}},\code{\link{coeffs}}}
\note{A few functions in the package take \code{drop} as an argument
  which, if \code{TRUE}, means that the function returns a
  \code{drop}ped value.}
\examples{
drop(linear(1:5))
drop(4+linear(1:5)*0)
}
