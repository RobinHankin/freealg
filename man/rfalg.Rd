\name{rfalg}
\alias{rfalg}
\alias{rfalgg}
\alias{rfalggg}
\alias{rfreealg}
\alias{rfree}
\title{Random free algebra objects}
\description{
  Generate random elements of the free algebra, intended as quick
  \dQuote{get you going} examples of \code{freealg} objects
}
\usage{
rfalg(n=7, distinct=3, maxsize=4, include.negative=FALSE)
rfalgg(n=30, distinct=8, maxsize=7, include.negative=FALSE)
rfalggg(n=100, distinct=26, maxsize=30, include.negative=FALSE)
}
\arguments{
  \item{n}{Number of terms to generate}
  \item{distinct}{Number of distinct symbols to use}
  \item{maxsize}{Maximum number of symbols in any word}
  \item{include.negative}{Boolean, with default \code{FALSE} meaning to
    use only positive symbols (lower-case letters) and \code{TRUE}
    meaning to use upper-case letters as well, corresponding to the
    inverse of the lower-case symbols}
}
\details{
  Terms are automatically simplified; for example, 
  \code{aaBaAbaC} will be simplified to \code{aaaC}.

  Functions \code{rfalgg()} and \code{rfalggg()} return successively
  more complicated \code{freealg} objects.

}
\author{Robin K. S. Hankin}
\examples{
rfalg()
rfalg(include.negative = TRUE)^2

constant(rfalg())

}
