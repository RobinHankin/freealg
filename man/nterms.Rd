\name{nterms}
\alias{nterms}
\alias{length}
\title{Number of terms in a freealg object}
\description{Number of terms in a freealg object; number of coefficients}
\usage{nterms(x)}
\arguments{\item{x}{Freealg object}}
\value{Returns a non-negative integer}
\author{Robin K. S. Hankin}
\examples{
(a <- freealg(list(1:3,4:7,1:10),1:3))
nterms(a)
nterms(a+1)
nterms(a*0)
}