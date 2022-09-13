\name{abelianize}
\alias{abelianize}
\title{Abelianize a \code{freealg} object}
\description{

Function \code{abelianize()} returns a \code{freealg} object that is
equivalent to its argument under assumption of Abelianness.  The symbols
are placed in alphabetical order.
}
\usage{abelianize(x)}
\arguments{\item{x}{A \code{freealg} object}}
\details{
Abelianizing a free group element means that the symbols can commute
past one another.  Abelianization is vectorized.
}
\value{Returns an object of class \code{freealg}.}
\author{Robin K. S. Hankin}
\note{There is a very similar function in the \pkg{freegroup} package.}
\examples{
(x <- rfalg())
abelianize(x)

.[rfalg(),rfalg()]

}
