\name{inverse}
\alias{inverse}
\alias{all_pos}
\alias{keep_pos}
\title{Inverses}
\description{Multiplicative inverses of symbols in the free algebra}
\usage{
all_pos(x)
keep_pos(x)
}
\arguments{
  \item{x}{Freealg object}
}
\details{

  Function \code{all_pos()} tests for its argument having only positive
  powers; function \code{keep_pos()} discards any term with a negative
  power.
  
  At various points in the package, it is assumed that upper-case
  letters are the multiplicative inverses of the lower-case equivalents;
  so for example \code{as.freealg("aA")} and \code{as.freealg("aBcCbA")}
  evaluate to one.  This can be confusing with the default print method.

  Even though individual symbols have multiplicative inverses, a general
  element of the free algebra will not have a multiplicative inverse.
  For example, \code{1+x} does not have an inverse.  The free algebra is
  not a division algebra, in general.
  
}
\author{Robin K. S. Hankin}
\examples{

all_pos(rfalg(include.negative = TRUE))
all_pos(rfalg(include.negative = FALSE))


as.freealg("1+xaX")^5



}
