\name{Ops.freealg}
\alias{Ops.freealg}
\alias{ops}
\alias{free_negative}
\alias{freealg_negative}
\alias{free_times_free}
\alias{free_times_scalar}
\alias{free_plus_free}
\alias{free_plus_numeric}
\alias{free_power_scalar}
\alias{free_eq_free}
\title{Arithmetic Ops methods for the the free algebra}
\description{
  Allows arithmetic operators to be used for manipulation of freealg objects
  such as addition, multiplication, powers, etc
}
\usage{
\method{Ops}{freealg}(e1, e2)
free_negative(S)
free_power_scalar(S,n)
free_eq_free(e1,e2)
free_plus_numeric(S,x)
free_plus_free(e1,e2)
}
\arguments{
  \item{S,e1,e2}{Objects of class \code{freealg}}
  \item{n}{An integer, possibly non-positive}
  \item{x}{Scalar value}
}
\details{

  The function \code{Ops.freealg()} passes binary arithmetic operators
  (\dQuote{\code{+}}, \dQuote{\code{-}}, \dQuote{\code{*}},
  \dQuote{\code{^}}, and \dQuote{\code{==}}) to the appropriate
  specialist function.

  The caret, as in \code{a^n}, denotes arithmetic exponentiation, as in
  \code{x^3==x*x*x}.
}
\author{Robin K. S. Hankin}
\examples{
rfalg()
as.freealg("1+x+xy+yx")  # variables are non-commutative
as.freealg("x") * as.freealg("X") # upper-case letters are lower-case inverses

constant(as.freealg("x+y+X+Y")^6)  # OEIS sequence A035610
}
