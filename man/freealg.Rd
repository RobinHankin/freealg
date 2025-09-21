\name{freealg}
\alias{freealg}
\alias{is.freealg}
\alias{as.freealg}
\alias{is_ok_free}
\alias{char_to_freealg}
\alias{natural_char_to_freealg}
\alias{numeric_to_free}
\alias{vector_to_free}
\alias{string_to_freealg}
\title{The free algebra}
\description{Create, test for, and coerce to, \code{freealg} objects}
\usage{
freealg(words, coeffs)
is_ok_free(words,coeffs)
is.freealg(x)
as.freealg(x,...)
char_to_freealg(ch)
natural_char_to_freealg(string)
string_to_freealg(string)
vector_to_free(v,coeffs)
}
\arguments{
  \item{words}{Terms of the algebra object, eg \code{c(1,2,-1,-3,-2)}
    corresponds to \code{abACB} because \eqn{a=1}, \eqn{b=2} etc;
    uppercase, or negative number, means inverse}
  \item{coeffs}{Numeric vector corresponding to the coefficients of each
    element of the \code{word} list}
  \item{string}{Character string}
  \item{ch}{Character vector}
  \item{v}{Vector of integers}
  \item{x}{Object possibly of class \code{freealg}}
  \item{...}{Further arguments, passed to the methods}
}
\details{

Function \code{freealg()} is the formal creation mechanism for
\code{freealg} objects.  However, it is not very user-friendly; it is
better to use \code{as.freealg()} in day-to-day use (although it does
use heuristics for the coefficients if not supplied).

Low-level helper function \code{is_ok_freealg()} checks for consistency
of its arguments.

A \code{freealg} object is a two-element list.  The first element is a
list of integer vectors representing the indices and the second is a
numeric vector of coefficients.  Thus, for example:

\preformatted{
> as.freealg("a+4bd+3abbbbc")
free algebra element algebraically equal to
 + 1*a + 3*abbbbc + 4*bd
> dput(as.freealg("a+4bd+3abbbbc"))
structure(list(indices = list(1L, c(1L, 2L, 2L, 2L, 2L, 3L), 
    c(2L, 4L)), coeffs = c(1, 3, 4)), class = "freealg")
}

Observe that the order of the terms is not preserved and indeed is
undefined (implementation-specific).  Zero entries are stripped out.

Character strings may be coerced to \code{freealg} objects;
\code{as.freealg()} calls \code{natural_char_to_freealg()}, which is
user-friendly.  Functions \code{char_to_freealg()} and
\code{string_to_freealg()} are low-level helper functions.  These
functions assume that upper-case letters are the multiplicative inverses
of the lower-case equivalents; so for example \code{as.freealg("aA")}
and \code{as.freealg(aBcCbA)} evaluate to one.  This can be confusing
with the default print method.

}

\author{Robin K. S. Hankin}

\note{
  Internally, the package uses signed integers and as such can have
  \code{.Machine$integer.max} different symbols; on my machine this is
  2147483647.  Of course the print method cannot deal with this as it
  only has 26 symbols for letters a-z (and A-Z for the inverses), but
  the objects themselves do not care about the print method.  Note also
  that the experimental calculus facility (as per \code{deriv()})
  reserves numbers in the range \code{SHRT_MAX}\eqn{{}\pm r}{+/-r} for
  infinitesimals, where \code{r} is the integer for a symbol.  This
  system might change in the future.  }
\examples{

freealg(list(1:2, 2:1, numeric(0), 1:6), 1:4)
freealg(list(1:2, 2:1, numeric(0), 1:6)) # heuristics for coeffs: assume 1

freealg(sapply(1:5, seq_len), 1:5)

freealg(replicate(5, sample(-5:5,rgeom(1,1/5),replace=TRUE)), 1:5)

as.freealg("1+xaX")^5

}
