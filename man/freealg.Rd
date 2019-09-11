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
\description{Create, test for, an coerce to, \code{freealg} objects}
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
  \item{words}{Terms of the algebra object, eg \code{[1,2,-1,3,2]}
    corresponds to \code{abACB} (uppercase, or negative number, means
    inverse)} 
  \item{coeffs}{Numeric vector corresponding to the coefficients to each
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
better to use \code{as.freealg()} in day-to-day use.

Function \code{is_ok_freealg()} checks for consistency of its arguments.

Function \code{char_to_freealg()} 


}
\author{Robin K. S. Hankin}
\examples{

freealg(sapply(1:5,seq_len),1:5)

freealg(replicate(5,sample(-5:5,rgeom(1,1/5),replace=TRUE)),1:5)

}