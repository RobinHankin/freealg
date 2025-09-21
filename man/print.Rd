\name{print}
\alias{print.freealg}
\title{Print freealg objects}
\description{
Print methods for free algebra objects.  The indeterminates are
represented using lowercase letters a-z (currently hard coded).
}
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

The print method uses lowercase letters a-z to represent the
indeterminates; this is currently hard coded:

\preformatted{
> (x <- as.freealg("6abbbc + 7cax"))
free algebra element algebraically equal to
+ 6*abbbc + 7*cax
> unclass(x)
$indices
$indices[[1]]
[1] 1 2 2 2 3

$indices[[2]]
[1]  3  1 24


$coeffs
[1] 6 7
}

The print method has special dispensation for length-zero freealg objects
but these are not handled entirely consistently.

The print method is sensitive to the value of
\code{getOption("usecaret")}, defaulting to \dQuote{FALSE}.  The default
is to use uppercase letters to represent multiplicative inverses.  Thus,
the inverse of \code{a} appears as either \dQuote{\code{a^-1}} if
\code{usecaret} is \code{TRUE}, and \dQuote{\code{A}} if \code{FALSE}.
Carets become cumbersome for powers above the first.  For example, the
default notation for \eqn{aba^{-2}}{aba^-2} is \code{abAA} but becomes
\code{aba^-1a^-1} if \code{usecaret} is \code{TRUE}.

The symbols for the indeterminates are currently hardcoded as
\code{c(letters, LETTERS)}.  The intent is to be able to signify 52
distinct indeterminates, \code{a-z, A-Z}.  This works fine if option
\code{usecaret} is \code{TRUE}.  But if option \code{usecaret} is
\code{FALSE}, this can be confusing: for example, indeterminate number 1
appears as \code{a}, and its inverse would appear as \dQuote{\code{A}}.
But indeterminate number 27 also appears as \dQuote{\code{A}}.  They
look the same, but no warning is given: caveat emptor!

The method is also sensitive to \code{getOption("mulsym")}, defaulting
to \code{NULL}.  This is the multiplication symbol used between the
coefficient and the indeterminate string.  Sometimes an asterisk,
\code{*} or a space, might be useful.  If \code{mulsym} takes its
default of \code{NULL} [or a length zero string], the print method
suppresses coefficients of \eqn{\pm 1}.

Integers exceeding \code{SHRT_MAX} are reserved for infinitesimals,
which are printed as \dQuote{\code{da}}; see the note at \code{deriv.Rd}
for details.

}
\seealso{\code{\link{freealg}},\code{\link{deriv}}}
\examples{

rfalg()

x <- rfalg(inc=TRUE)
x                           # default
options("usecaret" = TRUE)  # use caret
x
options("usecaret" = FALSE) # back to the default
x


x <- freealg(list(5,1:4,3,8,7), c(1,1,1,3,22))
x


options(mulsym = "*")
x
options(mulsym = NULL)  # restore default

}
