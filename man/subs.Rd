\name{subs}
\alias{subs}
\alias{subsu}
\alias{substitute}
\alias{namechanger}
\title{Substitution}
\description{Substitute symbols in a \code{freealg} object for numbers or other
  \code{freealg} objects
}
\usage{
subs(...)
subsu(S1,S2,r)
}
\arguments{
  \item{S1,S2}{Objects of class \code{freealg}}
  \item{r}{Integer specifying symbol to substitute (\eqn{a=1,b=2} etc)}
  \item{\dots}{First element is a \code{freealg} object, remainder are
    named arguments corresponding to variables to substitute}
}
\value{
  Returns a \code{freealg} object.
}
\details{

Function \code{subs()} substitutes variables for \code{freealg} objects
(coerced if necessary) using natural \R idiom.  Observe that this type
of substitution is sensitive to order:

\preformatted{
> subs("ax",a="1+x",x="1+a")
free algebra element algebraically equal to
 + 2 + 3*a + 1*aa

> subs("ax",x="1+a",a="1+x")
free algebra element algebraically equal to
 + 2 + 3*x + 1*xx

}

Functions \code{subsu()} is a lower-level formal function, not really
intended for the end-user.  Function \code{subsu()} takes \code{S1} and
substitutes occurrences of symbol \code{r} with \code{S2}.

No equivalent to \code{mvp::subvec()} is currently implemented.

}
\author{Robin K. S. Hankin}
\note{

  Function \code{subs()} is one place in the package where the use of
  \code{letters} is effectively hard-wired in.  Idiom such as

  \code{subs("abccc",b="1+3x")}

  is very nice, but identifies \dQuote{b} with 2.  Note that argument
  \code{r} of \code{subsu()} is canonically an integer but a single
  character is interpreted as a \code{letter}.  See also the \code{note}
  at \code{freealg.Rd}.

}
\examples{
subs("abccc",b="1+3x")
subs("aaaa",a="1+x")  # binomial

subs("abA",b=31)

subs("1+a",a="A")   # can substitute for an inverse
subs("A",a="1+x")   # inverses are not substituted for

a <- rfalg()
subs(a,a=a)


## Sequential substitution works:

subs("abccc",b="1+3x",x="1+d+2e")
subs(rfalg(),a=rfalg())

}
\keyword{symbolmath}
