\name{letters}
\alias{letters}
\alias{lettersymbols}
\title{Single-letter symbols}
\description{
  Variables \code{a}, \code{b},\ldots, \code{z} and their inverses
  \code{A}-\code{Z} are given their \code{freealg} semantic meaning.
}

\details{

Sometimes it is convenient in an \R session to have all 26 letters a-z
and all 26 uppercase letters A-Z adopt their free algebra
interpretations.  To access this, load the \code{lettersymbols} dataset,
which is provided with the package in the \code{inst} directory:

\preformatted{
  load(system.file("lettersymbols.rda",package="freealg"))
}

  
Executing this allows you to do cool things such as the
following:

\preformatted{
> (1+a-b^2)^4
free algebra element algebraically equal to
+ 1 + 4a + 6aa + 4aaa + aaaa - aaabb - 4aabb - aabba + aabbbb - 6abb - 4abba -
abbaa + abbabb + 4abbbb + abbbba - abbbbbb - 4bb - 6bba - 4bbaa - bbaaa +
bbaabb + 4bbabb + bbabba - bbabbbb + 6bbbb + 4bbbba + bbbbaa - bbbbabb -
4bbbbbb - bbbbbba + bbbbbbbb
> 
}

Lowercase letters \code{c}, \code{q}, \code{t}, and uppercase letters
\code{C}, \code{D}, \code{F}, \code{I}, \code{T} might pose
difficulties.
  
These objects can also be generated by running script
\code{inst/symb.Rmd}, which includes some further discussion and
technical documentation and creates file \code{lettersymbols.rda} which
formerly resided in the \code{data/} directory.

}
\author{Robin K. S. Hankin}
\keyword{datasets}

