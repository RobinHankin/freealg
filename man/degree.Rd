\name{degree}
\alias{degree}
\alias{degree<-}
\alias{degrees}
\title{The degree of terms in a \code{freealg} object
}
\description{\loadmathjax

The free algebra \mjeqn{\mathcal B}{B} is a \dfn{graded} algebra: that
is, for each integer \mjeqn{n\geq 0}{n>=0} there is a homogeneous
subspace \mjeqn{\mathcal{B}_n}{B_n} with
\mjeqn{\mathcal{B}_0=\mathcal{R}}{B_0=R} and

\mjdeqn{
  \mathcal{B}=\bigoplus_{n=0}^\infty\mathcal{B}_n,\quad\mbox{and}\quad\mathcal{B}_n\mathcal{B}_m\subseteq\mathcal{B}_{n+m}\quad\mbox{for all $m,n\geq  0.$}
}{omitted}
  
The elements of \mjeqn{\cup_{n\geq 0}\mathcal{B}_n}{omitted} are
called \dfn{homogeneous} and those of \mjeqn{\mathcal{B}_n}{B_n} are
called homogenous of degree \mjseqn{n}.

}
\usage{
degrees(x)
degree(x,n)
degree(x,n) <- value
}
\arguments{
  \item{x}{Freealg object}
  \item{n}{Integer vector}
  \item{value}{Replacement value, a numeric vector}
}
\details{

  \code{degrees(x)} returns the number of symbols in each term of a
  \code{freealg} object \code{x}.

  \code{degree(x,n)} returns the freealg object comprising terms with
  degree \code{n} (which may be a vector).

  \code{degree(x,n) <- value} sets the coefficients of terms with degree
  \code{n}.
  
}
\value{Returns a disord object}
\references{
H. Munthe-Kaas and B. Owren 1999.  \dQuote{Computations in a free Lie
  algebra}, \emph{Phil. Trans. R. Soc. Lond. A}, 357:957--981 (theorem 3.8)
}
\author{Robin K. S. Hankin}
\note{A similar concept \dfn{grade} is discussed in the \pkg{clifford}
package}
\examples{

a <- rfalg(20)
degrees(a)
degree(a,2)
degree(a,2) <- 0 # kill all degree-2 terms

}