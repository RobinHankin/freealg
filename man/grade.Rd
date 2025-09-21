\name{grade}
\alias{grade}
\alias{grade<-}
\alias{grades}
\alias{degree}
\alias{deg}
\alias{degrees}
\title{The grade (or degree) of terms in a \code{freealg} object
}
\description{

The free algebra \eqn{\mathcal B}{B} is a \dfn{graded} algebra: that
is, for each integer \eqn{n\geqslant 0}{n>=0} there is a homogeneous
subspace \eqn{\mathcal{B}_n}{B_n} with
\eqn{\mathcal{B}_0=\mathcal{R}}{B_0=R} and

\deqn{
  \mathcal{B}=\bigoplus_{n=0}^\infty\mathcal{B}_n,\quad\mbox{and}\quad\mathcal{B}_n\mathcal{B}_m\subseteq\mathcal{B}_{n+m}\quad\mbox{for all $m,n\geqslant  0.$}
}{omitted: see PDF}
  
The elements of \eqn{\cup_{n\geqslant 0}\mathcal{B}_n}{omitted} are
called \dfn{homogeneous} and those of \eqn{\mathcal{B}_n}{B_n} are
called homogeneous of degree (or grade) \eqn{n}.

The \dfn{grade} of a term is the number of symbols in it.  Thus the
grade of \code{xxx} and \code{4xxy} is 3; the grade of a constant is
zero.  Because the terms are stored in an implementation-specific way,
the grade of a multi-term object is a \code{disord} object.

The grade of the zero \code{freealg} object,
\code{grade(as.freealg(0))}, is defined to be \eqn{-\infty}{omitted},
as per Knuth [TAOCP, volume 2, p436].  This ensures that
\code{max(grades(abelianize(x))) <= max(grades(x))} is always satisfied.
However, a case for \code{NULL} could be made.

}
\usage{
grades(x)
grade(x, n, drop=FALSE)
grade(x, n) <- value
deg(x)
}
\arguments{
  \item{x}{Freealg object}
  \item{n}{Integer vector}
  \item{value}{Replacement value, a numeric vector}
  \item{drop}{Boolean, indicating whether or not to return a
    \code{drop}ped value}
}
\details{

  \code{grades(x)} returns the grade (number of symbols) in each term
  of a \code{freealg} object \code{x}.

  \code{deg(x)} returns the maximum of the grades of each symbol of
  \code{x}; \code{max(grades(x))}.
  
  \code{grade(x,n)} returns the freealg object comprising terms with
  grade \code{n} (which may be a vector).  Note that this function is
  considerably less efficient than \code{clifford::grade()}.

  The \code{drop} argument to \code{grade()} only makes a difference if
  either the result is zero, or that \code{n=0} (that is, extracting the
  constant).  It defaults to \code{FALSE} on the grounds that
  \code{constant()} returns a \code{drop}ped value.

  \code{grade(x,n) <- value} sets the coefficients of terms with grade
  \code{n}.  For \code{value}, a length-one numeric vector is accepted
  (notably zero, which kills terms of grade \code{n}) and also a
  \code{freealg} object comprising terms of grade \code{n}.
  
}
\value{Returns a disord object}
\references{
H. Munthe-Kaas and B. Owren 1999.  \dQuote{Computations in a free Lie
  algebra}, \emph{Phil. Trans. R. Soc. Lond. A}, 357:957--981 (theorem 3.8)
}
\author{Robin K. S. Hankin}
\note{The \pkg{clifford} package includes a similar concept, also called
  \dfn{grade}.}
\examples{


X <- as.freealg("1 -x + 5*y + 6*x*y -8*x*x*x*x*y*x")
X
grades(X)

a <- rfalg(30)
a
grades(a)
grade(a, 2)
grade(a, 2) <- 0 # kill all grade-2 terms
a

grade(a, 1) <- grade(a, 1) * 888 
a
}
