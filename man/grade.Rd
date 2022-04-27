\name{grade}
\alias{grade}
\alias{grade<-}
\alias{grades}
\alias{degree}
\alias{degrees}
\title{The grade (or degree) of terms in a \code{freealg} object
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
called homogenous of degree (or grade) \mjseqn{n}.

The grade of a term is the number of symbols in it.  Thus the grade of
\code{xxx} and \code{4xxy} is 3; the grade of a constant is zero.
Because the terms are stored in an implementation-specific way, the
grade of a multi-term object is a \code{disord} object.

}
\usage{
grades(x)
grade(x,n)
grade(x,n) <- value
}
\arguments{
  \item{x}{Freealg object}
  \item{n}{Integer vector}
  \item{value}{Replacement value, a numeric vector}
}
\details{

  \code{grades(x)} returns the grade (number of symbols) in each term
  of a \code{freealg} object \code{x}.

  \code{grade(x,n)} returns the freealg object comprising terms with
  grade \code{n} (which may be a vector).  Note that this function is
  considerably less efficient than \code{clifford::grade()}.

  \code{grade(x,n) <- value} sets the coefficients of terms with grade
  \code{n}.  For \code{value}, a length-one numeric vector is accepted
  (notably zero, which kills terms of grade \code{n}) and also a
  \code{freealg} object comprising terms of grade code{n}.
  
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


X <- as.freealg("1 -x + 5*y + 6*x*y -8*x*x*x*x*y*x")
X
grades(X)

a <- rfalg(30)
a
grades(a)
grade(a,2)
grade(a,2) <- 0 # kill all grade-2 terms
a

grade(a,1) <- grade(a,1) * 888 
a
}