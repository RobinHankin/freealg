\name{horner}
\alias{horner}
\title{Horner's method}
\description{Horner's method for multivariate polynomials}
\usage{
horner(P,v)
}
\arguments{
  \item{P}{Free algebra polynomial}
  \item{v}{Numeric vector of coefficients}
}
\details{

  This function is (almost) the same as \code{mvp::horner()}.

  Given a polynomial

  \deqn{p(x) = a_0 +a_1+a_2x^2+\cdots + a_nx^n}{omitted; see PDF}

  it is possible to express \eqn{p(x)} in the algebraically equivalent
  form

  \deqn{p(x) = a_0 + x\left(a_1+x\left(a_2+\cdots + x\left(a_{n-1} +xa_n
\right)\cdots\right)\right)}{omitted; see PDF}

which is much more efficient for evaluation, as it requires only \eqn{n}
multiplications and \eqn{n} additions, and this is optimal.  Function
\code{horner()} will take a \code{freealg} object for its first
argument.

}
  
\author{Robin K. S. Hankin}
\examples{
horner("x+y",1:3) # note presence of xy and yx terms

horner("1+x+xyX",1:3)
}


\keyword{symbolmath}
