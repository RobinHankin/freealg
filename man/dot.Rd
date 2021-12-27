\name{dot-class}
\docType{class}
\alias{dot}
\alias{.}
\alias{dot-class}
\alias{extract}
\alias{commutator}
\alias{jacobi}
\alias{[.dot}
\alias{[,dot-method}
\alias{[,dot,ANY,ANY-method}
\alias{[,dot,ANY,missing-method}
\alias{[,dot,missing,ANY-method}
\alias{[,dot,matrix,matrix-method}
\title{Class \dQuote{dot}}
\description{
  The dot object is defined so that idiom like \code{.[x,y]} returns the
  commutator, that is, \code{xy-yx}.

  The matrix method is there because \dQuote{\code{*}} returns
  (incorrectly, in my view) the elementwise product, not the matrix
  product. 
}
\arguments{
  \item{x}{Object of any class}
  \item{i,j}{elements to commute}
}
\value{
  Always returns an object of the same class as \code{xy}.
}
\author{Robin K. S. Hankin}
\examples{
x <- rfalg()
y <- rfalg()
z <- rfalg()

.[x,.[y,z]] + .[y,.[z,x]] + .[z,.[x,y]]  # Jacobi identity


}
