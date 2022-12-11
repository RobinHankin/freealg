\name{adjoint}
\alias{adjoint}
\alias{ad}
\title{The adjoint map}
\description{\loadmathjax

  The adjoint \mjeqn{\mathrm{ad}_X}{ad_X} of \mjseqn{X} is a map from a
  Lie group \mjseqn{G} to the endomorphism group of \mjseqn{G} defined
  by

  \mjdeqn{\mathrm{ad}_X(Y)=\left[X,Y\right]}{ad_X(Y)=[X,Y]}


}
\usage{
ad(x)
}
\arguments{
  \item{x}{Object nominally of class \code{freealg} but other classes
    accepted where they make sense}
}
\details{

details here


}
\author{Robin K. S. Hankin}
\note{Vignette \code{adjoint} gives more description}
\examples{

x <- rfalg()
y <- rfalg()

f <- ad(x)
f(y)


f(f(y)) # [x,[x,y]]

}
