The Free Algebra in R
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="man/figures/freealg.png" width = "150" align="right" />

<!-- badges: start -->

[![Total
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/freealg)](https://CRAN.R-project.org/package=freealg)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/freealg)](https://cran.r-project.org/package=freealg)
<!-- badges: end -->

# Overview

The free algebra is an interesting and useful object. Here I present the
`freealg` package which provides some functionality for free algebra in
the R programming environment. The package uses the `C++ map` class for
efficiency and conforms to `disordR` discipline. Several use-cases are
provided.

# Installation

You can install the released version of `freealg` from
[CRAN](https://CRAN.R-project.org) with:

``` r
# install.packages("freealg")  # uncomment this to install the package
library("freealg")
```

# The free algebra

The free algebra is the free R-module with a basis consisting of all
words over an alphabet of symbols with multiplication of words defined
as concatenation. Thus, with an alphabet of
![\\x,y,z\\](https://latex.codecogs.com/png.latex?%5C%7Bx%2Cy%2Cz%5C%7D "\{x,y,z\}")
and

![A=\alpha x^2yx + \beta zy](https://latex.codecogs.com/png.latex?A%3D%5Calpha%20x%5E2yx%20%2B%20%5Cbeta%20zy "A=\alpha x^2yx + \beta zy")

and

![B=\gamma z + \delta y^4](https://latex.codecogs.com/png.latex?B%3D%5Cgamma%20z%20%2B%20%5Cdelta%20y%5E4 "B=\gamma z + \delta y^4")

we would have

![AB=\left(\alpha x^2yx+\beta zy\right)\left(\gamma z+\delta y^4\right)=\alpha\gamma x^2yxz+\alpha\delta x^2yxy^4+\beta\gamma zyz+\beta\delta zy^5](https://latex.codecogs.com/png.latex?AB%3D%5Cleft%28%5Calpha%20x%5E2yx%2B%5Cbeta%20zy%5Cright%29%5Cleft%28%5Cgamma%20z%2B%5Cdelta%20y%5E4%5Cright%29%3D%5Calpha%5Cgamma%20x%5E2yxz%2B%5Calpha%5Cdelta%20x%5E2yxy%5E4%2B%5Cbeta%5Cgamma%20zyz%2B%5Cbeta%5Cdelta%20zy%5E5 "AB=\left(\alpha x^2yx+\beta zy\right)\left(\gamma z+\delta y^4\right)=\alpha\gamma x^2yxz+\alpha\delta x^2yxy^4+\beta\gamma zyz+\beta\delta zy^5")

and

![BA=\left(\gamma z+\delta y^4\right)\left(\alpha x^2yx+\beta zy\right)=\alpha\gamma zx^2yx + \alpha\delta y^4 x^2yx + \beta\gamma z^2y + \beta\delta y^4zy.](https://latex.codecogs.com/png.latex?BA%3D%5Cleft%28%5Cgamma%20z%2B%5Cdelta%20y%5E4%5Cright%29%5Cleft%28%5Calpha%20x%5E2yx%2B%5Cbeta%20zy%5Cright%29%3D%5Calpha%5Cgamma%20zx%5E2yx%20%2B%20%5Calpha%5Cdelta%20y%5E4%20x%5E2yx%20%2B%20%5Cbeta%5Cgamma%20z%5E2y%20%2B%20%5Cbeta%5Cdelta%20y%5E4zy. "BA=\left(\gamma z+\delta y^4\right)\left(\alpha x^2yx+\beta zy\right)=\alpha\gamma zx^2yx + \alpha\delta y^4 x^2yx + \beta\gamma z^2y + \beta\delta y^4zy.")

A natural and easily implemented extension is to use upper-case symbols
to represent multiplicative inverses of the lower-case equivalents
(formally we would use the presentation
![xX=1](https://latex.codecogs.com/png.latex?xX%3D1 "xX=1")). Thus if

![C=\epsilon\left(x^{-1}\right)^2=\epsilon X^2](https://latex.codecogs.com/png.latex?C%3D%5Cepsilon%5Cleft%28x%5E%7B-1%7D%5Cright%29%5E2%3D%5Cepsilon%20X%5E2 "C=\epsilon\left(x^{-1}\right)^2=\epsilon X^2")

we would have

![AC=\left(\alpha x^2yx+\beta zy\right)\epsilon X^2=
\alpha\epsilon x^2yX + \beta\epsilon zyX^2](https://latex.codecogs.com/png.latex?AC%3D%5Cleft%28%5Calpha%20x%5E2yx%2B%5Cbeta%20zy%5Cright%29%5Cepsilon%20X%5E2%3D%0A%5Calpha%5Cepsilon%20x%5E2yX%20%2B%20%5Cbeta%5Cepsilon%20zyX%5E2 "AC=\left(\alpha x^2yx+\beta zy\right)\epsilon X^2=
\alpha\epsilon x^2yX + \beta\epsilon zyX^2")

and

![CA=\epsilon X^2\left(\alpha x^2yx+\beta zy\right)=
\alpha\epsilon yx + \beta\epsilon X^2zy.](https://latex.codecogs.com/png.latex?CA%3D%5Cepsilon%20X%5E2%5Cleft%28%5Calpha%20x%5E2yx%2B%5Cbeta%20zy%5Cright%29%3D%0A%5Calpha%5Cepsilon%20yx%20%2B%20%5Cbeta%5Cepsilon%20X%5E2zy. "CA=\epsilon X^2\left(\alpha x^2yx+\beta zy\right)=
\alpha\epsilon yx + \beta\epsilon X^2zy.")

The system inherits associativity from associativity of concatenation,
and distributivity is assumed, but it is not commutative.

# The `freealg` package in use

Creating a free algebra object is straightforward. We can coerce from a
character string with natural idiom:

``` r
X <- as.freealg("1 + 3a + 5b + 5abba")
X
#> free algebra element algebraically equal to
#> + 1 + 3*a + 5*abba + 5*b
```

or use a more formal method:

``` r
freealg(sapply(1:5,seq_len),1:5)
#> free algebra element algebraically equal to
#> + a + 2*ab + 3*abc + 4*abcd + 5*abcde
```

``` r
Y <- as.freealg("6 - 4a +2aaab")
X+Y
#> free algebra element algebraically equal to
#> + 7 - a + 2*aaab + 5*abba + 5*b
X*Y
#> free algebra element algebraically equal to
#> + 6 + 14*a - 12*aa + 6*aaaab + 2*aaab + 30*abba - 20*abbaa + 10*abbaaaab + 30*b
#> - 20*ba + 10*baaab
X^2
#> free algebra element algebraically equal to
#> + 1 + 6*a + 9*aa + 15*aabba + 15*ab + 10*abba + 15*abbaa + 25*abbaabba +
#> 25*abbab + 10*b + 15*ba + 25*babba + 25*bb
```

We can demonstrate associativity (which is non-trivial):

``` r
set.seed(0)
(x1 <- rfalg(inc=TRUE))
#> free algebra element algebraically equal to
#> + 7*C + 6*Ca + 4*B + 3*BC + a + 5*aCBB + 2*bc
(x2 <- rfalg(inc=TRUE))
#> free algebra element algebraically equal to
#> + 6 + CAAA + 2*Ca + 3*Cbcb + 7*aaCA + 4*b + 5*c
(x3 <- rfalg(inc=TRUE))
#> free algebra element algebraically equal to
#> + 3*C + 5*CbAc + BACB + 2*a + 10*b + 7*cb
```

(function `rfalg()` generates random `freealg` objects). Then

``` r
x1*(x2*x3) == (x1*x2)*x3
#> [1] TRUE
```

# Further information

For more detail, see the package vignette

`vignette("freealg")`
