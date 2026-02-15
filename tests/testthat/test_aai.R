## This file follows the structure of aaa.R in the free group package.
## Suite test_aai tests the power operator, as in rfalg() ^ as.freealg("z")

## Define some checker functions, and call them at the end.  They
## should all return TRUE if the package works, and stop with error if
## a test is failed.  Function checker1() has one argument and
## checker2() has two.



test_that("Test suite aai.R",{

checker1 <- function(x, TOL=1e-10){


    a <- as.freealg("a")*7
    b <- as.freealg("b")*pi

    expect_true(x^as.freealg(1) == x)

    infinite_precision <- FALSE
    if(infinite_precision){
        expect_true(x^a == inv(a) * x * a) # not true due to rounding error [of RHS!]
    }
    
    expect_true(max(abs(coeffs(x^a - inv(a) * x * a))) < TOL)
    
    expect_true(x^(a*b) == (x^a)^b)
}

checker2 <- function(x,y){

    a <- as.freealg("a")*7

    expect_true( (x^a) + (y^a) == (x+y)^a)
    expect_true( (x^a) * (y^a) == (x*y)^a)

}

for(i in 1:2){
  for(inc in c(TRUE, FALSE)){
    x <- rfalg(5, include.negative=inc)
    y <- rfalg(5, include.negative=inc)
    
    checker1(x)
    checker2(x,y)
  }
}

expect_error(as.freealg("a") ^ as.freealg("a+b"))


})
