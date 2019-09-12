## This file follows the structure of aaa.R in the free group package.

## Define some checker functions, and call them at the end.  They
## should all return TRUE if the package works, and stop with error if
## a test is failed.  Function checker1() has one argument, checker2()
## two, and checker3() has three.  

test_that("Test suite aaa.R",{

checker1 <- function(x){
    expect_true(x==x, info=dput(x))

    expect_true(x == x + constant(0), info=dput(x))
    expect_true(x == x + 0, info=dput(x))
    expect_true(x == (x + 4) -4, info=dput(x))
    expect_true(x == -(-x), info=dput(x))
    expect_true(x == +(+x), info=dput(x))

    expect_true(x+x-x == x, info=dput(x))

    expect_true(is.zero(x-x), info=dput(x))

    expect_true(0*x == constant(0), info=dput(x))
    expect_true(1*x == x, info=dput(x))
    expect_true(2*x == x+x, info=dput(x))
    expect_true(3*x == x+x+x, info=dput(x))
    expect_true(4*x == x+x+x+x, info=dput(x))
    expect_true(5*x == x+x+x+x+x, info=dput(x))
    expect_true(6*x == x+x+x+x+x+x, info=dput(x))

    expect_true(x^0 == constant(1), info=dput(x))
    expect_true(x^1 == x, info=dput(x))
    expect_true(x^2 == x*x, info=dput(x))
    expect_true(x^3 == x*x*x, info=dput(x))
    expect_true(x^4 == x*x*x*x, info=dput(x))
    
    ## check constant() and constant<-():
    ## checks below include 
    y <- x
    expect_true(constant(x) == constant(y), info=dput(x))
    constant(y) <- 4
    expect_true(constant(y) == 4, info=dput(x))
    constant(y) <- 0
    expect_true(constant(y) == 0, info=dput(x))

  

  return(TRUE)
}  # checker1() closes


checker2 <- function(x,y){
  expect_true(x == -y+x+y)
  expect_true(x+y == x-(-y))

  expect_true(x+y == y+x)

  expect_true((-x)*y == -(x*y))
  expect_true(x*(-y) == -(x*y))

  ##  expect_true(x*y == y*x)  
  return(TRUE)
}

checker3 <- function(x,y,z){
  expect_true(x+(y+z) == (x+y)+z) # associativity
  expect_true(x*(y*z) == (x*y)*z) # associativity

  expect_true(x*(y+z) == x*y + x*z)  # distributivity
  expect_true((y+z)*x == y*x + z*x)  # distributivity
  


  return(TRUE)
} # checker3() closes


for(i in 1:2){
    x <- rfalg(5)
    y <- rfalg(5)
    z <- rfalg(5)
    
    checker1(x)
    checker2(x,y)
    checker3(x,y,z)
}
})
