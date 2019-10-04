## This file follows the structure of aaa.R in the free group package.

## Tests of subs() here.

test_that("Test suite aac.R",{

  checker1 <- function(x){
    expect_true(deriv(x,c(1,2)) == deriv(x,c(2,1)))
    expect_true(deriv(x,c(1,2)) == deriv(x,c(2,1)))

    expect_true(deriv(x,c(1,2,3)) == deriv(x,c(1,2,3)))
    expect_true(deriv(x,c(1,2,3)) == deriv(x,c(1,3,2)))
    expect_true(deriv(x,c(1,2,3)) == deriv(x,c(2,1,3)))
    expect_true(deriv(x,c(1,2,3)) == deriv(x,c(2,3,1)))
    expect_true(deriv(x,c(1,2,3)) == deriv(x,c(3,1,2)))
    expect_true(deriv(x,c(1,2,3)) == deriv(x,c(3,2,1)))

    return(TRUE)
  }  # checker1() closes

  checker1r <- function(x,r){

    f <- function(x){deriv(x,r)}

    expect_true(f(x*2) == 2*f(x))
    expect_true(f(x^2) == f(x)*x + x*f(x))
    expect_true(f(x^3) == f(x)*x*x + x*f(x)*x + x*x*f(x) )
    expect_true(f(x^4) == f(x)*x*x*x + x*f(x)*x*x + x*x*f(x)*x + x*x*x*f(x))
    
  return(TRUE)
}  # checker1r() closes


checker2 <- function(x,y){

  expect_true(deriv(2*x+3*y,1) == 2*deriv(x,1) + 3*deriv(y,1))

  expect_true(deriv(x*y,1) == deriv(x,1)*y + x*deriv(y,1))
  expect_true(deriv(x*y,2) == deriv(x,2)*y + x*deriv(y,2))

  expect_true(deriv(x*y,c(1,2)) ==
              deriv(x,c(1,2))*       y         +
              deriv(x,c(1  ))* deriv(y,c(  2)) +
              deriv(x,c(  2))* deriv(y,c(1  )) +
                    x        * deriv(y,c(1,2))
              )

  return(TRUE)
}

checker3 <- function(x,y,z){
  expect_true(deriv(x*y*z,1) ==
              x*y*deriv(z,1) +
              x*deriv(y,1)*z +
              deriv(x,1)*y*z
              )
  
  return(TRUE)
} # checker3() closes


for(i in 1:2){
  for(inc in c(TRUE,FALSE)){
      x <- rfalg(5,include.negative=inc)
      y <- rfalg(5,include.negative=inc)
      z <- rfalg(5,include.negative=inc)
      checker1(x)
      checker2(x,y)
      checker3(x,y,z)
    for(r in 1:3){
      checker1r(x,r)
    }  # r loop closes
  } # inc loop closes
} # i loop closes

})  # test_that() function closes
