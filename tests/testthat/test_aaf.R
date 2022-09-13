## This file follows the structure of aaa.R in the free group package.

## Define some checker functions, and call them at the end.  They
## should all return TRUE if the package works, and stop with error if
## a test is failed.  Function checker1() has one argument, checker2()
## two, and checker3() has three.  

test_that("Test suite aaf.R",{

checker1 <- function(x){
    expect_error(.[])
    expect_error(.[x])
    expect_error(.[x,])
    expect_error(.[,x])

    expect_true(.[x,x] == 0)
    expect_true(.[x,x*3] == 0)

    return(TRUE)
}  # checker1() closes


checker2 <- function(x,y,n){
    expect_true(.[x,y] + .[y,x] == 0)
    expect_true(n*.[x,y] == .[n*x,y])
    expect_true(n*.[x,y] == .[x,n*y])
    expect_true(.[x+y,x-y] == .[y,x]*2)

    expect_true(abelianize(.[x,y]) == 0)
    
    return(TRUE)
} # checker2() closes

checker3 <- function(x,y,z){
    expect_true(jacobi(x,y,z) == 0)
    expect_true(.[x,.[y,z]] + .[y,.[z,x]] + .[z,.[x,y]] == 0)
    f <- ad(x)
    expect_true(f(y*z) == f(y)*z + y*f(z))

}

for(i in 1:2){
    for(inc in c(TRUE,FALSE)){
        x <- rfalg(5,include.negative=inc)
        y <- rfalg(5,include.negative=inc)
        z <- rfalg(5,include.negative=inc)
        n <- sample(1:9,1)
        checker1(x)
        checker2(x,y,n)
        checker3(x,y,z)
    }
}

})
