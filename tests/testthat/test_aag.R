## This file follows the structure of aaa.R in the free group package.

## Define some checker functions, and call them at the end.  They
## should all return TRUE if the package works, and stop with error if
## a test is failed.  Function checker1() has one argument, checker2()
## two, and checker3() has three.  

test_that("Test suite aag.R",{

  expect_true(as.freealg("-1 + x") == -as.freealg("1-x"))
  expect_true(as.freealg("-1 + 2x") == -as.freealg("1-2x"))
  expect_true(as.freealg("-4 + x") == -as.freealg("4-x"))

})
