## This file follows the structure of aaa.R in the free group package.

test_that("Test suite aag.R",{

  expect_true(as.freealg("-1 + x") == -as.freealg("1-x"))
  expect_true(as.freealg("-1 + 2x") == -as.freealg("1-2x"))
  expect_true(as.freealg("-4 + x") == -as.freealg("4-x"))

})
