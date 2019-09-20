# modelled on mvp/tests/testthat/test_aad.R which tests specific
# identities.

test_that("Test suite zzz, specific identities and miscellaneous checks",{

  expect_true(is_ok_free(list(),coeffs=1))
  expect_true(
      freealg(list(1,c(1,2)),c(4,5))==freealg(list(1,c(1,0,2)),c(4,5))
  )

  expect_silent(x <- as.freealg(list(list(1,1:2),1:2)))
  expect_silent(x <- as.freealg(1:6))
  expect_silent(x <- freealg(list(1,c(1,0,2)),c(4,5)))

  expect_true(is.constant(as.freealg("7")))
  expect_error(as.freealg(sin))


  x <- rfalg()
  expect_silent(x <- as.freealg(x))
  constant(x) <- 34
  expect_true(constant(x) == 34)
  expect_error(coeffs(x) <- seq_along(coeffs(x)))

  coeffs(x) <- 3
  expect_true(all(coeffs(x)==3))

  print(x)


  })

  


