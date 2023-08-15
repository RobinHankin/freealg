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
  expect_true(is.constant(as.freealg("0")))

  expect_true(is.constant(3))
  expect_true(is.constant(0))

  expect_error(as.freealg(sin))
  

  x <- as.freealg("1+x+2ax")
  y <- as.freealg("2+x+2ax")
  expect_false(is.constant(x))
  expect_error(coeffs(x) <- seq_along(coeffs(x)))
  expect_silent(coeffs(x) <- coeffs(x) + 3)
  expect_error(coeffs(x) <- coeffs(y))


  expect_true(horner("x+y",1:3) == as.freealg("1 + 2x + 2y + 3xy + 3xx  + 3yx + 3yy"))



  expect_true(all(coeffs(freealg(as.list(seq_len(1000))))==1))

  expect_output(print(deriv(as.freealg("aaaabcdaaa"),1)))
  expect_output(print(deriv(as.freealg("aaxAAAxaabcdaaaabXbaa"),1)))
  expect_output(print(deriv(as.freealg("aaxAAAxaabcdaaaabXbaa"),-1)))


  expect_true(deriv(as.freealg("aaaabcdaaxAAxa"), 1) == deriv(as.freealg("aaaabcdaaxAAxa"),"a"))
  expect_true(deriv(as.freealg("aaaabcdaaxAAxa"),-1) == deriv(as.freealg("aaaabcdaaxAAxa"),"A"))

  expect_true(all(grades(grade(horner("1+x+2y",1:7),5))==5))

  a <- horner("1+x+2y",1:7)
  b <- a
  grade(a,5) <- grade(a,5) *100
  expect_true(grade(a,5) == grade(b,5)*100)

  expect_error(grade(b))

  expect_true(nterms(horner("1+x+2y",seq_len(7))) == 2^7-1)
  expect_true(inv(as.freealg("aaaaabcaaabAAA")) == as.freealg("aaaBAAACBAAAAA"))
  expect_error(inv(as.freealg("3+aaaaabcaaabAAA")))
  expect_true(is.numeric(drop(as.freealg("9"))))

  expect_true(keep_pos(as.freealg("a+ 5asAbc+ abc+ A")) == as.freealg("abc + a"))
  
  })

  


