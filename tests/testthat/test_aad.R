test_that("test suite aad",{
  subs("a",a="1+x") == as.freealg("1+x")^1
  subs("aa",a="1+x") == as.freealg("1+x")^2
  subs("aaa",a="1+x") == as.freealg("1+x")^3
  subs("aaaa",a="1+x") == as.freealg("1+x")^4

  f <- function(n){expect_true(subs(as.freealg(paste(rep("a",n),collapse="")),a="1+x") == as.freealg("1+x")^n)}
  sapply(1:20,f)

  foo <- function(p){
    if(!(24 %in% unique(c(elements(words(p)),recursive=TRUE)))){
      expect_true(subs(p,x="1+x") == p)
    }
  }
  
  for(i in 1:10){
    foo(rfalg(distinct=3))
  }

  expect_true(linear(1:3) == as.freealg('a+2b+3c'))
  expect_false(linear(1:3) == as.freealg('1+a+2b+3c'))
  expect_silent(pepper("pepper"))

  p <- as.freealg("1 +2xy +3yx + 4xyz")
  # following line produces an error but only on disordR >= 0.9-2:
  expect_error(coeffs(p)[coeffs(p)<=2] <- coeffs(p)[coeffs(p)>2] * 1000)

  p <- as.freealg("1+x+xyz +X")  # issue #44
  coeffs(p)[unlist(lapply(words(p),function(x){any(x<0)}))] <- 0
  expect_true(p == as.freealg("1+x+xyz"))

  ##  issue #46, extraction:
  a <- as.freealg("aaa + 2*aaba + 3*abbbba + 4*xyzabc - 3*abc")
  expect_true(a[coeffs(a)==  1] == as.freealg("aaa"))
  expect_true(a[coeffs(a) >  2] == as.freealg("3*abbbba + 4*xyzabc"))
  expect_true(a[coeffs(a) >  4] == 0)
  
  ##  issue #46, replacement:
  a <- as.freealg("aaa + 2*aaba + 3*abbbba + 4*xyzabc - 3*abc")
  a[coeffs(a)>3] <- 9
  expect_true(a == as.freealg("aaa + 2*aaba + 3*abbbba + 9*xyzabc - 3*abc"))
  expect_error(a[a])

  a <- as.freealg("aaa + 2*aaba + 3*abbbba + 4*xyzabc - 3*abc")
  a[coeffs(a)>5] <- 99
  expect_true(a == as.freealg("aaa + 2*aaba + 3*abbbba + 4*xyzabc - 3*abc"))

  a <- as.freealg("aaa + 2*aaba + 3*abbbba + 4*xyzabc - 3*abc")
  a[coeffs(a) <= 9] <- 88
  expect_true(a == as.freealg("88*aaa + 88*aaba + 88*abbbba + 88*xyzabc + 88*abc"))

  a <- as.freealg("aaa + 2*aaba + 3*abbbba + 4*xyzabc - 3*abc")
  expect_error(a[a] <- 0)



  
  
})
