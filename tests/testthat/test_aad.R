

test_that("test suite aad",{
  subs("a",a="1+x") == as.freealg("1+x")^1
  subs("aa",a="1+x") == as.freealg("1+x")^2
  subs("aaa",a="1+x") == as.freealg("1+x")^3
  subs("aaaa",a="1+x") == as.freealg("1+x")^4

  f <- function(n){expect_true(subs(as.freealg(paste(rep("a",n),collapse="")),a="1+x") == as.freealg("1+x")^n)}
  sapply(1:20,f)



  foo <- function(p){
    if(!(24 %in% unique(c(words(p),recursive=TRUE)))){
      expect_true(subs(p,x="1+x") == p)
    }
  }
  
  for(i in 1:10){
    foo(rfalg(distinct=3))
  }

})
