test_that("Test suite aae.R",{
    x <- as.freealg("5B + 2Babc + 3aC + 4c + cB")
    expect_true(deriv(x,c(2,1)) == deriv(x,c(1,2)))

    jj <- list(indices = list(c(-5, -5, 5, 7), c(-5, 7)), coeffs = c(-2,2))
    x <- freealg(jj[[1]],jj[[2]])
    expect_true(x==x)

    expect_true(
        "1+x+xy" %>% subs(x="1+z") %>% horner(seq_len(5)) == 
        "1+x+xy" %>% horner(seq_len(5)) %>% subs(x="1+z"))
 
})  # test_that() function closes
