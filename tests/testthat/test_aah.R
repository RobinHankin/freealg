test_that("Test suite aah.R",{

    x <- rfalgg()
    y <- rfalggg()

    expect_true(is.freealg(x))
    expect_true(is.freealg(y))

    expect_true(x+y == y+x)
})
