test_that("Test pairwise", {
    X <- matrix(c(1, 2, 3, 4,
                  1, 2, 5, 6,
                  1, 2, 5, 7), nrow = 4)
    expect_res <- c(0, 2, 2, 2, 0, 1, 2, 1, 0)
    res <- pairwise_diff(X, 2)
    expect_equal(as.numeric(res), expect_res)
    expect_error(pairwise(X, 3))
    expect_error(pairwise(X, "3"))
    expect_error(pairwise(array(NA, dim = c(2, 3, 3)), "3"))
})
