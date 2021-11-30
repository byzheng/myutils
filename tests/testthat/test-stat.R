test_that("Statistics functions", {
    library(dplyr)
    expect_error(rsq("1", "a"))

    data <- data.frame(x = 1:10, y = 1:10)
    res <- data %>%
        model_summarise() %>%
        as.vector() %>%
        as.numeric()
    expect_equal(res, c(10, 1, 0, 0))
})
