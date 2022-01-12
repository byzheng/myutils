test_that("Statistics functions", {
    library(dplyr)

    # Test statistics function

    x <- c(1.3, 1.5, 3.2, 4.3, 5.3, 5.8)
    y <- c(0.7, 1.7, 2.6, 4.7, 5.3, 6.3)

    expect_error(rsq("1", "a"))
    expect_equal(rsq(x, y), 0.967166, tolerance=1e-7)

    expect_error(mse("1", "a"))
    expect_equal(mse(x, y), 0.195, tolerance=1e-7)

    data <- read.csv(file = textConnection('x,y
1,1.2655086631421
2,2.37212389963679
3,3.5728533633519
4,4.90820778999478
5,5.20168193103746
6,6.8983896849677
7,7.94467526860535
8,8.66079779248685
9,9.62911404389888
10,10.0617862704676'))

    # Test model_summarise
    expect_error(model_summarise(res, digits = 2, direction = "dd"))
    expect_value <- c(10, 0.99, 0.99, 0.39, -0.55, 0.63)
    res <- data %>%
        model_summarise(digits = 2) %>%
        as.vector() %>%
        as.numeric()
    expect_equal(res, expect_value)

    res <- data %>%
        model_summarise(digits = 2, direction = "long") %>%
        magrittr::use_series("value") %>%
        as.vector() %>%
        as.numeric()
    expect_equal(res, expect_value)

})
