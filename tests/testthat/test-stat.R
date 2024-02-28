test_that("Statistics functions", {
    library(dplyr)

    # Test statistics function

    x <- c(1.3, 1.5, 3.2, 4.3, 5.3, 5.8)
    y <- c(0.7, 1.7, 2.6, 4.7, 5.3, 6.3)

    expect_equal(slope(x, y), 1.142567, tolerance=1e-6)
    # Test rsq
    expect_error(rsq("1", "a"))
    expect_equal(rsq(x, y), 0.967166, tolerance=1e-7)

    # test nrmse
    expect_error(nrmse("1", "a"))
    expect_error(nrmse(x, y, method = "min"))
    expect_equal(nrmse(x, y), 0.09813068, tolerance=1e-7)
    expect_equal(nrmse(x, y, method = "maxmin"), 0.09813068, tolerance=1e-7)
    expect_equal(nrmse(x, y, method = "sd"), 0.2322647, tolerance=1e-7)
    expect_equal(nrmse(x, y, method = "mean"), 0.1238097, tolerance=1e-7)
    expect_equal(nrmse(x, y, method = "iq"), 0.1413082, tolerance=1e-7)

    # Test model_summarise
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

    expect_error(model_summarise(res, digits = 2, direction = "dd"))
    expect_value <- c(n = 10, r = 0.99, r2 = 0.99,
                      bias = -0.55,
                      mse = 0.39,
                      rmse = 0.63,
                      nrmse = 0.11,
                      d = 0.99)

    res0 <- data %>%
        model_summarise(digits = 2)
    expect_equal(names(res0), names(expect_value))
    res <- res0 %>%
        as.vector() %>%
        as.numeric()
    expect_equivalent(res, expect_value)

    # Test model_summarise
    res <- data %>%
        model_summarise(digits = 2, direction = "long") %>%
        magrittr::use_series("value") %>%
        as.vector() %>%
        as.numeric()
    expect_equivalent(res, expect_value)

    res <- data %>%
        model_summarise(digits = 2, direction = "long", nrmse_method = "mean") %>%
        magrittr::use_series("value") %>%
        as.vector() %>%
        as.numeric()
    expect_equivalent(res, expect_value)

    # test extra argument for model_summarise
    expect_value <- c(n = 10, r = 0.99, r2 = 0.99,
                      bias = -0.55,
                      mse = 0.39,
                      rmse = 0.63,
                      nrmse = 0.11,
                      d= 0.99, error7day = 1)
    res <- data %>%
        model_summarise(digits = 2, direction = "long", extra = TRUE) %>%
        magrittr::use_series("value") %>%
        as.vector() %>%
        as.numeric()
    expect_equivalent(res, expect_value)

    res <- data %>%
        model_summarise(digits = 2, direction = "long", nrmse_method = "mean", extra = TRUE) %>%
        magrittr::use_series("value") %>%
        as.vector() %>%
        as.numeric()
    expect_equivalent(res, expect_value)

    # Test group in model_summarise
    data <- read.csv(file = textConnection('g,x,y
1,1,1.2655086631421
1,2,2.37212389963679
1,3,3.5728533633519
1,4,4.90820778999478
1,5,5.20168193103746
2,6,6.8983896849677
2,7,7.94467526860535
2,8,8.66079779248685
2,9,9.62911404389888
2,10,10.0617862704676'))
    res <- data %>%
        group_by(g) %>%
        model_summarise(digits = 2, direction = "wide", extra = TRUE)
    expect_equivalent(nrow(res), 2)
    expect_equivalent(ncol(res), 10)

    res <- data %>%
        group_by(g) %>%
        model_summarise(digits = 2, direction = "wide", extra = TRUE, .groups = "keep")
    expect_equivalent(is_grouped_df(res), TRUE)
})

