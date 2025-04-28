test_that("Test geom_oz", {
    library(ggplot2)
    expect_no_error({
        p <- ggplot() +
            geom_oz()
    })
    expect_no_error({
        p <- ggplot() +
            geom_oz(color = "red")
    })
})
