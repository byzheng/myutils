test_that("Test geom_oz", {
    suppressPackageStartupMessages(suppressWarnings(library(ggplot2)))
    expect_no_error({
        p <- ggplot() +
            geom_oz()
    })
    expect_no_error({
        p <- ggplot() +
            geom_oz(color = "red")
    })

    expect_no_error({
        p <- ggplot() +
            geom_oz() +
            geom_oz_states()
    })


    expect_no_error({
        p <- ggplot() +
            geom_oz(color = "red") +
            geom_oz_states(color = "blue")
    })

    expect_no_error({
        p <- ggplot() +
            geom_oz(color = "red") +
            geom_oz_states(ACT = TRUE)
    })

})
