test_that("Test time", {
    first_day <- month_first_day()
    expect_equal(doy2date(first_day)[1:2],
                 c("01-Jan", "01-Feb"))
    expect_equal(date2doy(doy2date(first_day)), first_day)
})
