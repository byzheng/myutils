test_that("Statistics functions", {
    library(dplyr)
    expect_error(rsq("1", "a"))
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
    res <- data %>%
        model_summarise(digits = 2) %>%
        as.vector() %>%
        as.numeric()
    expect_equal(res, c(10, 0.99, -0.55, 0.63))
})
