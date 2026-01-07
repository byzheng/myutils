test_that("tabsets requires grouped data", {
    expect_error(
        tabsets(iris, knitr::kable),
        "`tabsets\\(\\)` requires grouped data. Use dplyr::group_by\\(\\)\\."
    )
})

test_that("tabsets requires data.frame input", {
    expect_error(
        tabsets(c(1, 2, 3), knitr::kable),
        "is.data.frame\\(data\\) is not TRUE"
    )
})

test_that("tabsets works with single grouping variable", {
    grouped_data <- iris |> dplyr::group_by(Species)
    
    # Capture output to test that function runs without error
    expect_no_error({
        output <- capture.output({
            result <- tabsets(grouped_data, head, n = 2)
        })
    })
    
    # Should return ungrouped data invisibly
    result <- tabsets(grouped_data, function(x) invisible(x))
    expect_false(dplyr::is_grouped_df(result))
    expect_s3_class(result, "data.frame")
})

test_that("tabsets works with multiple grouping variables", {
    # Create test data with multiple grouping variables
    test_data <- data.frame(
        group1 = rep(c("A", "B"), each = 4),
        group2 = rep(c("X", "Y"), 4),
        value = 1:8
    ) |> dplyr::group_by(group1, group2)
    
    expect_no_error({
        output <- capture.output({
            result <- tabsets(test_data, head, n = 2)
        })
    })
})

test_that("tabsets handles empty groups correctly", {
    # Create data where some groups might be empty after filtering
    test_data <- data.frame(
        group = c("A", "A", "B"),
        value = c(1, 2, 3)
    ) |> dplyr::group_by(group)
    
    expect_no_error({
        output <- capture.output({
            tabsets(test_data, head)
        })
    })
})

test_that("tabsets works with quarto format", {
    grouped_data <- iris |> dplyr::group_by(Species)
    
    output <- capture.output({
        tabsets(grouped_data, head, n = 2, use_quarto = TRUE)
    })
    
    # Check that quarto-specific syntax is present
    expect_true(any(grepl("::: \\{.panel-tabset\\}", output)))
    expect_true(any(grepl(":::", output)))
})

test_that("tabsets works with rmarkdown format", {
    grouped_data <- iris |> dplyr::group_by(Species)
    
    output <- capture.output({
        tabsets(grouped_data, head, n = 2, use_quarto = FALSE)
    })
    
    # Check that rmarkdown-specific syntax is present
    expect_true(any(grepl("\\{.tabset\\}", output)))
    expect_true(any(grepl("^## ", output)))
})

test_that("tabsets passes additional arguments to FUN", {
    grouped_data <- iris |> dplyr::group_by(Species)
    
    # Test with additional arguments
    expect_no_error({
        output <- capture.output({
            tabsets(grouped_data, head, n = 3)
        })
    })
    
    # Test with named arguments
    expect_no_error({
        output <- capture.output({
            tabsets(grouped_data, utils::head, n = 2)
        })
    })
})

test_that("tabsets works with factor grouping variables", {
    # Test with factor levels
    test_data <- iris
    test_data$Species <- factor(test_data$Species, levels = c("virginica", "setosa", "versicolor"))
    grouped_data <- test_data |> dplyr::group_by(Species)
    
    expect_no_error({
        output <- capture.output({
            tabsets(grouped_data, head, n = 2)
        })
    })
})

test_that("tabsets works with different FUN functions", {
    grouped_data <- iris |> dplyr::group_by(Species)
    
    # Test with different functions
    expect_no_error({
        capture.output({
            tabsets(grouped_data, summary)
        })
    })
    
    expect_no_error({
        capture.output({
            tabsets(grouped_data, function(x) print(nrow(x)))
        })
    })
})
