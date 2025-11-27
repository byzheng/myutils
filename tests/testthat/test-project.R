
# Test function
test_project_fun <- function() {
    # Create temporary directory for testing
    test_dir <- file.path(tempdir(), "test_project_fun")
    dir.create(test_dir, showWarnings = FALSE)
    
    # Create test R file
    test_file <- file.path(test_dir, "test_function.R")
    writeLines("test_var <- 42", test_file)
    
    # Test the function
    result <- project_fun(test_dir)
    
    # Check if function worked
    stopifnot(exists("test_var"))
    stopifnot(test_var == 42)
    
    # Clean up
    unlink(test_dir, recursive = TRUE)
    rm(test_var, envir = .GlobalEnv)
    
    cat("test_project_fun passed!\n")
}