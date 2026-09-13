test_that("Type hierarchy works with R6Class", {
    skip_on_cran()
    client <- language_client()

    single_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "library(R6)",
        "Animal <- R6::R6Class('Animal', public = list(",
        "  initialize = function(name) { self$name <- name }",
        "))",
        "Dog <- R6::R6Class('Dog', inherit = Animal, public = list(",
        "  bark = function() { print('Woof!') }",
        "))"
    ), single_file)

    client %>% did_open(single_file)

    # Test prepare type hierarchy for Animal
    result <- client %>% respond_prepare_type_hierarchy(
        single_file, c(1, 1), retry_when = function(result) length(result) == 0)

    expect_length(result, 1)
    expect_equal(result[[1]]$name, "Animal")
    expect_equal(result[[1]]$kind, SymbolKind$Class)
    expect_equal(result[[1]]$uri, path_to_uri(single_file))
    expect_true(!is.null(result[[1]]$data$classType))
    expect_equal(result[[1]]$data$classType, "R6")

    # Test prepare type hierarchy for Dog
    result <- client %>% respond_prepare_type_hierarchy(
        single_file, c(4, 1), retry_when = function(result) length(result) == 0)

    expect_length(result, 1)
    expect_equal(result[[1]]$name, "Dog")
    expect_equal(result[[1]]$kind, SymbolKind$Class)
    expect_equal(result[[1]]$data$classType, "R6")
})

test_that("Type hierarchy returns supertypes for R6Class", {
    skip_on_cran()
    client <- language_client()

    single_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "library(R6)",
        "Animal <- R6::R6Class('Animal', public = list(",
        "  initialize = function(name) { self$name <- name }",
        "))",
        "Dog <- R6::R6Class('Dog', inherit = Animal, public = list(",
        "  bark = function() { print('Woof!') }",
        "))"
    ), single_file)

    client %>% did_open(single_file)

    # Prepare Dog
    item <- client %>% respond_prepare_type_hierarchy(
        single_file, c(4, 1), retry_when = function(result) length(result) == 0)

    expect_length(item, 1)

    # Get supertypes
    result <- client %>% respond_type_hierarchy_supertypes(
        item[[1]], retry_when = function(result) length(result) == 0)

    expect_length(result, 1)
    expect_equal(result[[1]]$name, "Animal")
    expect_equal(result[[1]]$kind, SymbolKind$Class)
})

test_that("Type hierarchy returns subtypes for R6Class", {
    skip_on_cran()
    client <- language_client()

    single_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "library(R6)",
        "Animal <- R6::R6Class('Animal', public = list(",
        "  initialize = function(name) { self$name <- name }",
        "))",
        "Dog <- R6::R6Class('Dog', inherit = Animal, public = list(",
        "  bark = function() { print('Woof!') }",
        "))",
        "Cat <- R6::R6Class('Cat', inherit = Animal, public = list(",
        "  meow = function() { print('Meow!') }",
        "))"
    ), single_file)

    client %>% did_open(single_file)

    # Prepare Animal
    item <- client %>% respond_prepare_type_hierarchy(
        single_file, c(1, 1), retry_when = function(result) length(result) == 0)

    expect_length(item, 1)

    # Get subtypes
    result <- client %>% respond_type_hierarchy_subtypes(
        item[[1]], retry_when = function(result) length(result) == 0)

    expect_gte(length(result), 2)
    names <- vapply(result, function(x) x$name, character(1))
    expect_setequal(names, c("Dog", "Cat"))
})

test_that("Type hierarchy returns S4 supertypes and subtypes", {
    skip_on_cran()
    client <- language_client()

    single_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        'setClass("BaseEntity")',
        'setClass("User", contains = "BaseEntity")',
        'setClass("AdminUser", contains = "User")'
    ), single_file)

    client %>% did_open(single_file)
    item <- client %>% respond_prepare_type_hierarchy(
        single_file, c(1, 10), retry_when = function(result) length(result) == 0)

    expect_length(item, 1)
    expect_equal(item[[1]]$name, "User")
    expect_equal(item[[1]]$data$classType, "S4")
    expect_equal(item[[1]]$range, list(
        start = list(line = 1L, character = 10L),
        end = list(line = 1L, character = 14L)
    ))

    supertypes <- client %>% respond_type_hierarchy_supertypes(
        item[[1]], retry_when = function(result) length(result) == 0)

    expect_length(supertypes, 1)
    expect_equal(supertypes[[1]]$name, "BaseEntity")
    expect_equal(supertypes[[1]]$range, list(
        start = list(line = 1L, character = 29L),
        end = list(line = 1L, character = 39L)
    ))

    subtypes <- client %>% respond_type_hierarchy_subtypes(
        item[[1]], retry_when = function(result) length(result) == 0)

    expect_length(subtypes, 1)
    expect_equal(subtypes[[1]]$name, "AdminUser")
    expect_equal(subtypes[[1]]$range, list(
        start = list(line = 2L, character = 10L),
        end = list(line = 2L, character = 19L)
    ))
})

test_that("Type hierarchy returns RefClass supertypes and subtypes", {
    skip_on_cran()
    client <- language_client()

    single_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        'setRefClass("BaseReference")',
        'setRefClass("UserReference", contains = "BaseReference")',
        'setRefClass("AdminReference", contains = "UserReference")'
    ), single_file)

    client %>% did_open(single_file)
    item <- client %>% respond_prepare_type_hierarchy(
        single_file, c(1, 13), retry_when = function(result) length(result) == 0)

    expect_length(item, 1)
    expect_equal(item[[1]]$name, "UserReference")
    expect_equal(item[[1]]$data$classType, "RefClass")
    expect_equal(item[[1]]$range, list(
        start = list(line = 1L, character = 13L),
        end = list(line = 1L, character = 26L)
    ))

    supertypes <- client %>% respond_type_hierarchy_supertypes(
        item[[1]], retry_when = function(result) length(result) == 0)

    expect_length(supertypes, 1)
    expect_equal(supertypes[[1]]$name, "BaseReference")
    expect_equal(supertypes[[1]]$range, list(
        start = list(line = 1L, character = 41L),
        end = list(line = 1L, character = 54L)
    ))

    subtypes <- client %>% respond_type_hierarchy_subtypes(
        item[[1]], retry_when = function(result) length(result) == 0)

    expect_length(subtypes, 1)
    expect_equal(subtypes[[1]]$name, "AdminReference")
    expect_equal(subtypes[[1]]$range, list(
        start = list(line = 2L, character = 13L),
        end = list(line = 2L, character = 27L)
    ))
})

test_that("Type hierarchy returns empty for non-class definitions", {
    skip_on_cran()
    client <- language_client()

    single_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "foo <- function(x) { x + 1 }",
        "bar <- 42"
    ), single_file)

    client %>% did_open(single_file)

    # Try to prepare type hierarchy on a regular function
    result <- client %>% respond_prepare_type_hierarchy(
        single_file, c(0, 1), retry = FALSE)

    expect_null(result)
})
