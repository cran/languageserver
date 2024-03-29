test_that("enclosed_by_quotes works as expected", {
    enclosed <- function(text, col) {
          .Call("enclosed_by_quotes", PACKAGE = "languageserver", text, col)
      }
    expect_false(enclosed("hello_world", 4))
    expect_true(enclosed("'hello_world", 4))
    expect_true(enclosed("\"hello_world", 4))
    expect_true(enclosed("R\"(hello_world", 4))
    expect_true(enclosed("r\"(hello_world", 4))
    expect_true(enclosed("R\"-(hello_world", 5))
    expect_true(enclosed("r\"-(hello_world", 5))
    expect_true(enclosed("\"\\\"hello_world", 4))
    expect_false(enclosed("\"a\"hello_world", 4))
    expect_false(enclosed("R\"(a)\"hello_world", 7))
    expect_false(enclosed("R\"-(a)-\"hello_world", 9))
    expect_true(enclosed("R\"-(a)--\"hello_world", 9))
    expect_true(enclosed("R\"-(a)\"hello_world", 9))
    expect_true(enclosed("'\\'hello_world", 4))
    expect_false(enclosed("'a'hello_world", 4))
    #410
    expect_true(enclosed("fun('free', )", 8))
    expect_false(enclosed("fun('free', )", 9))
})


test_that("find_unbalanced_bracket works as expected", {
    bsearch <- function(content, row, column) {
        .Call("find_unbalanced_bracket",
            PACKAGE = "languageserver",
            content, row, column, TRUE
        )
    }
    expect_equal(bsearch("foo(xy", 0, 5)[[1]], c(0, 3))
    expect_equal(bsearch("foo(xy(abc)", 0, 5)[[1]], c(0, 3))
    expect_equal(bsearch("foobar", 0, 3)[[1]], c(-1, -1))
    expect_equal(bsearch("foo(bar)abc", 0, 7)[[1]], c(-1, -1))
    expect_equal(bsearch("foo(bar)abc", 0, 8)[[1]], c(-1, -1))
    expect_equal(bsearch("foo(xy(abc)", 0, 6)[[1]], c(0, 6))
    expect_equal(bsearch("foo(xy(abc)", 0, 7)[[1]], c(0, 6))
    expect_equal(bsearch("foo(xy(abc), param2", 0, 12)[[1]], c(0, 3))
    expect_equal(bsearch("foo(#xyz(bar", 0, 10)[[1]], c(0, 3))
    expect_equal(bsearch("foo('xyz(bar", 0, 10)[[1]], c(0, 3))
    expect_equal(bsearch("foo(\"xyz(bar", 0, 10)[[1]], c(0, 3))
    expect_equal(bsearch("foo('xyz', bar", 0, 10)[[1]], c(0, 3))
    expect_equal(bsearch("foo(\"xyz\", bar", 0, 10)[[1]], c(0, 3))
    expect_equal(bsearch("foo(R\"(x\"yz)\", bar", 0, 15)[[1]], c(0, 3))
    expect_equal(bsearch("foo(r\"(x\"yz)\", bar", 0, 15)[[1]], c(0, 3))
    expect_equal(bsearch("foo(R\"(x\"yz)\", bar", 0, 15)[[1]], c(0, 3))
    expect_equal(bsearch("foo(r\"(x\"yz)\", bar", 0, 15)[[1]], c(0, 3))
    expect_equal(bsearch("foo(\"(x\"yz)\", bar", 0, 15)[[1]], c(-1, -1))
    expect_equal(bsearch("foo(\"(x\"yz)\", bar", 0, 15)[[1]], c(-1, -1))
    expect_equal(bsearch("foo(\"(x\"yz)\", bar", 0, 15)[[1]], c(-1, -1))
    expect_equal(bsearch("foo(\"(x\"yz)\", bar", 0, 15)[[1]], c(-1, -1))
    expect_equal(bsearch("𐐀𐐀𐐀(𐐀𐐀𐐀", 0, 5)[[1]], c(0, 3))

    # multiline
    expect_equal(bsearch(c("foo(", "xyz"), 1, 3)[[1]], c(0, 3))
    expect_equal(bsearch(c("foo(  # (", "xyz"), 1, 1)[[1]], c(0, 3))
    expect_equal(bsearch(c("foo(  # \"", "xyz"), 1, 2)[[1]], c(0, 3))
    expect_equal(bsearch(c("foo(  # '", "xyz"), 1, 3)[[1]], c(0, 3))

    expect_equal(bsearch(c("foo({ ", "xyz"), 1, 3)[[2]], "{")
    expect_equal(bsearch(c("foo({} ", "xyz"), 1, 3)[[1]], c(0, 3))
    expect_equal(bsearch(c("foo({ ", "}, xyz"), 1, 3)[[1]], c(0, 3))
    expect_equal(bsearch(c("foo(( ", "), xyz"), 1, 3)[[1]], c(0, 3))

    # issue #530
    expect_equal(bsearch(c("if (sum()) ", "xyz"), 0, 7)[[1]], c(0, 7))
    expect_equal(bsearch(c("} if (sum()) ", "xyz"), 0, 9)[[1]], c(0, 9))

    # other brackets
    expect_equal(bsearch("foo[xy", 0, 5), list(c(0, 3), "["))
    expect_equal(bsearch("foo{xy", 0, 5), list(c(0, 3), "{"))
})
