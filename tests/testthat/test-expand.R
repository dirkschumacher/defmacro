test_that("simple expand example works", {
  add <- defmacro(function(a, b) {
    bquote(.(a) + .(b))
  })
  fun <- function(a, b) {
    add(a, b)
  }
  fun <- expand_function(fun)
  expect_true(is.function(fun))
  expect_equal(body(fun), quote({a + b}))
})
