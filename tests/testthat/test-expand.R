test_that("simple expand example works", {
  add <- defmacro(function(a, b) {
    bquote(.(a) + .(b))
  })
  fun <- function(a, b) {
    add(a, b)
  }
  fun <- expand_function(fun)
  expect_true(is.function(fun))
  expect_equal(body(fun), quote({
    a + b
  }))
})

test_that("expanding a macro retains attributes", {
  add <- defmacro(function() {
    1
  })
  add <- expand_function(add)
  expect_true(inherits(add, "defmacro_macro"))
})
