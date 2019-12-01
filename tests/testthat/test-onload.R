test_that("onload hook expands all functions", {
  envir <- as.environment(
    list(
      x = 42,
      y = identity,
      z = function(a, b) add(a, b),
      add = defmacro(function(a, b) quote(a + b))
    )
  )
  onload(envir)
  expect_equal(envir$x, 42)
  expect_equal(envir$y, identity)
  expect_equal(body(envir$z), quote(a + b))
  expect_true(is_macro(envir$add))
})
