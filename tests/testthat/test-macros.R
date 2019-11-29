test_that("macros are functions", {
  add <- defmacro(function(a, b) {
    a + b
  })
  expect_equal(add(2, 2), 4)
})
