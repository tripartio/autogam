test_that("mtcars smooth string works", {
  expect_equal(
    "mpg ~ cyl + s(disp) + s(hp) + s(drat) + s(wt) + s(qsec) + vs + am + gear + s(carb,k=3)",
    smooth_formula_string(mtcars, 'mpg')
  )
})
