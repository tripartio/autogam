test_that("mtcars smooth string works", {
  expect_equal(
    smooth_formula_string(mtcars, 'mpg'),
    "mpg ~ cyl + s(disp,bs='cr') + s(hp,bs='cr') + s(drat,bs='cr') + s(wt,bs='cr') + s(qsec,bs='cr') + vs + am + gear + s(carb,k=3,bs='cr')"
  )
})
