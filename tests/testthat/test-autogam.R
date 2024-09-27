test_that("autogam works on mtcars", {
  # There seems to be a BLAS issue on some online testing platforms
  skip_on_ci()

  expect_equal(
    autogam(mtcars, 'mpg') |>
      coef() |>
      sum() |>
      round(1),  # rounding needed for consistency across testing platforms
    11.3
  )
})
