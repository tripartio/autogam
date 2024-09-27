test_that("autogam works on mtcars", {
  # There seems to be inconsistencies in results on some online testing platforms.
  # Perhaps a BLAS issue?
  skip_on_ci()
  skip_on_cran()

  expect_equal(
    autogam(mtcars, 'mpg') |>
      coef() |>
      sum() |>
      round(1),  # rounding needed for consistency across testing platforms
    11.3
  )
})
