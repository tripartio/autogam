test_that("autogam works on mtcars", {
  expect_equal(
    autogam(mtcars, 'mpg') |>
      coef() |>
      sum() |>
      round(1),  # rounding needed for consistency across testing platforms
    11.3
  )
})
