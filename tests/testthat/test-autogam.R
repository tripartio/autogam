test_that("autogam works on mtcars", {
  expect_equal(
    autogam(mtcars, 'mpg') |>
      coef() |>
      sum() |>
      round(3),
    11.253
  )
})
