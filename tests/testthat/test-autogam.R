test_that("autogam works on mtcars", {
  expect_equal(
    11.253,
    autogam(mtcars, 'mpg') |>
      coef() |>
      sum() |>
      round(3)
  )
})
