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


# gmt <- gam(mpg ~ cyl + s(disp, bs = "cr") + s(hp, bs = "cr") + s(drat, bs = "cr") +
#              s(wt, bs = "cr") + s(qsec, bs = "cr") + vs + am + gear +
#              s(carb, k = 3, bs = "cr"),
#            data = mtcars)
# agmt <- autogam(mtcars, 'mpg')
#
# summary(gmt)
# summary(agmt)
#
# sa_wmae_mad(mtcars$mpg, predict(gmt))
# sa_wmae_mad(mtcars$mpg, predict(agmt, type = 'response'))
