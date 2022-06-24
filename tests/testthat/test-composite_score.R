test_that("composite_score: standard usecase", {
  test_df = data.frame(
    col1 = c(1, 4, 3, 4),
    col2 = c(1, 3, 3, 10),
    col3 = c(1, 8, NA, 4),
    extra = c(4, 6, 3, 2)
  )
  composite_df = composite_score(data = test_df, cols = contains('col'))
  testthat::expect_equal(composite_df,
                         data.frame(
                           col1 = c(1, 4, 3, 4),
                           col2 = c(1, 3, 3, 10),
                           col3 = c(1, 8, NA, 4),
                           extra = c(4, 6, 3, 2),
                           composited_column = c(1,5,NA,6)
                         ))
})
