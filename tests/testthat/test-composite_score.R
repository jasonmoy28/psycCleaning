test_that("composite_score: standard usecase", {
  test_df = data.frame(
    col1 = c(1, 4, 4, 4),
    col2 = c(1, 3, 3, 10),
    col3 = c(1, 8, NA, 4),
    extra = c(4, 6, 2, 2)
  )
  composite_df = composite_score(data = test_df, cols = contains('col'))
  testthat::expect_equal(composite_df,
                         data.frame(
                           col1 = c(1, 4, 4, 4),
                           col2 = c(1, 3, 3, 10),
                           col3 = c(1, 8, NA, 4),
                           extra = c(4, 6, 2, 2),
                           composited_column = c(1,5,NA,6)
                         ))
})

test_that("composite_score: na.rm = T", {
  test_df = data.frame(
    col1 = c(1, 4, 4, 4),
    col2 = c(1, 6, 3, 10),
    col3 = c(1, 8, NA, 4),
    extra = c(5, 6, 2, NA)
  )
  composite_df = composite_score(data = test_df, cols = everything(),na.rm = T)
  testthat::expect_equal(composite_df,
                         data.frame(
                           col1 = c(1, 4, 4, 4),
                           col2 = c(1, 6, 3, 10),
                           col3 = c(1, 8, NA, 4),
                           extra = c(5, 6, 2, NA),
                           composited_column = c(2,6,3,6)
                         ))
})
