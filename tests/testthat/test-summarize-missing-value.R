testthat::test_that('summarize missing value: standard use case', {
  test_df = data.frame(
    col1 = c(1, 2, 10),
    col2 = c(1, NA, 3),
    col3 = c(1, NA, NA),
    y = c(NA, NA, NA)
  )
  summarized_df = summarize_missing_values(test_df, cols = dplyr::contains('col'))
  expect_equal(c(summarized_df[, 2]), list(value = c(0, 1, 2)))
})

testthat::test_that('summarize missing value: grouping use case', {
  test_df = data.frame(
    col1 = c(1, 2, 10, 5),
    col2 = c(1, NA, 3, 4),
    col3 = c(1, NA, NA, 1),
    condition = c(1, 1, 2, 2)
  )
  summarized_df = summarize_missing_values(test_df,
                                           cols = dplyr::contains('col'),
                                           group = condition)
  expect_equal(summarized_df,
               data.frame(
                 condition = c(1, 2),
                 col1 = c(0, 0),
                 col2 = c(1, 0),
                 col3 = c(1, 1)
               ),
               ignore_attr = TRUE)
})
