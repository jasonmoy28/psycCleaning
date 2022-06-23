testthat::test_that('summarize_missing_values: no grouping', {
  test_df = data.frame(
    col1 = c(1, 1, 1, 2, 2, 2),
    col2 = c(1, NA, 3, 3, 5, 6),
    col3 = c(1, 2, NA, 6, 4, 3)
  )
  summarized_df = summarize_non_missing_values(test_df, everything())
  testthat::expect_equal(summarized_df,
                         data.frame(
                           name = c('col1', 'col2', 'col3'),
                           value = c(6, 5, 5)
                         ),
                         ignore_attr = TRUE)
})

testthat::test_that('summarize_missing_values: with grouping', {
  test_df = data.frame(
    col1 = c(
      'condition 1',
      'condition 1',
      'condition 1',
      'condition 2',
      'condition 2',
      'condition 2'
    ),
    col2 = c(1, NA, 3, 3, 5, 6),
    col3 = c(1, 2, NA, 6, 4, 3)
  )
  summarized_df = summarize_non_missing_values(test_df, everything(), group = col1)
  testthat::expect_equal(summarized_df,
                         data.frame(
                           col1 = c('condition 1', 'condition 2'),
                           col2 = c(2, 3),
                           col3 = c(2, 3)
                         ),
                         ignore_attr = TRUE)
})
