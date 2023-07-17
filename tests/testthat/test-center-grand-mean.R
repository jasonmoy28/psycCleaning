testthat::test_that('z_scored_grand_mean', code = {
  test_df = tibble::tibble(x = 1:5, y = 10:14)
  centered_df = center_grand_mean(test_df,cols = everything())
  for (i in 1:length(centered_df$x)) {
    expect_equal(centered_df$x_c[i], test_df$x[i] - mean(test_df$x))
  }
  
  for (i in 1:length(centered_df$y)) {
    expect_equal(centered_df$y_c[i], test_df$y[i] - mean(test_df$y))
  }
})

    