testthat::test_that('center_grand_mean', code = {
  test_df = tibble::tibble(x = 1:5, y = 10:14)
  centered_df = center_grand_mean(test_df,cols = everything())
  for (i in 1:length(centered_df$x)) {
    expect_equal(centered_df$x_c[i], test_df$x[i] - mean(test_df$x))
  }

  for (i in 1:length(centered_df$y)) {
    expect_equal(centered_df$y_c[i], test_df$y[i] - mean(test_df$y))
  }
})

testthat::test_that('z_scored_grand_mean', code = {
  centered_col = z_scored_grand_mean(iris,cols = 'Sepal.Length') %>% dplyr::pull('Sepal.Length_z')
  check_centered_col = scale(iris$Sepal.Length, center = TRUE,scale = TRUE) %>% as.numeric()
  testthat::expect_equal(centered_col,check_centered_col)
})


testthat::test_that('center_group_mean', code = {
  centered_col = center_group_mean(iris,cols = 'Sepal.Length',group = 'Species') %>% dplyr::pull('Sepal.Length_group_c')
  check_centered_col = misty::center(iris$Sepal.Length,type = 'CWC',cluster = iris$Species)
  testthat::expect_equal(centered_col,check_centered_col)
})


testthat::test_that('z_scored_group_mean', code = {
  centered_col = z_scored_group_mean(iris,cols = 'Sepal.Length',group = 'Species') %>% dplyr::pull('Sepal.Length_group_z')
  check_centered_col = misty::center(iris$Sepal.Length,type = 'CWC',cluster = iris$Species) %>% scale(center = TRUE,scale = TRUE) %>% as.numeric()
  testthat::expect_equal(centered_col,check_centered_col)
})

