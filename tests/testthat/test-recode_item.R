test_that("recode_item: standard usecase", {
  test_df = data.frame(id = c(1, 2, 3, 4, 5), x = c(1, 2, 3, 4, 999))
  recoded_df = psycCleaning::recode_item(
    test_df,
    cols = x,
    reverse_code = TRUE,
    retain_code = 1:5
  )
  testthat::expect_equal(recoded_df, tibble::tibble(id = c(1, 2, 3, 4, 5), x = c(4, 3, 2, 1, NA_real_)), ignore_attr = TRUE)
})

test_that("recode_item: code from & to usecase", {
  test_df = data.frame(
    x = c(1, 2, 3, 4),
    y = c(2, 4, 3, 1),
    z = c(1, 2, 5, 6),
    q = c(1, 2, 3, 4)
  )
  
  testthat::expect_warning({
    recoded_df = recode_item(
      data = test_df,
      cols = x:z,
      code_from = c(1, 2, 3, 4),
      code_to = c('1a', '2a', '3a', '4a')
    )
  })
  
  testthat::expect_equal(recoded_df,
                         tibble::tibble(
                           x = c('1a', '2a', '3a', '4a'),
                           y = c('2a', '4a', '3a', '1a'),
                           z = c('1a', '2a', NA, NA),
                           q = c(1, 2, 3, 4)
                         ),
                         ignore_attr = TRUE)
})

test_that("recode_item: only retain code", {
  test_df = data.frame(x = c(1, 2, 3, 4),
                       y = c(2, 4, 3, 1),
                       z = c(1, 2, 5, 6))
  
  recoded_df = recode_item(data = test_df,
                           cols = everything(),
                           retain_code = 1:3)
  
  testthat::expect_equal(recoded_df, tibble::tibble(
    x = c(1, 2, 3, NA),
    y = c(2, NA, 3, 1),
    z = c(1, 2, NA, NA)
  ), ignore_attr = TRUE)
})
