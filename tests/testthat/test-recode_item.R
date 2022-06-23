test_that("recode_item: standard usecase", {
  test_df = data.frame(id = c(1,2,3,4,5), x = c(1,2,3,4,999))
  recoded_df = psycCleaning::recode_item(test_df, cols = x, reverse_code = TRUE, retain_code = 1:5)
  testthat::expect_equal(recoded_df, tibble::tibble(id = c(1,2,3,4,5),x = c(4,3,2,1,NA_real_)),ignore_attr = TRUE)
})

test_that("recode_item: code from & to usecase", {
  test_df = data.frame(x = c(1,2,3,4))
  recoded_df = psycCleaning::recode_item(data = test_df, cols = x, code_from = c(1,2,3,4),code_to = c(7,8,9,10))
  testthat::expect_equal(recoded_df, tibble::tibble(x = c(7,8,9,10)),ignore_attr = TRUE)
})

