testthat::test_that('summarize_missing_values: no group',{
  test_df = data.frame(
    a = c(1,3,5,7,NA,4,2,NA,4,3,NA),
    b = c(1,NA,5,7,NA,4,NA,NA,4,3,NA),
    c = c(1,3,5,7,NA,3,2,4,4,4,3),
    d = c(NA,3,5,7,NA,3,2,4,NA,NA,3)
  )
  
  missing_value_summary = summarize_missing_values(test_df,
                                                   cols = a:d,
                                                   return_result = T,
                                                   verbose = F)
  
  testthat::expect_equal(missing_value_summary$miss_count, c(3,5,1,4))
  testthat::expect_equal(missing_value_summary$non_miss_count,c(8,6,10,7))
})

testthat::test_that('summarize_missing_values: with group',{
test_df = data.frame(
  condition = c(1,1,1,3,3,3,5,5,5,5,5),
  a = c(1,3,5,7,NA,4,2,NA,4,3,NA),
  b = c(1,NA,5,7,NA,4,NA,NA,4,3,NA),
  c = c(1,3,5,7,NA,3,2,4,4,4,3),
  d = c(NA,3,5,7,NA,3,2,4,NA,NA,3)
)

missing_summary = summarize_missing_values(test_df,cols = a:d,group = condition,return_result = T,verbose = F)
testthat::expect_equal(missing_summary$miss_count, c(0,1,0,1,1,1,1,1,2,3,0,2))
testthat::expect_equal(missing_summary$non_miss_count, c(3,2,3,2,2,2,2,2,3,2,5,3))
})
