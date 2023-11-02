testthat::test_that('center_mlm',{
  test_data = psycCleaning::mlbook_red_data %>% center_mlm(ses,group = 'schoolnr')
  
  testthat::expect_equal(test_data$ses_mean,test_data$ses_agg)
  testthat::expect_equal(test_data$ses_group_c,test_data$ses_CMC)
})

testthat::test_that('z_score_mlm',{
  test_data = psycCleaning::mlbook_red_data %>% z_scored_mlm(ses,group = 'schoolnr')
  
  testthat::expect_equal(test_data$ses_mean_z,test_data$Zses_agg)
  testthat::expect_equal(test_data$ses_group_z,test_data$Zses_CMC)
})

testthat::test_that('z_scored_mlm_categorical',{
  test_data = psycCleaning::mlbook_red_data %>% z_scored_mlm_categorical(cols = 'female_eff',dummy_coded = 'female_dum',group = 'schoolnr')
  
  testthat::expect_equal(test_data$female_eff_group_c,test_data$female_CMC)
  testthat::expect_equal(test_data$female_dum_mean_z,test_data$Zfempct_agg)
})

testthat::test_that('center_mlm_categorical',{
  suppressWarnings({test_data = psycCleaning::mlbook_red_data %>% center_mlm(cols = 'female_dum',group = 'schoolnr')}) 

  testthat::expect_equal(test_data$female_dum_mean,test_data$fempct_agg)
})
