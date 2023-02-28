testthat::test_that('Effect coding',{
  test_df = data.frame(a = c(1,1,2,2,3,2,1,3,2,3,2,1,3,2,1,3,2),
                       b = c(2,1,2,2,3,2,1,1,2,2,2,1,1,3,1,3,1), 
                       c = c(2,2,12,24,35,2,61,61,32,22,22,61,41,73,17,34,11))
  
  # effect coding using function
  new_test_df = effect_coding(test_df,a:b)
  model_summary = lm(data = new_test_df, c ~ a) %>% summary()
  model_summary$coefficients
  
  test_df = test_df %>%  
    dplyr::mutate(a1 = dplyr::case_when(a == 1 ~ 1,
                                        a == 2 ~ 0,
                                        a == 3 ~ -1)) %>% 
    dplyr::mutate(a2 = dplyr::case_when(a == 1 ~ 0,
                                        a == 2 ~ 1,
                                        a == 3 ~ -1)) 
  model_summary_2 = lm(data = test_df, c ~ a1 + a2) %>% summary()
  model_summary_2$coefficients
  
  testthat::expect_identical(model_summary$coefficients,model_summary_2$coefficients)
})
