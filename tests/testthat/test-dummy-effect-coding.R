testthat::test_that('Effect coding',{
  test_df = data.frame(a = c(1,1,2,2,3,2,1,3,2,3,2,1,3,2,1,3,2),
                       b = c(2,1,2,2,3,2,1,1,2,2,2,1,1,3,1,3,1), 
                       c = c(2,2,12,24,35,2,61,61,32,22,22,61,41,73,17,34,11))
  
  # effect coding using function
  test_df_1 = suppressMessages(effect_coding(test_df,cols = a:b)) %>% 
    dplyr::select(dplyr::ends_with('eff'))
  test_df_2 = test_df %>% 
    dplyr::mutate(a_1_eff = dplyr::case_when(a == 1 ~ 1, a == 3 ~ -1,T ~ 0)) %>% 
    dplyr::mutate(a_2_eff = dplyr::case_when(a == 2 ~ 1, a == 3 ~ -1,T ~ 0)) %>% 
    dplyr::mutate(b_2_eff = dplyr::case_when(b == 2 ~ 1, b == 3 ~ -1,T ~ 0)) %>% 
    dplyr::mutate(b_1_eff = dplyr::case_when(b == 1 ~ 1, b == 3 ~ -1,T ~ 0)) %>% 
    dplyr::select(dplyr::ends_with('eff'))
  
  testthat::expect_equal(test_df_1,test_df_2)
})

testthat::test_that('Dummy coding',{
  test_df = data.frame(a = c(1,1,2,2,3,2,1,3,2,3,2,1,3,2,1,3,2),
                       b = c(2,1,2,2,3,2,1,1,2,2,2,1,1,3,1,3,1), 
                       c = c(2,2,12,24,35,2,61,61,32,22,22,61,41,73,17,34,11))
  
  # dummy coding using function
  test_df_1 =  suppressMessages(dummy_coding(test_df,cols = a:b)) %>% 
    dplyr::select(dplyr::ends_with('dum'))
  test_df_2 = test_df %>% 
    dplyr::mutate(a_1_dum = dplyr::case_when(a == 1 ~ 1, T ~ 0)) %>% 
    dplyr::mutate(a_2_dum = dplyr::case_when(a == 2 ~ 1, T ~ 0)) %>% 
    dplyr::mutate(b_2_dum = dplyr::case_when(b == 2 ~ 1, T ~ 0)) %>% 
    dplyr::mutate(b_1_dum = dplyr::case_when(b == 1 ~ 1, T ~ 0)) %>% 
    dplyr::select(dplyr::ends_with('dum'))
  
  testthat::expect_equal(test_df_1,test_df_2)
})
