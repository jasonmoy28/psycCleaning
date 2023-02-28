#' Count the number of missing / `NA` values
#'
#' It counts the number of missing / `NA` values in each column.
#'
#' @param data data frame.
#' @param cols vector or tidyselect syntax or helpers. default is all columns
#' @param verbose default is `TRUE`. Print the missing value data frame
#' @param return_result default is `FALSE`. Return `data_frame` if set to yes
#' @param group character. count missing values by group. 
#'
#' @return
#' return a data frame with the number of NA values of each columns if `return_result = TRUE`
#' @export
#'
#' @examples
#' df1 = data.frame(col1 = c(1,2,3),col2 = c(1,NA,3),col3 = c(1,2,NA))
#' summarize_missing_values(df1,everything())
#'
summarize_missing_values = function(data,
                                    cols = dplyr::everything(),
                                    group = NULL,
                                    verbose = TRUE,
                                    return_result = FALSE) {
  cols = dplyr::enquo(cols)
  group = dplyr::enquo(group)
 
  missing_count_df = data %>%
    dplyr::group_by(!!group) %>% 
    dplyr::summarize(dplyr::across(!!cols, ~ sum(is.na(.)))) %>% 
    tidyr::pivot_longer(c(!!cols,-!!group),names_to = 'var_name',values_to = 'miss_count')

  non_missing_count_df = data %>%
    dplyr::group_by(!!group) %>% 
    dplyr::summarize(dplyr::across(!!cols, ~ sum(!is.na(.)))) %>% 
    tidyr::pivot_longer(c(!!cols,-!!group),names_to = 'var_name',values_to = 'non_miss_count')
  
  total_value_df = data %>%
    dplyr::group_by(!!group) %>% 
    dplyr::summarize(dplyr::across(!!cols, ~ dplyr::n())) %>% 
    tidyr::pivot_longer(c(!!cols,-!!group),names_to = 'var_name',values_to = 'total_value')
  
  group_name = data %>% dplyr::select(!!group) %>% colnames()
  if (length(group_name) == 0) {
    return_df = total_value_df %>% 
      dplyr::full_join(missing_count_df,by = 'var_name') %>% 
      dplyr::full_join(non_missing_count_df,by = 'var_name') %>% 
      dplyr::mutate(miss_perc = paste0(round(.data$miss_count/.data$total_value*100,3),'%')) %>% 
      dplyr::mutate(non_miss_perc = paste0(round(.data$non_miss_count/.data$total_value*100,3),'%')) %>% 
      dplyr::select(c('var_name','miss_count','non_miss_count','miss_perc','non_miss_perc'))
  
    } else{
    return_df = total_value_df %>% 
      dplyr::full_join(missing_count_df,by = c('var_name',group_name)) %>% 
      dplyr::full_join(non_missing_count_df,by = c('var_name',group_name)) %>% 
      dplyr::mutate(miss_perc = paste0(round(.data$miss_count/.data$total_value*100,3),'%')) %>% 
      dplyr::mutate(non_miss_perc = paste0(round(.data$non_miss_count/.data$total_value*100,3),'%')) %>% 
      dplyr::select(c('var_name',!!group,'miss_count','non_miss_count','miss_perc','non_miss_perc'))
  }
  if (verbose) {print(return_df)} 
  
  if(return_result){return(return_df)}
}
