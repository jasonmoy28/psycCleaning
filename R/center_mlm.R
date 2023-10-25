#' Centering for multilevel analyses
#' It will group mean center the L1 score and create a mean score for each L2 group.
#' 
#' @param data dataframe
#' @param cols vector or tidyselect syntax or helpers. column(s) that need to be centered
#' @param group the grouping variable. Must be character
#' @param keep_original default is `FALSE`. Set to `TRUE` to keep original columns

#' @return
#' return a dataframe with the columns z-scored (replace existing columns)
#' @export
#'
#' @examples
#' center_mlm(iris,ends_with('Length',group = 'Species')
#'
center_mlm = function(data,cols,group,keep_original = TRUE){
  data_names = names(data)
  cols = data %>% dplyr::select(!!enquo(cols)) %>% names()
  group = enquo(group)
  group_name = data %>% dplyr::select(!!enquo(group)) %>% names()

  mean_data = data %>%
    dplyr::group_by(across(!!group)) %>% 
    dplyr::summarise(dplyr::across(!!cols,~mean(.,na.rm = T))) %>%
    dplyr::mutate(dplyr::across(!!cols, function(x) { (x - mean(x,na.rm = TRUE))})) %>% 
    dplyr::ungroup() %>% 
    dplyr::rename_with(.fn = ~paste0(.,'_mean',recycle0 = T),.cols = !!cols)

  original_df = data %>% dplyr::select(!!cols)
  centered_data = data %>%
    dplyr::group_by(dplyr::across(!!group)) %>%
    dplyr::mutate(dplyr::across(!!cols, function(x) { (x - mean(x,na.rm = TRUE))})) %>% 
    dplyr::rename_with(~ paste(.,'_group_c',sep = ''),!!cols) %>% 
    dplyr::ungroup()
  if (keep_original == TRUE) {
    centered_data = dplyr::bind_cols(centered_data,original_df)
  }
  
  return_data = dplyr::full_join(centered_data,mean_data,by = group_name) %>% 
    dplyr::select(c(all_of(data_names),dplyr::ends_with('group_c'),dplyr::ends_with('_mean'),dplyr::everything()))
  
  
  return(return_data)    
  
}