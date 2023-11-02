#' Z-scored for multilevel analyses
#' 
#' This is a specialized function for mean centering categorical variables. There are two cases where this function should be used instead of the generic `center_mlm`. 
#' 1. This function should be used when you need group mean centering for non-dummy-coded variables at L1. Variables at L2 are always dummy-coded as they represent the percentage of subjects in that group.
#' 2. This function should be used whenever you want to z-score the aggregated L2 means
#' 
#' @param data A data.frame or a data.frame extension (e.g. a tibble).
#' @param cols Dummy-coded or effect-coded columns for group-mean centering. Support `dplyr::dplyr_tidy_select` options. 
#' @param dummy_coded Dummy-coded variables (cannot be effect-coded) for L2 aggregated means. Support `dplyr::dplyr_tidy_select` options. 
#' @param group the grouping variable. Must be character
#' @param keep_original default is `FALSE`. Set to `TRUE` to keep original columns
#' @export
#' @return
#' An object of the same type as .data. The output has the following properties:
#' 1. Columns from .data will be preserved
#' 2. Columns with L1 scores that are group-mean centered 
#' 3. Columns with L2 aggregated means (i.e., percentage) that are z-scored
#' 
#' @examples z_scored_mlm_categorical(mlbook_data,'female_eff','female_dum','schoolnr')


z_scored_mlm_categorical = function(data,cols,dummy_coded = NA,group,keep_original = TRUE){
  try({if (is.na(dummy_coded)) {
    dummy_coded = cols
    dummy_coded = data %>% dplyr::select(!!enquo(dummy_coded)) %>% names()
    warning('Cols are presumed to be dummy-coded')
  }})
  
  data_names = names(data)
  cols = data %>% dplyr::select(!!enquo(cols)) %>% names()
  dummy_coded = data %>% dplyr::select(!!enquo(dummy_coded)) %>% names()
  group = enquo(group)
  group_name = data %>% dplyr::select(!!enquo(group)) %>% names()
  
  
  # aggregated group mean
  mean_data = data %>%
    dplyr::group_by(dplyr::across(!!group)) %>% 
    dplyr::summarise(dplyr::across(!!dummy_coded,~mean(.,na.rm = T))) %>%
    dplyr::mutate(dplyr::across(!!dummy_coded, function(x) { (x - mean(x,na.rm = TRUE))/stats::sd(x,na.rm = TRUE)})) %>% 
    dplyr::ungroup() %>% 
    dplyr::rename_with(.fn = ~paste0(.,'_mean_z',recycle0 = T),.cols = !!dummy_coded)
  
  # group-mean center
  original_df = data %>% dplyr::select(!!cols)
  centered_data = data %>%
    dplyr::group_by(dplyr::across(!!group)) %>%
    dplyr::mutate(dplyr::across(!!cols, function(x) { (x - mean(x,na.rm = TRUE))})) %>% 
    dplyr::ungroup() %>% 
    dplyr::rename_with(~ paste(.,'_group_c',sep = ''),!!cols) %>% 
    dplyr::ungroup()
  if (keep_original == TRUE) {
    centered_data = dplyr::bind_cols(centered_data,original_df)
  }
  
  return_data = dplyr::full_join(centered_data,mean_data,by = group_name) %>% 
    dplyr::select(c(dplyr::all_of(data_names),dplyr::ends_with('group_c'),dplyr::ends_with('_mean_z'),dplyr::everything()))
  
  
  return(return_data)    
  
}