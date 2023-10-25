#' Z-scored for multilevel analyses
#' It will group mean center and z-scored the L1 score and create a mean score for each L2 group.
#' 
#' @param data dataframe
#' @param effect_coded effect coded variables for L1 group-mean centering
#' @param dummy_coded dummy-coded variables for L2 aggregated means
#' @param group the grouping variable. Must be character
#' @param keep_original default is `FALSE`. Set to `TRUE` to keep original columns
#'
#' @return
#' return a dataframe with the columns z-scored (replace existing columns)
#' @export
#' 
#' @examples
#' iris %>% dplyr::mutate(cluster = rep(c(1,2,3),nrow(.)/3)) %>% effect_coding('Species',factor = FALSE) %>% dummy_coding('Species') %>% z_scored_mlm_categorical(effect_coded = dplyr::ends_with('eff'),dummy_coded = dplyr::ends_with('dum'),group = 'cluster')


z_scored_mlm_categorical = function(data,effect_coded,dummy_coded,group,keep_original = TRUE){
  data_names = names(data)
  effect_coded = data %>% dplyr::select(!!enquo(effect_coded)) %>% names()
  dummy_coded = data %>% dplyr::select(!!enquo(dummy_coded)) %>% names()
  group = enquo(group)
  group_name = data %>% dplyr::select(!!enquo(group)) %>% names()
  
  # group mean
  mean_data = data %>%
    dplyr::group_by(dplyr::across(!!group)) %>% 
    dplyr::summarise(dplyr::across(!!dummy_coded,~mean(.,na.rm = T))) %>%
    dplyr::mutate(dplyr::across(!!dummy_coded, function(x) { (x - mean(x,na.rm = TRUE))/stats::sd(x,na.rm = TRUE)})) %>% 
    dplyr::ungroup() %>% 
    dplyr::rename_with(.fn = ~paste0(.,'_mean_z',recycle0 = T),.cols = !!dummy_coded)
  
  # group-mean center
  original_df = data %>% dplyr::select(!!effect_coded)
  centered_data = data %>%
    dplyr::group_by(dplyr::across(!!group)) %>%
    dplyr::mutate(dplyr::across(!!effect_coded, function(x) { (x - mean(x,na.rm = TRUE))})) %>% 
    dplyr::ungroup() %>% 
    dplyr::rename_with(~ paste(.,'_group_c',sep = ''),!!effect_coded) %>% 
    dplyr::ungroup()
  if (keep_original == TRUE) {
    centered_data = dplyr::bind_cols(centered_data,original_df)
  }
  
  return_data = dplyr::full_join(centered_data,mean_data,by = group_name) %>% 
    dplyr::select(c(dplyr::all_of(data_names),dplyr::ends_with('group_c'),dplyr::ends_with('_mean_z'),dplyr::everything()))
  
  
  return(return_data)    
  
}