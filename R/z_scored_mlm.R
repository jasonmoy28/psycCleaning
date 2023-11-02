#' Z-scored for multilevel analyses
#' 
#' This function will group mean centered the scores at the level 1 and create an aggregated mean score for each group at L2. 
#' After that, the group-mean-centered L1 scores and mean L2 scores will be z-scored with respect to the grand mean. 
#' Please see `center_mlm` if you want to use the version without the z-scoring. 
#' 
#' @param data A data.frame or a data.frame extension (e.g. a tibble).
#' @param cols Columns that need to be centered. See `dplyr::dplyr_tidy_select` for available options. 
#' @param group The grouping/cluster variable.
#' @param keep_original default is `TRUE`. Set to `FALSE` to remove original columns

#' @return
#' An object of the same type as .data. The output has the following properties:
#' 1. Columns from .data will be preserved  
#' 2. Columns with L1 scores that are group-mean centered then grand-mean z-scored.
#' 3. Columns with L2 aggregated means that are z-scored
#' 
#' @export
#'
#' @examples
#' z_scored_mlm(iris,dplyr::ends_with('Length'),group = 'Species')
#'
z_scored_mlm = function(data,cols,group,keep_original = TRUE){
  data_names = names(data)
  cols = data %>% dplyr::select(!!enquo(cols)) %>% names()
  group = enquo(group)
  group_name = data %>% dplyr::select(!!enquo(group)) %>% names()
  
  mean_data = data %>%
    dplyr::group_by(dplyr::across(!!group)) %>% 
    dplyr::summarise(dplyr::across(!!cols,~mean(.,na.rm = TRUE))) %>%
    dplyr::mutate(dplyr::across(!!cols, function(x) { (x - mean(x,na.rm = TRUE))/stats::sd(x,na.rm = TRUE)})) %>% 
    dplyr::ungroup() %>% 
    dplyr::rename_with(.fn = ~paste0(.,'_mean_z',recycle0 = TRUE),.cols = !!cols)
  
  original_df = data %>% dplyr::select(!!cols)
  centered_data = data %>%
    dplyr::group_by(dplyr::across(!!group)) %>%
    dplyr::mutate(dplyr::across(!!cols, function(x) { (x - mean(x,na.rm = TRUE))})) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(dplyr::across(!!cols, function(x) { (x - mean(x,na.rm = TRUE))/stats::sd(x,na.rm = TRUE)})) %>% 
    dplyr::rename_with(~ paste(.,'_group_z',sep = ''),!!cols) %>% 
    dplyr::ungroup()
  if (keep_original == TRUE) {
    centered_data = dplyr::bind_cols(centered_data,original_df)
  }
  
  return_data = dplyr::full_join(centered_data,mean_data,by = group_name) %>% 
    dplyr::select(c(dplyr::all_of(data_names),dplyr::ends_with('group_z'),dplyr::ends_with('_mean'),dplyr::everything()))
  
  
  return(return_data)    
  
}
