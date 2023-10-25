#' Effect Coding
#'
#' @param data data.frame object 
#' @param cols vector or tidyselect syntax or helpers. column(s) that need to be re-coded
#' @param factor If factor = `TRUE`, this function return a `tibble` with effect coded factors. If factor = `FALSE`, this function return a tibble with effect coded columns. 
#' @return
#' return a data.frame with factors being effect coded
#' @export
#'
#' @examples
#' effect_coding(iris,Species)
effect_coding = function(data,cols,factor = TRUE){
  if (factor == TRUE) {
    names = data %>% dplyr::select(!!enquo(cols)) %>% colnames(.)
    return_df = data %>% dplyr::mutate(dplyr::across(dplyr::all_of(names),~as.factor(.)))
    for (name in names) {
      stats::contrasts(return_df[[name]]) = stats::contr.sum(length(levels(return_df[[name]])))
    }
    return(return_df) 
  } else if(factor == FALSE){
    cols_names = data %>% select(!!enquo(cols)) %>% names()
    return_df = data
    for (group in cols_names) {
      group_name = data %>% select(!!enquo(group)) %>% names()
      distinct_group = data %>% distinct(across(!!enquo(group))) %>% pull(.) %>% as.character()
      distinct_group_num = data %>% distinct(across(!!enquo(group))) %>% nrow()
      contrasts_df = data.frame(contr.sum(distinct_group_num))
      colnames(contrasts_df) = paste0(group_name,'_',distinct_group[1:nrow(contrasts_df)-1],'_eff')
      dummy_coded_df = contrasts_df %>% 
        mutate(group = distinct_group) %>% 
        rename(!!enquo(group):=group)
      return_df = full_join(return_df,dummy_coded_df)
    }
    return(return_df)
    
  }
}

