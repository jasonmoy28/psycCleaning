#' RMarkdown Render
#'
#' It will render all RMarkdown files in the directory.
#'
#' @param dir directory path
#' @param render_exist render existed pdf files or not
#'
#' @return
#'
#' @export
#'
#' @examples
#'
render_all = function(dir,render_exist = T) {
  files = list.files(dir)
  if (render_exist == F) {
    pdf_files = files[stringr::str_detect(pattern = '.pdf',string = files)]
    pdf_cleaned = stringr::str_replace(pdf_files,'.pdf','')
    files = files[stringr::str_detect(pattern = '.Rmd',string = files)]
    files = files[!stringr::str_detect(pattern = pdf_cleaned,string = files)]
  }
  files = files[stringr::str_detect(pattern = '.Rmd',string = files)]
  print(paste('Rendering', length(files), 'files'))
  for (file in files) {
    file_path = paste(dir,'/',file,sep = '')
    skip_to_next <- FALSE
    tryCatch(rmarkdown::render(file_path), error = function(e) { skip_to_next <<- TRUE})

    if(skip_to_next) { next }
  }
}
