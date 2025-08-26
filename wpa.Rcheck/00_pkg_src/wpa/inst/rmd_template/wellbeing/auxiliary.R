md2html <- function(text){

  html_chunk <- markdown::markdownToHTML(text = text,
                                         fragment.only = TRUE)

  htmltools::HTML(html_chunk)

}
