# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Generate HTML report with list inputs
#'
#' @description
#' This is a support function using a list-pmap workflow to
#' create a HTML document, using RMarkdown as the engine.
#'
#' @author Martin Chan <martin.chan@@microsoft.com>
#'
#' @param title Character string to specify the title of the chunk.
#' @param filename File name to be used in the exported HTML.
#' @param outputs A list of outputs to be added to the HTML report.
#' Note that `outputs`, `titles`, `echos`, and `levels` must have the same
#' length
#' @param titles A list/vector of character strings to specify the title of the
#'   chunks.
#' @param subheaders A list/vector of character strings to specify the
#'   subheaders for each chunk.
#' @param echos A list/vector of logical values to specify whether to display
#'   code.
#' @param levels A list/vector of numeric value to specify the header level of
#'   the chunk.
#' @param theme Character vector to specify theme to be used for the report.
#' E.g. `"united"`, `"default"`.
#' @param preamble A preamble to appear at the beginning of the report, passed
#'   as a text string.
#'
#' @importFrom purrr pmap
#' @importFrom purrr reduce
#'
#' @family Reports
#'
#' @section Creating a custom report:
#'
#' Below is an example on how to set up a custom report.
#'
#' The first step is to define the content that will go into a report and assign
#' the outputs to a list.
#'
#' ```
#' # Step 1: Define Content
#' output_list <-
#'   list(sq_data %>% workloads_summary(return = "plot"),
#'        sq_data %>% workloads_summary(return = "table")) %>%
#'   purrr::map_if(is.data.frame, create_dt)
#' ```
#'
#' The next step is to add a list of titles for each of the objects on the list:
#'
#' ```
#' # Step 2: Add Corresponding Titles
#' title_list <- c("Workloads Summary - Plot", "Workloads Summary - Table")
#' n_title <- length(title_list)
#' ```
#' The final step is to run `generate_report()`. This can all be wrapped within
#' a function such that the function can be used to generate a HTML report.
#' ```
#' # Step 3: Generate Report
#' generate_report(title = "My First Report",
#'                 filename = "My First Report",
#'                 outputs = output_list,
#'                 titles = title_list,
#'                 subheaders = rep("", n_title),
#'                 echos = rep(FALSE, n_title
#' ```
#' @return
#' An HTML report with the same file name as specified in the arguments is
#' generated in the working directory. No outputs are directly returned by the
#' function.
#'
#' @export
generate_report <- function(title = "My minimal HTML generator",
                            filename = "minimal_html",
                            outputs = output_list,
                            titles,
                            subheaders,
                            echos,
                            levels,
                            theme = "united",
                            preamble = ""){



  ## Title of document
  title_chr <- paste0('title: \"', title, '\"')


  ## chunk loopage
  ## merged to create `chunk_merged`
  chunk_merged <-
    list(output = outputs,
         title = titles,
         subheader = subheaders,
         echo = echos,
         level = levels,
         id = seq(1, length(outputs))) %>%
    purrr::pmap(function(output, title, subheader, echo, level, id){

      generate_chunks(level = level,
                      title = title,
                      subheader = subheader,
                      echo = echo,
                      object = paste0("outputs[[", id, "]]"))

    }) %>%
    purrr::reduce(c)

  # wpa_logo <- system.file("logos/logo.PNG", package = "wpa")

  ## markdown object
  markobj <- c('---',
               title_chr <- paste0('title: \"', title, '\"'),
               'output: ',
               '  html_document:',
               paste0('    theme: ', theme),
               # '    theme: united',
               '    toc: true',
               '    toc_float:',
               '      collapsed: false',
               '      smooth_scroll: true',
               '---',
               # paste0('![]("', wpa_logo, '")'),
               '',
               preamble,
               '',
               chunk_merged)

  writeLines(markobj, paste0(filename, ".Rmd"))
  rmarkdown::render(paste0(filename, ".Rmd"))

  ## Load in browser
  utils::browseURL(paste0(filename, ".html"))

  ## Deletes specified files
  unlink(c(paste0(filename, ".Rmd"),
           paste0(filename, ".md")))


}


#' @title Generate chunk strings
#'
#' @description This is used as a supporting function for `generate_report()`
#' and not directly used. `generate_report()`` works by creating a
#' loop structure around generate_chunks(), and binds them together
#' to create a report.
#'
#' @details
#' `generate_chunks()` is primarily a wrapper around paste() functions,
#'  to create a structured character vector that will form the individual
#'  chunks. No plots 'exist' within the environment of `generate_chunks()`.
#'
#' @param level Numeric value to specify the header level of the chunk.
#' @param title Character string to specify the title of the chunk.
#' @param subheader Character string to specify the subheader of the chunk.
#' @param echo Logical value to specify whether to display code.
#' @param object Character string to specify name of the object to show.
#'
#' @noRd
generate_chunks <- function(level = 3,
                            title,
                            subheader = "",
                            echo = FALSE,
                            object){

  level_hash <- paste(rep('#', level), collapse = "")

  obj <- c(paste(level_hash, title),
           subheader,
           paste0('```{r, echo=',
                  echo,
                  ', fig.height=9, fig.width=12}'),
           object,
           '```',
           ' ')

  return(obj)
}

#' @title Read preamble
#'
#' @description
#' Read in a preamble to be used within each individual reporting function.
#' Reads from the Markdown file installed with the package.
#'
#' @param path Text string containing the path for the appropriate Markdown file.
#'
#' @return
#' String containing the text read in from the specified Markdown file.
#'
#' @family Support
#' @family Reports
#'
#' @export
read_preamble <- function(path){

  full_path <- paste0("/preamble/", path)

  complete_path <- paste0(path.package("wpa"), full_path)

  text <- suppressWarnings(readLines(complete_path))

  return(text)
}

#' Display HTML fragment in RMarkdown chunk, from Markdown text
#'
#' @description
#' This is a wrapper around `markdown::markdownToHTML()`, where
#' the default behaviour is to produce a HTML fragment.
#' `htmltools::HTML()` is then used to evaluate the HTML code
#' within a RMarkdown chunk.
#'
#' @importFrom htmltools HTML
#' @importFrom markdown markdownToHTML
#'
#' @param text Character vector containing Markdown text
#'
#' @family Support
#'
#' @noRd
#'
md2html <- function(text){

  html_chunk <- markdown::markdownToHTML(text = text,
                                         fragment.only = TRUE)

  htmltools::HTML(html_chunk)

}



