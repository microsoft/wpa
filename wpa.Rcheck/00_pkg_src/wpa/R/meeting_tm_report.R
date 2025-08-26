#' @title Generate a Meeting Text Mining report in HTML
#'
#' @description
#' Create a text mining report in HTML based on Meeting Subject Lines
#'
#' @param data A Meeting Query dataset in the form of a data frame.
#' @param path Pass the file path and the desired file name, _excluding the file
#'   extension_. For example, `"meeting text mining report"`.
#' @param stopwords A character vector OR a single-column data frame labelled
#'   `'word'` containing custom stopwords to remove.
#' @param timestamp Logical vector specifying whether to include a timestamp in
#'   the file name. Defaults to TRUE.
#' @param keep A numeric vector specifying maximum number of words to keep.
#' @param seed A numeric vector to set seed for random generation.
#'
#' @family Reports
#' @family Meetings
#' @family Text-mining
#'
#' @inherit generate_report return
#'
#' @export
meeting_tm_report <- function(data,
                              path = "meeting text mining report",
                              stopwords = NULL,
                              timestamp = TRUE,
                              keep = 100,
                              seed = 100){

  ## Check if dependencies are installed
  check_pkg_installed(pkgname = "flexdashboard")

  ## Add timestamp
  if(timestamp == TRUE){
    path <- paste(path, tstamp())
  }

  ## Generate report from RMarkdown
  generate_report2(
    data = data,
    stopwords = stopwords,
    keep = keep,
    seed = seed,
    output_file = paste0(path, ".html"),
    report_title = "Analysis of Meeting Subject Lines",
    rmd_dir = system.file("rmd_template/meeting_tm/meeting_tm_report.rmd", package = "wpa"),
    output_format =
      flexdashboard::flex_dashboard(orientation = "columns",
                                    vertical_layout = "fill",
                                    source_code = "embed"),
  )


#   ## Create timestamped path (if applicable)
#   if(timestamp == TRUE){
#     newpath <- paste(path, wpa::tstamp())
#   } else {
#     newpath <- path
#   }
#
#   # Set outputs
#   output_list <-
#     list(
#       md2html(text = read_preamble("blank.md")), # Header
#       data %>% tm_wordcloud(stopwords = stopwords, keep = keep),
#       data %>% tm_freq(token = "words", stopwords = stopwords, keep = keep),
#       data %>% tm_freq(token = "words", stopwords = stopwords, keep = keep, return = "table"),
#       data %>% tm_freq(token = "ngrams", stopwords = stopwords, keep = keep),
#       data %>% tm_freq(token = "ngrams", stopwords = stopwords, keep = keep, return = "table"),
#       data %>% tm_cooc(stopwords = stopwords, seed = seed),
#       data %>% tm_cooc(stopwords = stopwords, seed = seed, return = "table"),
#       data %>% subject_scan(mode = "days"),
#       data %>% subject_scan(mode = "hours")
#          ) %>%
#     purrr::map_if(is.data.frame, create_dt)
#
#   # Set header titles
#   title_list <-
#     c(
#       "Text Mining Report", # Section header
#       "Word cloud",
#       "Word Frequency",
# 	    "",
#       "Phrase Frequency",
# 	    "",
#       "Word Co-occurrence",
# 	    "",
#       "Top terms",
#       ""
#       )
#
#   # Set header levels
#   n_title <- length(title_list)
#   levels_list <- rep(3, n_title)
#   levels_list[c(1)] <- 2 # Section header
#
#   # Generate report
#   generate_report(title = "Analysis of Meeting Subject Lines",
#                   filename = newpath,
#                   outputs = output_list,
#                   titles = title_list,
#                   subheaders = rep("", n_title),
#                   echos = rep(FALSE, n_title),
#                   levels = levels_list,
#                   theme = "cosmo",
#                   preamble = read_preamble("meeting_tm_report.md"))

}







