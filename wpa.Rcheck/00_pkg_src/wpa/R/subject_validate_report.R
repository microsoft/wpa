# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Generate Meeting Text Mining report in HTML for Common Exclusion Terms
#'
#' @description This functions creates a text mining report in HTML based on
#' Meeting Subject Lines for data validation. It scans a meeting query and
#' highlights meetings with subjects that include common exlusion terms. It is
#' intended to be used by an analyst to validate raw data before conducting
#' additional analysis. Returns a HTML report by default.
#'
#' @family Data Validation
#' @family Text-mining
#' @family Reports
#'
#' @param data A Meeting Query dataset in the form of a data frame.
#' @param path Pass the file path and the desired file name, _excluding the file
#'   extension_. For example, `"meeting text mining report"`.
#' @param timestamp Logical vector specifying whether to include a timestamp in
#'   the file name. Defaults to `TRUE`.
#' @param keep A numeric vector specifying maximum number of words to keep.
#' @param seed A numeric vector to set seed for random generation.
#'
#'
#' @inherit generate_report return
#'
#' @export
subject_validate_report <- function(data,
                              path = "Subject Lines Validation Report",
                              timestamp = TRUE,
                              keep = 100,
                              seed = 100){

  ## Create timestamped path (if applicable)
  if(timestamp == TRUE){
    newpath <- paste(path, wpa::tstamp())
  } else {
    newpath <- path
  }

# Get Results
test_data <- data %>% subject_validate(return="data") %>% filter(subjectFlag==1)
results <- data %>% subject_validate(return="table")

# Set outputs
  output_list <-
    list(data %>% subject_validate(return="table"),
         test_data %>% tm_wordcloud(),
         test_data %>% tm_freq(token = "words"),
         test_data %>% tm_freq(token = "words", return = "table"),
         test_data %>% tm_freq(token = "ngrams"),
         test_data %>% tm_freq(token = "ngrams", return = "table"),
         test_data %>% tm_cooc(),
         test_data %>% tm_cooc(return="table")) %>%
    purrr::map_if(is.data.frame, create_dt)

  # Set header titles
  title_list <-
    c("Exclusion Terms Identified",
	  "Related Words",
      "",
	  "",
      "Common Phrases",
	  "",
      "Word Co-occurrence",
	  "")

  # Set header levels
  n_title <- length(title_list)
  levels_list <- rep(3, n_title)

  # Generate report
  generate_report(title = "Subject Lines Validation Report",
                  filename = newpath,
                  outputs = output_list,
                  titles = title_list,
                  subheaders = rep("", n_title),
                  echos = rep(FALSE, n_title),
                  levels = levels_list,
                  theme = "cosmo",
                  preamble = "")
}

