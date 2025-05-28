# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Identify Non-Knowledge workers in a Person Query using Collaboration
#'   Hours
#'
#' @description
#' This function scans a standard query output to identify employees with
#' consistently low collaboration signals. Returns the % of non-knowledge
#' workers identified by Organization, and optionally an edited data frame with
#' non-knowledge workers removed, or the full data frame with the kw/nkw flag
#' added.
#'
#' @param data A Standard Person Query dataset in the form of a data frame.
#' @param collab_threshold Positive numeric value representing the collaboration
#'   hours threshold that should be exceeded as an average for the entire
#'   analysis period for the employee to be categorized as a knowledge worker
#'   ("kw"). Default is set to 5 collaboration hours. Any versions after v1.4.3,
#'   this uses a "greater than or equal to" logic (`>=`), in which case persons
#'   with exactly 5 collaboration hours will pass.
#' @param return String specifying what to return. This must be one of the
#'   following strings:
#'   - `"text"`
#'   - `"data_with_flag"`
#'   - `"data_clean"`
#'   - `"data_summary"`
#'
#' See `Value` for more information.
#'
#' @import dplyr
#'
#' @family Data Validation
#'
#' @return
#' A different output is returned depending on the value passed to the `return`
#' argument:
#'   - `"text"`: string. Returns a diagnostic message.
#'   - `"data_with_flag"`: data frame. Original input data with an additional
#'   column containing the `kw`/`nkw` flag.
#'   - `"data_clean"`: data frame. Data frame with non-knowledge workers
#'   excluded.
#'   - `"data_summary"`: data frame. A summary table by organization listing
#'   the number and % of non-knowledge workers.
#'
#' @export
identify_nkw <- function(data, collab_threshold = 5, return = "data_summary"){

  summary_byPersonId <-
    data %>%
    group_by(PersonId, Organization) %>%
    summarize(mean_collab = mean(Collaboration_hours), .groups = "drop")%>%
    mutate(flag_nkw = case_when(mean_collab >= collab_threshold ~ "kw",
                                TRUE ~ "nkw"))

  data_with_flag <-
    left_join(data,
              summary_byPersonId %>%
                dplyr::select(PersonId,flag_nkw),
              by = 'PersonId')

  summary_byOrganization <-
    summary_byPersonId %>%
    group_by(Organization, flag_nkw)%>%
    summarise(total = n(), .groups = "drop")%>%
    group_by(Organization)%>%
    mutate(perc = total/sum(total))%>% #need to format to %
    filter(flag_nkw == "nkw")%>%
    rename(n_nkw = total, perc_nkw = perc)%>%
    select(-flag_nkw) %>%
    ungroup()

  ## Number of NKW identified
  n_nkw <- sum(summary_byPersonId$flag_nkw == "nkw")


  if(n_nkw == 0){
    flagMessage <- paste0("[Pass] There are no non-knowledge workers identified",
                          " (average collaboration hours below ",
                          collab_threshold,
                          " hours).")
  } else {
    flagMessage <-
      paste0("[Warning] Out of a population of ", n_distinct(data$PersonId),
             ", there are ", n_nkw,
             " employees who may be non-knowledge workers (average collaboration hours below ",
             collab_threshold, " hours).")
  }

   if(return == "data_with_flag"){
    return(data_with_flag)

  } else if(return %in% c("data_clean", "data_cleaned")){

    data_with_flag %>%
      filter(flag_nkw == "kw")

  } else if(return == "text"){

    flagMessage

  } else if(return =="data_summary"){

    summary_byOrganization %>%
      mutate(perc_nkw = scales::percent(perc_nkw, accuracy = 1)) %>%
      rename(`Non-knowledge workers (count)` = "n_nkw",
             `Non-knowledge workers (%)` = "perc_nkw")

  } else {
    stop("Error: please check inputs for `return`")
  }
}









