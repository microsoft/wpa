# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Identify Non-Knowledge workers in a Person Query using Collaboration Hours
#'
#' @description
#' This function scans a standard query output to identify employees with consistently low collaboration signals.
#' Returns the % of non-knowledge workers identified by Organization, and optionally an edited dataframe with non-knowledge workers removed, or the full dataframe with the kw/nkw flag added.
#'
#' @param data A Standard Person Query dataset in the form of a data frame.
#' @param collab_threshold The collaboration hours threshold that should be exceeded as an average for the entire analysis period for the employee to be categorized as a knowledge worker ("kw").
#' Enter a positive number. Default is set to 5 collaboration hours.
#' @param return String to specify what to return
#'
#' @import dplyr
#'
#' @family Data Validation
#'
#' @return
#' Returns an error message by default, where 'text' is passed in `return`.
#' When 'data_with_flag' is passed, a copy of your original dataframe with an additional column containing the kw/nkw flag will be returned.
#' When 'data_clean' is passed, a full dataframe excluding "nkw" employees will be returned.
#' when 'data_summmary' is passed, a summary table by organization listing the number and % of non-knowledge workers will be returned.
#'
#' @export
identify_nkw <- function(data, collab_threshold = 5, return = "data_summary"){

  options(dplyr.summarise.inform = FALSE)

  summary_byPersonId <-
    data %>%
    group_by(PersonId, Organization) %>%
    summarize(mean_collab = mean(Collaboration_hours))%>%
    mutate(flag_nkw = case_when(mean_collab > collab_threshold ~ "kw",
                                TRUE ~ "nkw"))

  data_with_flag <- left_join(data,
                              summary_byPersonId %>%
                                dplyr::select(PersonId,flag_nkw), by = 'PersonId')

  summary_byOrganization <-
    summary_byPersonId %>%
    group_by(Organization, flag_nkw)%>%
    summarise(total = n())%>%
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

    return(data_with_flag %>% filter(flag_nkw == "kw") %>% data.frame())

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









