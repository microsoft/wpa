# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title
#' Uncover HR attributes which best represent a population for a Person to
#' Person query
#'
#' @author Tannaz Sattari Tabrizi <Tannaz.Sattari@microsoft.com>
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Returns a data frame that gives a percentage of the group combinations that
#' best represent the population provided. Uses a person to person query. This
#' is used internally within `network_p2p()`.
#'
#' @param data Data frame containing a vertex table output from `network_p2p()`.
#' @param hrvar Character vector of length 3 containing the HR attributes to be
#'   used. Defaults to `c("Organization", "LevelDesignation", "FunctionType")`.
#'
#' @import dplyr
#' @import tidyr
#'
#' @family Network
#'
#' @return data frame. A summary table giving the percentage of group
#' combinations that best represent the provided data.
#'
#' @examples
#' # Simulate a P2P edge list
#' sim_data <- p2p_data_sim()
#'
#' # Perform Louvain Community Detection and return vertices
#' lc_df <-
#'   sim_data %>%
#'   network_p2p(
#'     display = "louvain",
#'     return = "data"
#'   )
#'
#' # Join org data from input edge list
#' joined_df <-
#'   lc_df %>%
#'   dplyr::left_join(
#'     sim_data %>%
#'       dplyr::select(TieOrigin_PersonId,
#'                     TieOrigin_Organization,
#'                     TieOrigin_LevelDesignation,
#'                     TieOrigin_City),
#'     by = c("name" = "TieOrigin_PersonId"))
#'
#' # Describe cluster 2
#' joined_df %>%
#'   # dplyr::filter(cluster == "2") %>%
#'   network_describe(
#'     hrvar = c(
#'       "Organization",
#'       "LevelDesignation",
#'       "City"
#'     )
#'   ) %>%
#'   dplyr::glimpse()
#'
#' @export
network_describe <- function(data,
                             hrvar = c("Organization",
                                       "LevelDesignation",
                                       "FunctionType")){

  if(length(hrvar) != 3){

    stop("Please provide a character vector of length 3 for `hrvar`")

  }

  ## De-duplicated data containing only TieOrigins
  filtered_Data <- unique(select(data, starts_with("TieOrigin_")))

  ## Select features
  features <- select(filtered_Data, paste0("TieOrigin_", hrvar))

  ## Feature set: 1
  max_percentages_1f <-
    features %>%
    colnames() %>%
    purrr::map(function(c){

      agg <-
        features %>%
        group_by_at(.vars = vars(c)) %>%
        summarise(count = n(), .groups = "drop") %>%
        # mutate(percentage = count / sum(count, na.rm = TRUE))
        mutate(percentage = count)

      agg %>%
        arrange(desc(percentage)) %>%
        mutate(feature_1 = c,
               feature_1_value = !!sym(c)) %>%
        select(feature_1, feature_1_value, Percentage = "percentage")
    }) %>%
    bind_rows()

  ## Feature set: 2
  max_percentages_2f <-
    list(c1 = colnames(features),
         c2 = colnames(features)) %>%
    expand.grid(stringsAsFactors = FALSE) %>%
    filter(c1 != c2) %>%
    purrr::pmap(function(c1, c2){
      agg <-
        features %>%
        group_by_at(.vars=vars(c1, c2)) %>%
        summarise(count = n(), .groups = "drop") %>%
        # mutate(percentage = count / sum(count, na.rm = TRUE))
        mutate(percentage = count)

      agg %>%
        arrange(desc(percentage)) %>%
        mutate(feature_1 = c1,
               feature_1_value = !!sym(as.character(c1)),
               feature_2 = c2,
               feature_2_value = !!sym(as.character(c2))) %>%
        select(feature_1,
               feature_1_value,
               feature_2,
               feature_2_value,
               Percentage = "percentage")
    }) %>%
    bind_rows()


  ## Feature set: 3
  max_percentages_3f <-
    list(c1 = colnames(features),
         c2 = colnames(features),
         c3 = colnames(features)) %>%
    expand.grid(stringsAsFactors = FALSE) %>%
    filter(c1 != c2,
           c2 != c3,
           c3 != c1) %>%
    purrr::pmap(function(c1, c2, c3){
      agg <-
        features %>%
        group_by_at(.vars=vars(c1, c2, c3)) %>%
        summarise(count = n(), .groups = "drop") %>%
        # mutate(percentage = count / sum(count, na.rm = TRUE))
        mutate(percentage = count)

      agg %>%
        arrange(desc(percentage)) %>%
        mutate(feature_1 = c1,
               feature_1_value = !!sym(c1),
               feature_2 = c2,
               feature_2_value = !!sym(c2),
               feature_3 = c3,
               feature_3_value = !!sym(c3)) %>%
        select(feature_1,
               feature_1_value,
               feature_2,
               feature_2_value,
               feature_3,
               feature_3_value,
               Percentage = "percentage")
    }) %>%
    bind_rows()

  list(max_percentages_1f,
       max_percentages_2f,
       max_percentages_3f) %>%
    bind_rows() %>%
    select(starts_with("feature"), Percentage) %>%
    # Convert counts to percentage - description as % of community
    mutate(Percentage = Percentage / nrow(filtered_Data))
}
