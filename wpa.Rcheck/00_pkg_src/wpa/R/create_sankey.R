# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Create a sankey chart from a two-column count table
#'
#' @description
#' Create a 'networkD3' style sankey chart based on a long count table
#' with two variables. The input data should have three columns, where
#' each row is a unique group:
#'   1. Variable 1
#'   2. Variable 2
#'   3. Count
#'
#' @param data Data frame of the long count table.
#' @param var1 String containing the name of the variable to be shown on the
#'   left.
#' @param var2 String containing the name of the variable to be shown on the
#'   right.
#' @param count String containing the name of the count variable.
#'
#' @import dplyr
#'
#' @return A 'sankeyNetwork' and 'htmlwidget' object containing a two-tier
#'  sankey plot. The output can be saved locally with
#'  `htmlwidgets::saveWidget()`.
#'
#' @examples
#' \donttest{
#' sq_data %>%
#'   dplyr::count(Organization, FunctionType) %>%
#'   create_sankey(var1 = "Organization", var2 = "FunctionType")
#' }
#'
#' @family Visualization
#' @family Flexible
#'
#' @export
create_sankey <- function(data, var1, var2, count = "n"){

  ## Rename
  data$pre_group <- data[[var1]]
  data$group <- data[[var2]]

  ## Set up `nodes`
  group_source <- unique(data$pre_group)
  group_target <- paste0(unique(data$group), " ")

  groups <- c(group_source, group_target)

  nodes_source <- tibble(name = group_source)
  nodes_target <- tibble(name = group_target)
  nodes <- rbind(nodes_source, nodes_target) %>% mutate(node = 0:(nrow(.) - 1))

  ## Set up `links`
  links <-
    data %>%
    mutate(group = paste0(group, " ")) %>%
    select(source = "pre_group",
           target = "group",
           value = count)

  nodes_source <- nodes_source %>% select(name) # Make `nodes` a single column data frame
  nodes_target <- nodes_target %>% select(name) # Make `nodes` a single column data frame

  links <-
    links %>%
    left_join(nodes %>% rename(IDsource = "node"), by = c("source" = "name")) %>%
    left_join(nodes %>% rename(IDtarget = "node"), by = c("target" = "name"))


  networkD3::sankeyNetwork(Links = as.data.frame(links),
                           Nodes = as.data.frame(nodes),
                           Source = 'IDsource', # Change reference to IDsource
                           Target = 'IDtarget', # Change reference to IDtarget
                           Value = 'value',
                           NodeID = 'name',
                           units="count",
                           sinksRight = FALSE)
}
