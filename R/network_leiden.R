# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Implement the Leiden community detection on a Person to Person network query
#'
#' @description
#' Take a P2P network query and implement the Leiden community detection method. To run
#' this function, you will require all the pre-requisites of the **leiden** package installed,
#' which includes Python and **reticulate**.
#'
#' @inheritParams network_p2p
#'
#' @param return
#' String specifying what output to return. Defaults to "plot-leiden". Valid return options include:
#'   - 'plot-leiden': return a network plot coloured by leiden communities, saving a PDF to path.
#'   - 'plot-hrvar': return a network plot coloured by HR attribute, saving a PDF to path.
#'   - 'plot-sankey': return a sankey plot combining communities and HR attribute.
#'   - 'table': return a vertex summary table with counts in communities and HR attribute.
#'   - 'data': return a vertex data file that matches vertices with communities and HR attributes.
#'   - 'describe': return a list of data frames which describe each of the identified communities.
#'     The first data frame is a summary table of all the communities.
#'   - 'network': return igraph object.
#'
#' @examples
#' \dontrun{
#' ## Simulate a small person-to-person dataset
#' p2p_data <- p2p_data_sim(size = 50)
#'
#' ## Return leiden, console, plot
#' p2p_data %>%
#'   network_leiden(path = NULL,
#'                  return = "plot")
#' }
#'
#'
#' @export
network_leiden <- function(data,
                           hrvar = "Organization",
                           bg_fill = "#000000",
                           font_col = "#FFFFFF",
                           algorithm = "mds",
                           path = "network_p2p_leiden",
                           node_alpha = 0.8,
                           res = 0.5,
                           seed = 1,
                           desc_hrvar = c("Organization", "LevelDesignation", "FunctionType"),
                           return = "plot-leiden",
                           size_threshold = 5000){

  ## Default value for display
  display <- "leiden"

  ## Make code backward compatible

  if(grepl(pattern = "plot-", x = return)){

    return <- gsub(pattern = "-",
                   replacement = "",
                   x = return)

    if(return %in% c("leiden", "hrvar")){

      display <- return # Either "leiden" or "hrvar"
      return <- "network"

    }

  }


  ## Wrapper
  network_p2p(data = data,
              hrvar = hrvar,
              display = display,
              return = return,
              path = path,
              desc_hrvar = desc_hrvar,
              bg_fill = bg_fill,
              font_col = font_col,
              node_alpha = node_alpha,
              res = res, # Leiden specific
              seed = seed, # Leiden specific
              algorithm = algorithm,
              size_threshold = size_threshold)

  # ## Set variables
  # TO_hrvar <- paste0("TieOrigin_", hrvar)
  # TD_hrvar <- paste0("TieDestination_", hrvar)
  #
  # ## Set edges df
  # edges <-
  #   data %>%
  #   select(from = "TieOrigin_PersonId",
  #          to = "TieDestination_PersonId",
  #          weight = "StrongTieScore")
  #
  # ## Vertices data frame to provide meta-data
  # vert_ft <-
  #   rbind(
  #     # TieOrigin
  #     edges %>%
  #       select(from) %>% # Single column
  #       unique() %>% # Remove duplications
  #       left_join(select(data, TieOrigin_PersonId, TO_hrvar),
  #                 by = c("from"  = "TieOrigin_PersonId")) %>%
  #       select(node = "from", !!sym(hrvar) := TO_hrvar),
  #
  #     # TieDestination
  #     edges %>%
  #       select(to) %>% # Single column
  #       unique() %>% # Remove duplications
  #       left_join(select(data, TieDestination_PersonId, TD_hrvar),
  #                 by = c("to"  = "TieDestination_PersonId")) %>%
  #       select(node = "to", !!sym(hrvar) := TD_hrvar)
  #   )
  #
  # ## Create igraph object
  # g_raw <-
  #   igraph::graph_from_data_frame(edges,
  #                                 directed = TRUE, # Directed, but FALSE for visualization
  #                                 vertices = unique(vert_ft)) # remove duplicates
  #
  # ## Return a numeric vector of partitions / clusters / modules
  # ## Set a low resolution parameter to have fewer groups
  # ld <- leiden::leiden(g_raw, resolution_parameter = res, seed = seed) # create partitions
  #
  # ## Add cluster
  # g <-
  #   g_raw %>%
  #   # Add leiden partitions to graph object
  #   igraph::set_vertex_attr("cluster", value = as.character(ld)) %>%
  #   igraph::simplify()
  #
  # ## Create vertex table
  # vertex_tb <-
  #   g %>%
  #   igraph::get.vertex.attribute() %>%
  #   as_tibble()
  #
  # g_layout <-
  #   g %>%
  #   ggraph::ggraph(layout = "igraph", algorithm = algorithm)
  #
  # ## Timestamped File Path
  # out_path <- paste0(path, tstamp(), ".pdf")
  #
  # ## Return
  # if(return == "plot-leiden"){
  #
  #   if(igraph::ecount(g) > size_threshold){
  #
  #     message("Using fast plot method due to large network size...")
  #
  #     ## Set colours
  #     colour_tb <-
  #       tibble(cluster = unique(igraph::V(g)$cluster)) %>%
  #       mutate(colour = rainbow(nrow(.)))
  #
  #     ## Colour vector
  #     colour_v <-
  #       tibble(cluster = igraph::V(g)$cluster) %>%
  #       left_join(colour_tb, by = "cluster") %>%
  #       pull(colour)
  #
  #
  #     igraph::V(g)$color <- grDevices::adjustcolor(colour_v, alpha.f = node_alpha)
  #     igraph::V(g)$frame.color <- NA
  #     igraph::E(g)$width <- 1
  #
  #     plot_cluster <- function(){
  #
  #       par(bg = bg_fill)
  #
  #       plot(g,
  #            layout = igraph::layout_with_mds,
  #            vertex.label = NA,
  #            vertex.size = 3,
  #            edge.arrow.mode = "-",
  #            edge.color = "#adadad")
  #
  #       legend(x = -1.5,
  #              y = 0.5,
  #              legend = colour_tb$cluster,
  #              pch = 21,
  #              text.col = font_col,
  #              col = "#777777",
  #              pt.bg= colour_tb$colour,
  #              pt.cex = 2,
  #              cex = .8,
  #              bty = "n",
  #              ncol = 1)
  #
  #     }
  #
  #     # Default PDF output unless NULL supplied to path
  #     if(is.null(path)){
  #
  #       plot_cluster()
  #
  #     } else {
  #
  #       grDevices::pdf(out_path)
  #
  #       plot_cluster()
  #
  #       message(paste0("Saved to ", out_path, "."))
  #
  #     }
  #
  #   } else {
  #
  #     plot_output <-
  #       g_layout +
  #       ggraph::geom_edge_link(colour = "lightgrey", edge_width = 0.01, alpha = 0.15) +
  #       ggraph::geom_node_point(aes(colour = cluster),
  #                               alpha = node_alpha,
  #                               pch = 16) +
  #       theme_void() +
  #       theme(legend.position = "bottom",
  #             legend.background = element_rect(fill = bg_fill),
  #             plot.background = element_rect(fill = bg_fill),
  #             text = element_text(colour = font_col),
  #             axis.line = element_blank()) +
  #       labs(caption = "Person to person collaboration with Community Detection
  #            based on the Leiden algorithm.  ",
  #            y = "",
  #            x = "")
  #
  #     # Default PDF output unless NULL supplied to path
  #     if(is.null(path)){
  #
  #       plot_output
  #
  #     } else {
  #
  #       ggsave(paste0(path, tstamp(), ".pdf"),
  #              plot = plot_output,
  #              width = 16,
  #              height = 9)
  #
  #       message(paste0("Saved to ", out_path, "."))
  #
  #     }
  #   }
  #
  # } else if(return == "plot-hrvar"){
  #
  #   if(igraph::ecount(g) > size_threshold){
  #
  #     message("Using fast plot method due to large network size...")
  #
  #     ## Set colours
  #     colour_tb <-
  #       tibble(!!sym(hrvar) := unique(igraph::get.vertex.attribute(g, name = hrvar))) %>%
  #       mutate(colour = rainbow(nrow(.)))
  #
  #     ## Colour vector
  #     colour_v <-
  #       tibble(!!sym(hrvar) := igraph::get.vertex.attribute(g, name = hrvar)) %>%
  #       left_join(colour_tb, by = hrvar) %>%
  #       pull(colour)
  #
  #
  #     igraph::V(g)$color <- grDevices::adjustcolor(colour_v, alpha.f = node_alpha)
  #     igraph::V(g)$frame.color <- NA
  #     igraph::E(g)$width <- 1
  #
  #     plot_hrvar <- function(){
  #
  #       par(bg = bg_fill)
  #
  #       plot(g,
  #            layout = igraph::layout_with_mds,
  #            vertex.label = NA,
  #            vertex.size = 3,
  #            edge.arrow.mode = "-",
  #            edge.color = "#adadad")
  #
  #       legend(x = -1.5,
  #              y = 0.5,
  #              legend = colour_tb[[hrvar]],
  #              pch = 21,
  #              text.col = font_col,
  #              col = "#777777",
  #              pt.bg = colour_tb$colour,
  #              pt.cex = 2,
  #              cex = .8,
  #              bty = "n",
  #              ncol = 1)
  #
  #     }
  #
  #     # Default PDF output unless NULL supplied to path
  #     if(is.null(path)){
  #
  #       plot_hrvar()
  #
  #     } else {
  #
  #       grDevices::pdf(out_path)
  #
  #       plot_hrvar()
  #
  #       grDevices::dev.off()
  #
  #       message(paste0("Saved to ", out_path, "."))
  #
  #     }
  #
  #   } else {
  #
  #     plot_output <-
  #       g_layout +
  #       ggraph::geom_edge_link(colour = "lightgrey", edge_width = 0.01, alpha = 0.15) +
  #       ggraph::geom_node_point(aes(colour = !!sym(hrvar)),
  #                               alpha = node_alpha,
  #                               pch = 16) +
  #       theme_void() +
  #       theme(legend.position = "bottom",
  #             legend.background = element_rect(fill = bg_fill),
  #             plot.background = element_rect(fill = bg_fill),
  #             text = element_text(colour = font_col),
  #             axis.line = element_blank()) +
  #       labs(caption = paste0("Person to person collaboration showing ", hrvar, ".  "), # spaces intentional
  #            y = "",
  #            x = "")
  #
  #     # Default PDF output unless NULL supplied to path
  #     if(is.null(path)){
  #
  #       plot_output
  #
  #     } else {
  #
  #       ggsave(paste0(path, tstamp(), ".pdf"),
  #              plot = plot_output,
  #              width = 16,
  #              height = 9)
  #
  #       message(paste0("Saved to ", out_path, "."))
  #
  #     }
  #
  #   }
  #
  # } else if(return == "table"){
  #
  #   vertex_tb %>%
  #     count(!!sym(hrvar), cluster)
  #
  # } else if(return == "data"){
  #
  #   vertex_tb
  #
  # } else if(return == "network"){
  #
  #   g
  #
  #
  # } else if(return == "plot-sankey"){
  #
  #   create_sankey(data = vertex_tb %>% count(!!sym(hrvar), cluster),
  #                 var1 = hrvar,
  #                 var2 = "cluster",
  #                 count = "n")
  #
  # } else if(return == "describe"){
  #
  #   describe_tb <-
  #     vertex_tb %>%
  #     left_join(select(data, starts_with("TieOrigin_")),
  #               by = c("name" = "TieOrigin_PersonId"))
  #
  #   desc_str <-
  #     describe_tb %>%
  #     pull(cluster) %>%
  #     unique()
  #
  #   out_list <-
  #     desc_str %>%
  #     purrr::map(function(x){
  #       describe_tb %>%
  #         filter(cluster == x) %>%
  #         network_describe(hrvar = desc_hrvar)
  #     }) %>%
  #     setNames(nm = desc_str)
  #
  #   summaryTable <-
  #     list(i = out_list,
  #        j = names(out_list)) %>%
  #     purrr::pmap(function(i, j){
  #       i %>%
  #         arrange(desc(Percentage)) %>%
  #         slice(1) %>%
  #         mutate_at(vars(starts_with("feature_")), ~tidyr::replace_na(., "")) %>%
  #         mutate(Community = j,
  #                `Attribute 1` = paste(feature_1, "=", feature_1_value),
  #                `Attribute 2` = paste(feature_2, "=", feature_2_value),
  #                `Attribute 3` = paste(feature_3, "=", feature_3_value)) %>%
  #         select(Community,
  #                `Attribute 1`,
  #                `Attribute 2`,
  #                `Attribute 3`,
  #                PercentageExplained = "Percentage") %>%
  #         mutate_at(vars(starts_with("Attribute")), ~ifelse(. == " = ", NA, .))
  #     }) %>%
  #     bind_rows()
  #
  #   c(list("summaryTable" = summaryTable), out_list)
  #
  # } else {
  #
  #   stop("Please enter a valid input for `return`.")
  #
  # }
}
