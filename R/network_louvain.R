# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Implement the Louvain community detection on a Person to Person network query
#'
#' @description
#' Take a P2P network query and implement the Louvain community detection method. The
#' **igraph** implementation of the Louvain method is used.
#'
#' @param data Data frame containing a Person to Person Network query. Note that this function is
#' computationally intensive and may take a noticeably longer time to process beyond 5000 rows.
#' @param hrvar String containing the HR attribute to be matched in the dataset.
#' Defaults to "Organization".
#' @param bg_fill String to specify background fill colour.
#' @param font_col String to specify font and link colour.
#' @param node_alpha A numeric value between 0 and 1 to specify the transparency of the nodes.
#' @param algorithm String to specify the node placement algorithm to be used. Defaults to "mds" to perform
#' a multidimensional scaling of nodes using a shortest path, which is also a deterministic method.
#' See <https://rdrr.io/cran/ggraph/man/layout_tbl_graph_igraph.html> for a full list of options.
#' @param path File path for saving the PDF output. Defaults to "network_p2p_louvain".
#' Since the network outputs are computationally intensive, the default behaviour is to save time by
#' saving the plot output directly as a PDF in the specified path. To override this behaviour and return
#' a plot object instead, you can pass `NULL` to `path`. What is passed to `path` makes no difference
#' if returning anything other than "plot-louvain" or "plot-hrvar".
#' @param desc_hrvar Character vector of length 3 containing the HR attributes to use when returning the
#' "describe" output. See `network_describe()`.
#'
#' @param return String specifying what output to return. Defaults to "plot-louvain".
#' Valid return options include:
#'   - 'plot-louvain': return a network plot coloured by louvain communities, saving a PDF to `path`.
#'   - 'plot-hrvar': return a network plot coloured by HR attribute, saving a PDF to `path`.
#'   - 'plot-sankey': return a sankey plot combining communities and HR attribute.
#'   - 'table': return a vertex summary table with counts in communities and HR attribute.
#'   - 'data': return a vertex data file that matches vertices with communities and HR attributes.
#'   - 'describe': returns a list of data frames which describe each of the identified communities.
#'   The first data frame is a summary table of all the communities.
#'   - 'network': return igraph object.
#' @param size_threshold Numeric value representing the maximum number of edges before `network_leiden()`
#' switches to use a more efficient, but less elegant plotting method (native igraph). Defaults to 5000.
#' Set as `0` to co-erce to a fast plotting method every time, and `Inf` to always use the default plotting
#' method.
#'
#' @import ggraph
#' @import dplyr
#' @importFrom igraph plot.igraph
#'
#' @export
network_louvain <- function(data,
                            hrvar = "Organization",
                            bg_fill = "#000000",
                            font_col = "#FFFFFF",
                            node_alpha = 0.8,
                            algorithm = "mds",
                            path = "network_p2p_louvain",
                            desc_hrvar = c("Organization", "LevelDesignation", "FunctionType"),
                            return = "plot-louvain",
                            size_threshold = 5000){

  ## Set variables
  TO_hrvar <- paste0("TieOrigin_", hrvar)
  TD_hrvar <- paste0("TieDestination_", hrvar)

  ## Set edges df
  edges <-
    data %>%
    select(from = "TieOrigin_PersonId",
           to = "TieDestination_PersonId",
           weight = "StrongTieScore")

  ## Vertices data frame to provide meta-data
  vert_ft <-
    rbind(
      # TieOrigin
      edges %>%
        select(from) %>% # Single column
        unique() %>% # Remove duplications
        left_join(select(data, TieOrigin_PersonId, TO_hrvar),
                  by = c("from"  = "TieOrigin_PersonId")) %>%
        select(node = "from", !!sym(hrvar) := TO_hrvar),

      # TieDestination
      edges %>%
        select(to) %>% # Single column
        unique() %>% # Remove duplications
        left_join(select(data, TieDestination_PersonId, TD_hrvar),
                  by = c("to"  = "TieDestination_PersonId")) %>%
        select(node = "to", !!sym(hrvar) := TD_hrvar)
    )

  ## Create igraph object
  g_raw <-
    igraph::graph_from_data_frame(edges,
                                  directed = FALSE, # Set to undirected for clustering
                                  vertices = unique(vert_ft)) # remove duplicates

  ## Return a numeric vector of partitions / clusters / modules
  ## Set a low resolution parameter to have fewer groups
  lc <- igraph::cluster_louvain(g_raw)

  ## Add cluster
  g <-
    g_raw %>%
    # Add louvain partitions to graph object
    igraph::set_vertex_attr("cluster", value = as.character(igraph::membership(lc))) %>% # Return membership - diff from Leiden
    igraph::simplify()

  ## Create vertex table
  vertex_tb <-
    g %>%
    igraph::get.vertex.attribute() %>%
    as_tibble()

  g_layout <-
    g %>%
    ggraph::ggraph(layout = "igraph", algorithm = algorithm)

  ## Timestamped File Path
  out_path <- paste0(path, tstamp(), ".pdf")

  ## Return
  if(return == "plot-louvain"){

    if(igraph::ecount(g) > size_threshold){

      message("Using fast plot method due to large network size...")

      ## Set colours
      colour_tb <-
        tibble(cluster = unique(igraph::V(g)$cluster)) %>%
        mutate(colour = rainbow(nrow(.)))

      ## Colour vector
      colour_v <-
        tibble(cluster = igraph::V(g)$cluster) %>%
        left_join(colour_tb, by = "cluster") %>%
        pull(colour)


      igraph::V(g)$color <- grDevices::adjustcolor(colour_v, alpha.f = node_alpha)
      igraph::V(g)$frame.color <- NA
      igraph::E(g)$width <- 1

      grDevices::pdf(out_path)

      par(bg = bg_fill)

      plot(g,
           layout = layout_with_mds,
           vertex.label = NA,
           vertex.size = 3,
           edge.arrow.mode = "-",
           edge.color = "#adadad")

      legend(x = -1.5,
             y = 0.5,
             legend = colour_tb$cluster,
             pch = 21,
             text.col = font_col,
             col = "#777777",
             pt.bg= colour_tb$colour,
             pt.cex = 2,
             cex = .8,
             bty = "n",
             ncol = 1)

      grDevices::dev.off()

    } else {

      plot_output <-
        g_layout +
        ggraph::geom_edge_link(colour = "lightgrey", edge_width = 0.01, alpha = 0.15) +
        ggraph::geom_node_point(aes(colour = cluster),
                                alpha = node_alpha,
                                pch = 16) +
        theme_void() +
        theme(legend.position = "bottom",
              legend.background = element_rect(fill = bg_fill),
              plot.background = element_rect(fill = bg_fill),
              text = element_text(colour = font_col),
              axis.line = element_blank()) +
        labs(caption = "Person to person collaboration with Community Detection
             based on the Louvain algorithm.  ",
             y = "",
             x = "")

      # Default PDF output unless NULL supplied to path
      if(is.null(path)){

        plot_output

      } else {

        ggsave(out_path,
               plot = plot_output,
               width = 16,
               height = 9)

      }

    }

    message(paste0("Saved to ", out_path, "."))


  } else if(return == "plot-hrvar"){

    if(igraph::ecount(g) > size_threshold){

      message("Using fast plot method due to large network size...")

      ## Set colours
      colour_tb <-
        tibble(!!sym(hrvar) := unique(igraph::get.vertex.attribute(g, name = hrvar))) %>%
        mutate(colour = rainbow(nrow(.)))

      ## Colour vector
      colour_v <-
        tibble(!!sym(hrvar) := igraph::get.vertex.attribute(g, name = hrvar)) %>%
        left_join(colour_tb, by = hrvar) %>%
        pull(colour)


      igraph::V(g)$color <- grDevices::adjustcolor(colour_v, alpha.f = node_alpha)
      igraph::V(g)$frame.color <- NA
      igraph::E(g)$width <- 1

      grDevices::pdf(out_path)

      par(bg = bg_fill)

      plot(g,
           layout = layout_with_mds,
           vertex.label = NA,
           vertex.size = 3,
           edge.arrow.mode = "-",
           edge.color = "#adadad")

      legend(x = -1.5,
             y = 0.5,
             legend = colour_tb[[hrvar]],
             pch = 21,
             text.col = font_col,
             col = "#777777",
             pt.bg = colour_tb$colour,
             pt.cex = 2,
             cex = .8,
             bty = "n",
             ncol = 1)

      grDevices::dev.off()

    } else {

      plot_output <-
        g_layout +
        ggraph::geom_edge_link(colour = "lightgrey", edge_width = 0.01, alpha = 0.15) +
        ggraph::geom_node_point(aes(colour = !!sym(hrvar)),
                                alpha = node_alpha,
                                pch = 16) +
        theme_void() +
        theme(legend.position = "bottom",
              legend.background = element_rect(fill = bg_fill),
              plot.background = element_rect(fill = bg_fill),
              text = element_text(colour = font_col),
              axis.line = element_blank()) +
        labs(caption = paste0("Person to person collaboration showing ", hrvar, ".  "), # spaces intentional
             y = "",
             x = "")

      # Default PDF output unless NULL supplied to path
      if(is.null(path)){

        plot_output

      } else {

        ggsave(out_path,
               plot = plot_output,
               width = 16,
               height = 9)

      }

    }

    message(paste0("Saved to ", out_path, "."))

  } else if(return == "table"){

    vertex_tb %>%
      count(!!sym(hrvar), cluster)

  } else if(return == "data"){

    vertex_tb

  } else if(return == "network"){

    g


  } else if(return == "plot-sankey"){

    create_sankey(data = vertex_tb %>% count(!!sym(hrvar), cluster),
                  var1 = hrvar,
                  var2 = "cluster",
                  count = "n")

  } else if(return == "describe"){

    describe_tb <-
      vertex_tb %>%
      left_join(select(data, starts_with("TieOrigin_")),
                by = c("name" = "TieOrigin_PersonId"))

    desc_str <-
      describe_tb %>%
      pull(cluster) %>%
      unique()

    out_list <-
      desc_str %>%
      purrr::map(function(x){
        describe_tb %>%
          filter(cluster == x) %>%
          network_describe(hrvar = desc_hrvar)
      }) %>%
      setNames(nm = desc_str)

    summaryTable <-
      list(i = out_list,
           j = names(out_list)) %>%
      purrr::pmap(function(i, j){
        i %>%
          arrange(desc(Percentage)) %>%
          slice(1) %>%
          mutate_at(vars(starts_with("feature_")), ~tidyr::replace_na(., "")) %>%
          mutate(Community = j,
                 `Attribute 1` = paste(feature_1, "=", feature_1_value),
                 `Attribute 2` = paste(feature_2, "=", feature_2_value),
                 `Attribute 3` = paste(feature_3, "=", feature_3_value)) %>%
          select(Community,
                 `Attribute 1`,
                 `Attribute 2`,
                 `Attribute 3`,
                 PercentageExplained = "Percentage") %>%
          mutate_at(vars(starts_with("Attribute")), ~ifelse(. == " = ", NA, .))
      }) %>%
      bind_rows()

    c(list("summaryTable" = summaryTable), out_list)

  } else {

    stop("Please enter a valid input for `return`.")

  }
}
