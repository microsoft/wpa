#' @title Generate a report on working patterns in HTML
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' This function takes a Hourly Collaboration query and generates a HTML report
#' on working patterns archetypes. Archetypes are created using the binary-week
#' method.
#'
#' @param data A Hourly Collaboration Query dataset in the form of a data frame.
#' @param hrvar String specifying HR attribute to cut by archetypes. Defaults to
#'   `Organization`.
#' @param signals See `workpatterns_classify()`.
#' @param start_hour See `workpatterns_classify()`.
#' @param end_hour See `workpatterns_classify()`.
#' @param path Pass the file path and the desired file name, _excluding the file
#'   extension_. For example, `"scope report"`.
#' @param timestamp Logical vector specifying whether to include a timestamp in
#'   the file name. Defaults to TRUE.
#'
#' @inherit generate_report return
#'
#' @family Reports
#' @family Working Patterns
#'
#' @importFrom purrr map_if
#' @importFrom methods is
#'
#' @export
workpatterns_report <- function(data,
                                hrvar = "Organization",
                                signals = c("email", "IM"),
                                start_hour = "0900",
                                end_hour = "1700",
                                path = "workpatterns report",
                                timestamp = TRUE){

  ## Pre-empt Date format issues
  data$Date <- as.Date(data$Date, format = "%m/%d/%Y")

  ## Create timestamped path (if applicable)
  if(timestamp == TRUE){
    newpath <- paste(path, wpa::tstamp())
  } else {
    newpath <- path
  }

  ## Get list output from `workpatterns_classify_bw()`
  wp_list <-
    data %>%
    workpatterns_classify_bw(hrvar = hrvar,
                             signals = signals,
                             start_hour = start_hour,
                             end_hour = end_hour,
                             return = "list")

  ## plot for `workpatterns_rank`
  pd_id <-
    wp_list$data %>%
    mutate(PersonWeekId = paste0(PersonId,"_",Date)) %>%
    select(PersonWeekId, Personas)

  ## plot table for `workpatterns_rank`
  plot_table_list <-
    data %>%
    mutate(PersonWeekId = paste0(PersonId,"_",Date)) %>%
    left_join(pd_id, by = "PersonWeekId") %>%
    split(.$Personas)

  plot_rank_list <-
    plot_table_list %>%
    purrr::map(function(x){

      if(nrow(x) == 0){
        "Low base size for this archetype."
      } else {
        workpatterns_rank(x, start_hour = start_hour, end_hour = end_hour, return = "plot")
      }
    })

  ## Create custom bar plot for archetypes
  personas_table <-
    wp_list$data %>%
    dplyr::as_tibble() %>%
    dplyr::group_by(Personas) %>%
    dplyr::summarise(Count = dplyr::n()) %>%
    dplyr::mutate(Percentage = Count / sum(Count, na.rm = TRUE)) %>%
    dplyr::mutate(Percentage= scales::percent(Percentage, accuracy = 0.1))

  personas_plot <-
    personas_table %>%
    ggplot(aes(x = Personas, y = Count)) +
    geom_col(fill = wpa::rgb2hex(0, 120, 212)) +
    geom_text(aes(label = Percentage),
              vjust = -1,
              fontface = "bold",
              size = 4) +
    wpa::theme_wpa_basic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Frequency of Working Patterns",
         subtitle = "Distribution of Cases by Archetype",
         y = "Number of Cases",
         x = "",
         caption = wpa::extract_date_range(data, return = "text")) +
    scale_y_continuous(limits = c(0, max(personas_table$Count) * 1.1))


  ## Set outputs
  output_list <-
    list(
      data %>% wpa::check_query(return = "text"),
      data %>%
        workpatterns_rank(start_hour = start_hour,
                          end_hour = end_hour,
                          return = "plot"),
      personas_plot,
      wp_list$table,
      wp_list$plot_area,
      plot_rank_list[[1]],
      plot_rank_list[[2]],
      plot_rank_list[[3]],
      plot_rank_list[[4]],
      plot_rank_list[[5]],
      plot_rank_list[[6]],
      plot_rank_list[[7]]) %>% # Expand objects to this list
    purrr::map_if(is.data.frame, wpa::create_dt, rounding = 2) %>%
    purrr::map_if(is.character, md2html)

  ## Set header titles
  ## The length must match `output_list`
  title_list <-
    c("Data Overview",
	    "Common Patterns",
      "Archetypes",
      "Split by HR Attribute",
      "Time Dynamics",
      paste(names(plot_table_list)[[1]]),
      paste(names(plot_table_list)[[2]]),
      paste(names(plot_table_list)[[3]]),
      paste(names(plot_table_list)[[4]]),
      paste(names(plot_table_list)[[5]]),
      paste(names(plot_table_list)[[6]]),
      paste(names(plot_table_list)[[7]]))

  ## Set header levels
  ## Makes use of level/header system for Markdown syntax
  n_title <- length(title_list)
  levels_list <- rep(3, n_title) # All chunks to have level 3 header
  levels_list[c(1,2,3)] <- 2 # Set level 2 for section header

  ## Generate report
  generate_report(title = "Working Patterns Report",
                  filename = newpath,
                  outputs = output_list,
                  titles = title_list,
                  subheaders = rep("", n_title),
                  echos = rep(FALSE, n_title),
                  levels = levels_list,
                  theme = "cosmo",
                  preamble = read_preamble("workpatterns_report.md"))
}
