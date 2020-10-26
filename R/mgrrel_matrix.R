#' @title Manager Relationship 2x2 Matrix
#'
#' @description
#' Generate the Manager-Relationship 2x2 matrix, returning a ggplot object by default.
#' Additional options available to return a "wide" or "long" summary table.
#'
#' @param data Standard Query data to pass through. Accepts a data frame.
#' @param hrvar HR Variable by which to split metrics. Accepts a character vector,
#' e.g. "Organization". Defaults to NULL.
#' @param return A character vector specifying whether to return a matrix plot or a table.
#' Defaults to the 2 by 2 matrix. Valid values are "plot", "table", and "chartdata".
#' @param plot_colors
#' Pass a character vector of length 4 containing HEX codes to specify colors to use in plotting.
#' @param threshold
#' Specify a numeric value to determine threshold (in minutes) for 1:1 manager hours.
#' Defaults to 15.
#'
#' @import dplyr
#' @import reshape2
#' @import ggplot2
#' @importFrom scales percent
#'
#' @family Managerial Relations
#' @family Meeting Culture
#'
#' @examples
#' mgrrel_matrix(sq_data)
#'
#' @export
mgrrel_matrix <- function(data,
                          hrvar = NULL,
                          return = "plot",
                          plot_colors = c("#FE7F4F", "#C3E5FF", "#FFD4C4", "#FEAA8A"),
                          threshold = 15){

  ## Add dummy "Total" column if hrvar = NULL
  if(is.null(hrvar)){

      data <- mutate(data, Total = "Total")
      hrvar <- "Total"

    }


  ## Check inputs
  required_variables <- c("Date",
                          hrvar,
                          "PersonId",
                          "Meeting_hours_with_manager",
                          "Meeting_hours",
                          "Meeting_hours_with_manager_1_on_1")

  ## Error message if variables are not present
  ## Nothing happens if all present
  data %>%
    check_inputs(requirements = required_variables)

  ## Create a Person Weekly Average
  data1 <-
    data %>%
    mutate(coattendman_rate = Meeting_hours_with_manager / Meeting_hours) %>% # Coattendance Rate with Manager
    filter(!is.na(coattendman_rate)) %>%
    group_by(PersonId, !!sym(hrvar)) %>%
    summarise_at(vars(Meeting_hours_with_manager,
                      Meeting_hours,
                      Meeting_hours_with_manager_1_on_1,
                      coattendman_rate),
                 ~mean(.)) %>%
    ungroup()


  ## Threshold
  thres_low_chr <- paste("<", threshold, "min")
  thres_top_chr <- paste(">", threshold, "min")


  ## Create key variables
  data2 <-
    data1 %>%
    mutate(coattendande = ifelse(coattendman_rate < 0.5, "<50%", ">50%"),
           mgr1on1 = ifelse(Meeting_hours_with_manager_1_on_1 * 60 < threshold,
                            thres_low_chr,
                            thres_top_chr))


  ## Grouping variable split
  if(hrvar == "Total"){
    data2 %>%
      count(mgr1on1, coattendande) %>%
      mutate(perc = n / sum(n)) %>% # Calculate percentages
      mutate(xmin = ifelse(mgr1on1 == thres_low_chr, -sqrt(perc), 0),
             xmax = ifelse(mgr1on1 == thres_top_chr, sqrt(perc), 0),
             ymin = ifelse(coattendande == "<50%", -sqrt(perc), 0),
             ymax = ifelse(coattendande == ">50%", sqrt(perc), 0),
             mgrRel = case_when(mgr1on1 == thres_low_chr & coattendande == "<50%" ~ "Under-coached",
                                mgr1on1 == thres_low_chr & coattendande == ">50%" ~ "Co-attending",
                                mgr1on1 == thres_top_chr & coattendande == ">50%" ~ "Highly managed",
                                TRUE ~ "Coaching")) %>%
      mutate_at("mgrRel", ~as.factor(.)) -> chart

    chart %>%
      select(mgrRel, n, perc) %>%
      group_by(mgrRel) %>%
      summarise_all(~sum(., na.rm = TRUE)) -> clean_tb

  } else if(hrvar != "Total"){

    data2 %>%
      count(!!sym(hrvar), mgr1on1, coattendande) %>%
      group_by(!!sym(hrvar)) %>%
      mutate(perc = n / sum(n)) %>% # Calculate percentages
      mutate(xmin = ifelse(mgr1on1 == thres_low_chr, -sqrt(perc), 0),
             xmax = ifelse(mgr1on1 == thres_top_chr, sqrt(perc), 0),
             ymin = ifelse(coattendande == "<50%", -sqrt(perc), 0),
             ymax = ifelse(coattendande == ">50%", sqrt(perc), 0),
             mgrRel = case_when(mgr1on1 == thres_low_chr & coattendande == "<50%" ~ "Under-coached",
                                mgr1on1 == thres_low_chr & coattendande == ">50%" ~ "Co-attending",
                                mgr1on1 == thres_top_chr & coattendande == ">50%" ~ "Highly managed",
                                TRUE ~ "Coaching")) %>%
      ungroup() %>%
      mutate_at("mgrRel", ~as.factor(.)) -> chart

    chart %>%
      select(mgrRel, !!sym(hrvar), n, perc) %>%
      group_by(mgrRel, !!sym(hrvar)) %>%
      summarise_all(~sum(., na.rm = TRUE)) -> clean_tb

  }

  ## Sort colours out
  # Legacy variable names
  myColors <- plot_colors
  names(myColors) <- levels(chart$mgrRel)

  ## Show stacked bar chart if multiple groups

  if(hrvar == "Total"){

    plot <-
      chart %>%
      ggplot() +
      geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill = mgrRel), color = "white") +
      scale_fill_manual(name = "mgrRel", values = myColors) +
      geom_text(aes(x = xmin + 0.5*sqrt(perc),
                    y = ymin + 0.5*sqrt(perc),
                    label = scales::percent(perc, accuracy = 1))) +
      coord_equal() +
      xlab("Weekly 1:1 time with manager") +
      scale_x_continuous(breaks = c(-max(abs(chart$xmin),abs(chart$xmax))/2,max(abs(chart$xmin),abs(chart$xmax))/2),
                         labels = c(thres_low_chr, thres_top_chr),
                         limits = c(-max(abs(chart$xmin), abs(chart$xmax)), max(abs(chart$xmin), abs(chart$xmax)))) +
      ylab("Employee and manager coattend") +
      scale_y_continuous(breaks = c(-max(abs(chart$ymin), abs(chart$ymax))/2, max(abs(chart$ymin), abs(chart$ymax))/2),
                         labels = c("<50%", ">50%"),
                         limits = c(-max(abs(chart$ymin), abs(chart$ymax)), max(abs(chart$ymin), abs(chart$ymax)))) +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            plot.title = element_text(color="grey40", face="bold", size=20),
            axis.line = element_line(),
            legend.position = "bottom",
            legend.title = element_blank(),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 12),
            legend.text = element_text(size = 12),
            strip.text.x = element_text(color = "grey40", face = "bold", size = 14)) +
      labs(caption = extract_date_range(data, return = "text"))

  } else if(hrvar != "Total"){
    plot <-
      chart %>%
      mutate(Fill = case_when(mgrRel == "Co-attending" ~ rgb2hex(68,151,169),
                              mgrRel == "Coaching" ~ rgb2hex(95,190,212),
                              mgrRel == "Highly managed" ~ rgb2hex(49,97,124),
                              mgrRel == "Under-coached" ~ rgb2hex(89,89,89))) %>%
      ggplot(aes(x = !!sym(hrvar), y = perc, group = mgrRel, fill = Fill)) +
      geom_bar(position = "stack", stat = "identity") +
      geom_text(aes(label = paste(round(perc * 100), "%")),
                position = position_stack(vjust = 0.5),
                color = "#FFFFFF",
                fontface = "bold") +
      scale_fill_identity(name = "Coaching styles",
                          breaks = c(rgb2hex(68,151,169),
                                     rgb2hex(95,190,212),
                                     rgb2hex(49,97,124),
                                     rgb2hex(89,89,89)),
                          labels = c("Co-attending",
                                     "Coaching",
                                     "Highly managed",
                                     "Under-coached"),
                          guide = "legend") +
      coord_flip() +
      theme_wpa_basic() +
      labs(title = "Distribution of types of \nmanager-direct relationship across organizations",
           subtitle = "Based on manager 1:1 time and percentage of overall time spent with managers")
  }


  if(return == "plot"){

    plot +
      labs(title = "Distribution of types of \nmanager-direct relationship",
           subtitle = "Based on manager 1:1 time and percentage of\noverall time spent with managers")

  } else if(return == "table"){

    clean_tb %>%
      as_tibble() %>%
      return()

  } else if(return == "chartdata"){

    chart %>%
      as_tibble() %>%
      return()

  } else {

    stop("Please enter a valid input for `return`.")

  }
}


