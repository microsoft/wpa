# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Plot the distribution of percentage change between periods
#' of a WpA metric by the number of employees.
#'
#' @description
#' This function also presents the p-value for the null hypothesis
#'  that the variable has not changed, using a Wilcox signed-rank test.
#'
#' @author Mark Powers <mark.powers@@microsoft.com>
#'
#' @param data Person Query as a dataframe including date column named `"Date"`
#'   This function assumes the data format is `MM/DD/YYYY` as is standard in a
#'   Workplace Analytics query output.
#' @param compvar WpA comparison variable to compare person change before and
#'   after For example, `"Collaboration_hours"`
#' @param before_start Start date of "before" time period in `YYYY-MM-DD`
#' @param before_end End date of "before" time period in `YYYY-MM-DD`
#' @param after_start Start date of "after" time period in `YYYY-MM-DD`
#' @param after_end End date of "after" time period in `YYYY-MM-DD`
#' @param return Character vector specifying whether to return plot as Count or
#'   Percentage of Employees. Valid inputs include:
#'    - "count" (default)
#'    - "percentage"
#'    - "table"
#'
#' @return
#' ggplot object showing a bar plot (histogram) of change for two time
#' intervals.
#'
#' @import dplyr
#' @import reshape2
#' @import ggplot2
#' @import scales
#' @importFrom stats wilcox.test
#'
#' @family Visualization
#' @family Time-series
#' @family Flexible
#'
#' @examples
#' # Run plot
#' period_change(sq_data, compvar = "Workweek_span", before_end = "2019-11-16")
#'
#' \donttest{
#' # Run plot with more specific arguments
#' period_change(sq_data,
#'               compvar = "Workweek_span",
#'               before_start = "2019-11-03",
#'               before_end = "2019-11-16",
#'               after_start = "2019-12-03",
#'               after_end = "2019-12-16",
#'               return = "percentage")
#' }
#' @family Flexible Input
#' @export

period_change <-
  function(data,
           compvar,
           before_start = min(as.Date(data$Date, "%m/%d/%Y")),
           before_end,
           after_start = as.Date(before_end) + 1,
           after_end = max(as.Date(data$Date, "%m/%d/%Y")),
           return = "count") {

    ## Check inputs
    ## Update these column names as per appropriate
    required_variables <- c("Date",
                            compvar,
                            "PersonId")
    ## Error message if variables are not present
    ## Nothing happens if all present
    data %>%
      check_inputs(requirements = required_variables)

    daterange_1_start <- as.Date(before_start)
    daterange_1_end <- as.Date(before_end)
    daterange_2_start <- as.Date(after_start)
    daterange_2_end <- as.Date(after_end)

    # Fix dates format for Workplace Analytics Queries
    WpA_dataset <- data %>% mutate(Date = as.Date(Date, "%m/%d/%Y"))



    # Check for dates in data file
    if (daterange_1_start < min(WpA_dataset$Date) |
        daterange_1_start > max(WpA_dataset$Date) |
        daterange_1_end < min(WpA_dataset$Date) |
        daterange_1_end > max(WpA_dataset$Date) |
        daterange_2_start < min(WpA_dataset$Date) |
        daterange_2_start > max(WpA_dataset$Date) |
        daterange_2_end < min(WpA_dataset$Date) |
        daterange_2_end > max(WpA_dataset$Date)) {
      stop('Dates not found in dataset')
      geterrmessage()
    }

    # Create variable => Period
    WpA_dataset_table <-
      WpA_dataset %>% mutate(
        Period = case_when(
          Date >= daterange_1_start &
            Date <= daterange_1_end ~ "Before",
          Date >= daterange_2_start &
            Date <= daterange_2_end ~ "After"
        )
      ) %>% filter(Period == "Before" | Period == "After")

    # Group data by
    mydata_table <-
      WpA_dataset_table %>%
      group_by(Period, PersonId) %>%
      summarise_if(is.numeric, mean, na.rm = TRUE)

    # Select comparison variable
    mydata_table <-
      mydata_table %>% select(PersonId, Period, all_of(compvar))

    # Turn to Long
    data_wide <- tidyr::gather(mydata_table, KPI, value,-Period,-PersonId)

    # Turn to Wide
    data_final <- tidyr::spread(data_wide, Period, value)

    # Drop nas
    data_final <- data_final %>% tidyr::drop_na()

    # run Wilcox Signed-Rank test
    test_set <- data_final
    res <- wilcox.test(test_set$Before, test_set$After, paired = TRUE)
    p_val <- signif(res$p.value, digits = 3)

    # Calculate change between periods
    data_final <- data_final %>% mutate(delta = After - Before)

    # Calculate percent change between periods
    data_final <-
      data_final %>% mutate(perc_diff = (After - Before) / Before)

    # Drop NA and Errors
    data_final <- data_final %>% tidyr::drop_na(delta)
    data_final <-
      data_final %>% filter(Before > 0) #filters out people who joined the organization after the 'Before' period

    # Group 100%+ together so you can see how many are over 100% in a plot data set
    data_plot <- data_final
    data_plot$perc_diff[which(data_plot$perc_diff > 1)] <- 1.01

    # Categorize perc_diff by percentages
    data_plot <-
      data_plot %>% mutate(Mybins = cut_width(
        perc_diff,
        width = 0.1,
        boundary = 0,
        labels = F
      ))

    # create x-axis labels and replace Mybins with label
    labels = c(paste(seq(-100, 100, 10), "%", sep = ""), "100%+")
    x_axis <- vector("character", 21)
    for (i in 1:20) {
      x_axis[i] <- paste(labels[i], "-", labels[i + 1])
    }
    x_axis[21] <- labels[21]
    data_plot <- data_plot %>% mutate(Mybins = x_axis[Mybins])
    data_plot <-
      data_plot %>%
      mutate(Mybins = factor(Mybins, levels = x_axis))

    #calculate percentage of employees in each bin
    data_legend <-
      data_plot %>%
      group_by(Mybins) %>%
      summarize(Employee_Count = n_distinct(PersonId))

    data_legend <- data.frame(data_legend)

    data_legend <-
      data_legend %>%
      mutate(Employee_Counts = paste("n=", Employee_Count))

    data_legend <-
      data_legend %>%
      mutate(Employee_perc = Employee_Count / sum(data_legend$Employee_Count))

    # create summary table for table output
    summary_table <-
      data_legend %>%
      select(Mybins, Employee_Count, Employee_perc)

    if (return == "count") {
      # Plot barplot of Count of Employees with % change
      data_plot %>%
        ggplot(aes(x = Mybins)) +
        geom_bar(fill = "#203864") +
        scale_x_discrete(name = "Percent change") +
        scale_y_continuous(name = "Employee count") +
        theme_wpa_basic() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(
          title = paste("Number of employees with change in", compvar),
          subtitle = paste(
            "Period 1:",
            daterange_1_start,
            "to",
            daterange_1_end,
            "and Period 2:",
            daterange_2_start,
            "to",
            daterange_2_end
          )
        ) +
        labs(
          caption = paste(
            "Total employees =",
            sum(data_legend$Employee_Count),
            "| Data from",
            daterange_1_start,
            "to",
            daterange_1_end,
            "and",
            daterange_2_start,
            "to",
            daterange_2_end,
            "| p =",
            p_val
          )
        )

    } else if (return == "percentage") {
      # Create histogram by % of employees changing
      data_legend %>%
        ggplot(aes(x = Mybins, y = Employee_perc)) +
        geom_col( fill = "#203864") +
        scale_x_discrete(name = "Percent change") +
        scale_y_continuous(name = "Percentage of measured employees",
                           labels = scales::percent_format(accuracy = 1)) +
        annotate(
          "text",
          x = data_legend$Mybins,
          y = -0.005,
          label = data_legend$Employee_Counts,
          size = 3
        ) +
        theme_wpa_basic() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(
          title = paste("Percentage of employees with change in", compvar),
          subtitle = paste(
            "Period 1:",
            daterange_1_start,
            "to",
            daterange_1_end,
            "and Period 2:",
            daterange_2_start,
            "to",
            daterange_2_end
          )
        ) +
        labs(
          caption = paste(
            "Total employees =",
            sum(data_legend$Employee_Count),
            "| Data from",
            daterange_1_start,
            "to",
            daterange_1_end,
            "and",
            daterange_2_start,
            "to",
            daterange_2_end,
            "| p =",
            p_val
          )
        )
    } else if(return == "table"){
      summary_table %>%
        mutate_at("Employee_perc", ~round(. * 100, 1)) %>%
        as_tibble() %>%
        return()

    } else {

      stop("Please enter a valid input for `return`, either count, percentage, or table.")

    }
  }
