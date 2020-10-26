#' @title Manager meeting coattendance distribution
#'
#' @description
#' Analyze degree of attendance between employes and their managers.
#' Returns a stacked bar plot of different buckets of coattendance.
#' Additional options available to return a table with distribution elements.
#'
#' @param data A Standard Query dataset in the form of a data frame.
#' @param hrvar HR Variable by which to split metrics. Accepts a character vector, defaults to "Organization" but accepts any character vector, e.g. "LevelDesignation"
#' @param mingroup Numeric value setting the privacy threshold / minimum group size, defaults to 5.
#' @param return Character vector specifying what to return, defaults to "plot".
#' Valid inputs are "plot" and "table".
#'
#' @import dplyr
#' @import ggplot2
#' @import reshape2
#' @import scales
#' @importFrom stats median
#' @importFrom stats sd
#'
#' @family Managerial Relations
#' @family Meeting Culture
#'
#' @examples
#' workloads_dist(sq_data, hrvar = "Organization", return = "table")
#' @export

mgrcoatt_dist <- function(data, hrvar = "Organization", mingroup = 5, return = "plot") {

myPeriod <-
    data %>%
    mutate(Date=as.Date(Date, "%m/%d/%Y")) %>%
    arrange(Date) %>%
    mutate(Start=first(Date), End=last(Date)) %>%
    filter(row_number()==1) %>%
    select(Start, End)

  ## Basic Data for bar plot
  plot_data <-
    data %>%
    rename(group = !!sym(hrvar)) %>%
    group_by(PersonId) %>%
	filter(Meeting_hours>0) %>%
	mutate(coattendman_rate = Meeting_hours_with_manager / Meeting_hours) %>%
    summarise(periods = n(),
              group = first(group), coattendman_rate=mean(coattendman_rate)) %>%
    group_by(group) %>%
    mutate(Employee_Count = n_distinct(PersonId)) %>%
    filter(Employee_Count >= mingroup)

  ## Create buckets of coattendance time
  plot_data <-
    plot_data %>%
    mutate(bucket_coattendman_rate = case_when(coattendman_rate>=0 &  coattendman_rate<.20 ~ "0 - 20%",
                                                                        coattendman_rate>=.20 & coattendman_rate<.4 ~ "20 - 40%",
                                                                        coattendman_rate>=.40 & coattendman_rate<.6 ~ "40 - 60%",
                                                                        coattendman_rate>=.6 ~ "60% +"))


  ## Employee count / base size table
  plot_legend <-
    plot_data %>%
    group_by(group) %>%
    summarize(Employee_Count=first(Employee_Count)) %>%
    mutate(Employee_Count = paste("n=",Employee_Count))

  ## Data for bar plot
  plot_table <-
    plot_data %>%
    group_by(group, bucket_coattendman_rate) %>%
    summarize(Employees=n(),
              Employee_Count=first(Employee_Count), percent= Employees / Employee_Count) %>%
    arrange(group, bucket_coattendman_rate)

  ## Table for annotation
  annot_table <-
    plot_legend %>%
    dplyr::left_join(plot_table, by = "group")

  ## Bar plot
  plot_object <-
    plot_table %>%
    ggplot(aes(x = group, y=Employees, fill = bucket_coattendman_rate)) +
    geom_bar(stat = "identity", position = position_fill(reverse = TRUE)) +
	coord_flip() +
	scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
	annotate("text", x = plot_legend$group, y = -.05, label = plot_legend$Employee_Count ) +
	scale_fill_manual(name="", values = c("#bed6f2", "#e9f1fb","#ffdfd3","#FE7F4F")) +
	theme_classic() +
    theme(axis.text=element_text(size=12),
          plot.title = element_text(color="grey40", face="bold", size=18),
          plot.subtitle = element_text(size=14), legend.position = "top", legend.justification = "right",
          legend.title=element_text(size=14), legend.text=element_text(size=14)) +
	labs(title = "Time with Manager", subtitle = paste("Meeting Co-attendance Rate by", hrvar)) +
	xlab(hrvar) +
	ylab("Fraction of Employees") +
	labs(caption = paste("Data from week of", myPeriod$Start, "to week of", myPeriod$End))

  ## Table to return
  return_table <-
    plot_table %>%
    select(group, bucket_coattendman_rate,  percent) %>%
    spread(bucket_coattendman_rate,  percent)

  if(return == "table"){

    return_table %>%
      as_tibble() %>%
      return()

  } else if(return == "plot"){

    return(plot_object)

  } else {

    stop("Please enter a valid input for `return`.")

  }
}
