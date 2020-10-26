# #' @title Create an exploratory overview of Standard Query dataset
# #'
# #' @description
# #' Input a Standard Query dataset as a data frame in the argument, returning an overview of the key metrics.
# #'
# #' @param data A Standard Query dataset in the form of a data frame.
# #' @param mingroup Numeric value setting the privacy threshold / minimum group size.
# #'
# #' @import dplyr
# #' @import ggplot2
# #' @import reshape2
# #'
# #' @return
# #' Returns a HTML report containing an overview of the key metrics in a Standard Person Query.
# #'
# #' @export
#
# keymetrics_report <- function(data, mingroup = 10){
#   ## Table by Person (Period Averages)
#   data %>%
#     group_by(PersonId) %>%
#     summarise(periods = n(),
#               Organization = first(Organization),
#               LevelDesignation = first(LevelDesignation)) -> mydata_person
#
#   ## Plot available data by Organization
#   mydata_person %>% count(Organization, name = "count") -> mydata_OrgLevel
#
#   mydata_OrgLevel %>%
#     ggplot(aes(x = Organization, y = 1, label=count)) +
#     geom_point(aes(size = count), colour = "steelblue", alpha = 0.5) +
#     scale_size_continuous(range = c(20, 50)) +
#     geom_label() +
#     ggtitle("Measured Individuals (by Organization)") +
#     ylab("") +
#     xlab("Organization") +
#     theme(legend.position="none",
#           axis.text.x = element_text(angle = 90, hjust = 1),
#           axis.title.y = element_blank(),
#           axis.text.y = element_blank(),
#           axis.ticks.y = element_blank()) -> plot_OrgLevel
#
#   ## Plot available data by LevelDesignation
#   mydata_person %>% count(LevelDesignation, name = "count") -> mydata_LevelDesignation
#
#   mydata_LevelDesignation %>%
#     ggplot(aes(x=LevelDesignation, y=1, label=count)) +
#     geom_point(aes(size = count), colour = "steelblue", alpha = 0.5) +
#     scale_size_continuous(range = c(20, 50)) +
#     geom_label() +
#     ggtitle("Measured Individuals (by Level Designation)") +
#     ylab("") + xlab("Level Designation") +
#     theme(legend.position="none",
#           axis.text.x = element_text(angle = 90, hjust = 1),
#           axis.title.y = element_blank(),
#           axis.text.y = element_blank(),
#           axis.ticks.y = element_blank(),
#           plot.title = element_text(color="grey40", face="bold", size=20)) -> plot_LevelDesignation
#
#   ## Plot available data by Organization and Level
#   mydata_person %>% count(Organization, LevelDesignation, name = "count") -> mydata_OrgLevelDesignation
#
#   mydata_OrgLevelDesignation %>%
#     ggplot(aes(x = Organization, y = LevelDesignation)) +
#     geom_tile(aes(fill = count), colour = "grey50") +
#     geom_text(aes(label = round(count, 1))) +
#     scale_fill_gradient(low = "lightblue", high = "blue",
#                         limits=c(mingroup,NA)) +
#     labs(fill = "Count") +
#     ggtitle("Group Sizes (by Organization and Level)") +
#     ylab("Level Designation") +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1),
#           plot.title = element_text(color="grey40", face="bold", size=20)) -> plot_OrgLevelDesignation
#
#   ## Pipe in Manager Relationship
#   data %>% mgrrel_matrix(return = "plot") -> plot_ManagerRelationship
#
#   ## Pipe in WITL
#   data %>% keymetrics_scan(hrvar = "Organization", return = "plot") -> plot_WITL
#
#   ## Pipe in Meeting Habits
#   data %>% meeting_summary(hrvar = "Organization", return = "plot") -> plot_MeetingHabits
#
#   ## Generate kables
#
#   mydata_OrgLevel %>%
#     knitr::kable(caption = "Data by Organization") %>%
#     kableExtra::kable_styling(bootstrap_options = "striped", full_width = TRUE) -> tb_OrgLevel
#
#   mydata_LevelDesignation %>%
#     knitr::kable(caption = "Data by Level Designation") %>%
#     kableExtra::kable_styling(bootstrap_options = "striped", full_width = TRUE) -> tb_LevelDesignation
#
#   mydata_OrgLevelDesignation %>%
#     spread(LevelDesignation, count) %>%
#     knitr::kable(caption = "Data by Level Designation") %>%
#     kableExtra::kable_styling(bootstrap_options = "striped", full_width = TRUE) -> tb_OrgLevelDesignation
#
#   ## Pipe in Manager Relationship
#   data %>%
#     mgrrel_matrix(return = "table") %>%
#     knitr::kable(caption = "Distribution of Manager-Direct Relationship") %>%
#     kableExtra::kable_styling(bootstrap_options = "striped", full_width = TRUE) -> tb_ManagerRelationship
#
#   ## Pipe in WITL
#   data %>%
#     keymetrics_scan(hrvar = "Organization", return = "table") %>%
#     mutate_at(vars(-variable), ~round(., 1)) %>%
#     knitr::kable(caption = "Week in the Life - by Organization") %>%
#     kableExtra::kable_styling(bootstrap_options = "striped", full_width = TRUE)  -> tb_WITL
#
#   ## Pipe in Meeting Habits
#   data %>%
#     meeting_summary(hrvar = "Organization", return = "table") %>%
#     mutate_at(vars(-variable), ~round(., 1)) %>%
#     knitr::kable(caption = "Meeting Habits - by Organization") %>%
#     kableExtra::kable_styling(bootstrap_options = "striped", full_width = TRUE)  -> tb_MeetingHabits
#
#   markobj <- c('---',
#                'title: "WPA Standard Query Overview"',
#                'output: ',
#                '  html_document:',
#                '    theme: united',
#                '    toc: true',
#                '    toc_float:',
#                '      collapsed: false',
#                '      smooth_scroll: true',
#
#                '---',
#                '',
#                '',
#
#                '### Week in the Life',
#                '```{r, echo=FALSE}',
#                'plot_WITL',
#                '```',
#                '```{r, echo=FALSE}',
#                'tb_WITL',
#                '```',
#
#                '### Organization Level',
#
#                '```{r, echo=FALSE}',
#                'tb_OrgLevel',
#                '```',
#
#                '### Level Designation',
#                '```{r, echo=FALSE}',
#                'plot_LevelDesignation',
#                '```',
#                '```{r, echo=FALSE}',
#                'tb_LevelDesignation',
#                '```',
#
#                '### Organization and Level Designation',
#                '```{r, echo=FALSE}',
#                'plot_OrgLevelDesignation',
#                '```',
#                '```{r, echo=FALSE}',
#                'tb_OrgLevelDesignation',
#                '```',
#
#                '### Meeting Habits',
#                '```{r, echo=FALSE}',
#                'plot_MeetingHabits',
#                '```',
#                '```{r, echo=FALSE}',
#                'tb_MeetingHabits',
#                '```',
#
#                '### Manager-direct relationship Distribution',
#                '```{r, echo=FALSE}',
#                'plot_ManagerRelationship',
#                '```',
#                '```{r, echo=FALSE}',
#                'tb_ManagerRelationship')
#
#   writeLines(markobj, "SQ-overview.Rmd")
#   rmarkdown::render("SQ-overview.Rmd")
#   utils::browseURL("SQ-overview.html")
#   unlink(c("SQ-overview.Rmd",
#            "SQ-overview.md"))
# }
