# wpa R Playbook

This page offers a **playbook for the Workplace Analytics analyst**: a guide to what analyses or functions could be useful when exploring a particular area of Workplace Analytics. This map is divided into the following sections.

- [Ways of Working](#ways-of-working)
- [Employee Wellbeing](#employee-wellbeing)
- [Managerial Excellence](#managerial-excellence)
- [Teaming and Networks](#teaming-and-networks)

For each individual function, you can click into the linked documentation for more information on how to run them and what outputs are returned. 

## Ways of Working

### Essentials

It is recommended that you start with running `keymetrics_scan()` on a Standard Person Query, which returns a heat map table with an overview with all the key metrics in Workplace Analytics. For more detailed breakdowns, you may also run 
`collaboration_report()` to establish the baseline of collaboration. 

Here are a list of other individual plots that you can run:
- Summary of metrics: `collab_sum()`, `workloads_sum()`, `email_sum()`, `meeting_sum()`.
- Time dimension of metrics: `collab_area()`, or any of the `*_line()` or `*_trend()` functions.

### Advanced

#### Meeting efficiency

One option to advance the analysis is to take a deep dive into meetings.

- `meeting_skim()` can be used to understand the overall % of meetings which are low quality. Hours can be expressed in terms of number of FTE-weeks (or months), or even dollar values for greater impact.
- `meetingtype_dist()` can be used to understand the distribution of long or large meetings.
- **Meeting subject line text mining**: `meeting_tm_report()` can reveal patterns underlying meeting subject lines. The report is made of individual visualization functions, i.e.:
	- `tm_cooc()`
	- `tm_freq()`
	- `tm_wordcloud()

#### Collaboration across time

Another option is to analyse collaboration metrics across a time dimension.
- To understand detailed changes in collaboration metrics over time, you can use `period_change()` to visualize the distribution across the population in a _before vs after_ comparison.
- To understand what metrics have changed the most with respect to time, you can use `IV_by_period()`, which uses the _before vs after_ as an outcome variable to calculate which Workplace Analytics metrics have had the greatest impact according to Information Value. 

## Employee Wellbeing

### Essentials
The key metrics in relation to **Employee Wellbeing** are _Work Week Span_, _Collaboration Hours_, and _After-hours collaboration hours_, which can be visualized in a number of ways. You can start with the `capacity_report()` which provides a baseline of this view.

- Rank all groups by their group average: the functions to use are `workloads_rank()`, `collab_rank()`, and `afterhours_rank()`.
- Distribution of capacity metrics: `*_fizz()` functions offer a way of quickly visualizing the distribution of the capacity metrics. For the traditionalist, you can also use `create_boxplot()` for the given metric to create boxplot views.
- Visualize hourly collaboration with `workpatterns_area()`
- Any of the `workloads_*`, `collab_*` and `afterhours_*` functions.

### Advanced

#### Working patterns archetypes
You can classify person-weeks or persons into individual archetypes with `workpatterns_classify()`, which reveals _break_ patterns in employees' weeks. If you are unsure with where to start, you can also run `workpatterns_report()` which combines a number of plots based on the classified archetypes. You will need an Hourly Collaboration query.

#### Flexibility Index
`flex_index()` computes a **Flexibility Index** which is a measure of how flexible teams are working, based on three components, i.e.:
  - (i) whether they take breaks during the week, 
  - (ii) whether they start their date at different times compared to the official time, and 
  - (iii) whether they keep their total working hours under control. 

#### Analysing costly business processes
By filtering on email and meeting subject lines in a Person Query, it is possible to compute the employee-hours that have been invested into a particular business process. To identify the right keywords to use in the filter, you can run `meeting_tm_report()` or one of its component functions (`tm_cooc()`, `tm_freq()`, `tm_wordcloud()`) to examine what keywords are related to the business process you have in mind.

## Managerial Excellence

### Essentials

You can start with the `coaching_report()` which provides a baseline of this view.

- `mgrrel_matrix()`: a two-by-two matrix describing manager relations styles experienced by the analysis group.
- Any of the functions in the `one2one_*()` family, i.e.:
  - `one2one_dist()`
  - `one2one_fizz()`
  - `one2one_line()`
  - `one2one_rank()`

### Advanced

Good manager coaching is a key factor for employee engagement and retention. 

**Employee sentiment analysis**
- To compute feature importance, you can leverage `create_IV()` and `IV_report()` for calculating Information Value, using  Workplace Analytics metrics (including manager metrics) as predictor variables and employee sentiment scores as an outcome variable.
- If employee churn and sentiment data is available, you can also use Workplace Analytics to build a model for understanding the most important reasons for churn. There currently isn't a function for modelling in **wpa**, but you can make use of capabilities in the wider R universe, such as **randomForest** and **tidymodels**. 
- For more capabilities in calculating variable importance, you may reference the **rwa** and the **vip** packages.

## Teaming and Networks

### Essentials

You can start with the `connectivity_report()` which provides a baseline of this view.

- Visualize external and internal networks with `external_network_plot()` and `internal_network_plot()`
- Visualize correlation between network metrics (e.g. network size) and organizational attributes, such as employee tenure, organization, and level. You can use `create_bar()` , `create_boxplot()`, or `create_fizz()`.
- To visualize correlation between network metrics and another Workplace Analytics metric, you can use `create_scatter()` or `create_bubble()`.

### Advanced

#### Network visualization

Collaboration can be visualized as a network in several ways.

- Group-to-group network with `network_g2g()`. There are `return` options for a simple network visualization or an interaction table.
- Use `create_sankey()` to create a Group to Group collaboration visual
- Person-to-person network with `network_p2p()`.

#### Community Detection
- Community detection can be performed with `network_p2p()`. See function documentation for more information on methodology and usage.
