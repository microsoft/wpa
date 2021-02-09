# Analysis Map

This page offers a 'map' for the Workplace Analytics analyst - a guide to what analyses or functions could be useful when exploring a particular area of Workplace Analytics. This map is divided into the following sections.

- Collaboration
- Capacity
- Coaching
- Connectivity

## Collaboration

### Essentials

It is recommended that you start with running `keymetrics_scan()` on a Standard Person Query, which returns a heat map table with an overview with all the key metrics in Workplace Analytics. For more detailed breakdowns, you may also run 
`collaboration_report()` to establish the baseline of collaboration. 

Here are a list of other individual plots that you can run:
- Summary of metrics: `collab_sum()`, `workloads_sum()`, `email_sum()`, `meeting_sum()`.
- Time dimension of metrics: `collab_area()`, or any of the `*_dist()` or `*_trend()` functions.

### Advanced

If the topic of interest is meetings:
- `meeting_skim()` to understand the overall % of meetings which are low quality.
- `meetingtype_dist()` to understand the distribution of long or large meetings.
- **Meeting subject line text mining**: `meeting_tm_report()` can reveal patterns underlying meeting subject lines. 

## Capacity

### Essentials
The key metrics in relation to **capacity** are _Work Week Span_, _Collaboration Hours_, and _After-hours collaboration hours_, which can be visualized in a number of ways. You can start with the `capacity_report()` which provides a baseline of this view.

- Rank all groups by their group average: the functions to use are `workloads_rank()`, `collab_rank()`, and `afterhours_rank()`.
- Distribution of capacity metrics: `*_fizz()` functions offer a way of quickly visualizing the distribution of the capacity metrics. For the traditionalist, you can also use `create_boxplot()` for the given metric to create boxplot views.
- Visualize hourly collaboration with `workpatterns_area()`
- Any of the `workloads_*`, `collab_*` and `afterhours_*` functions.

### Advanced

- Working patterns archetypes: you can classify person-weeks or persons into individual archetypes with `workpatterns_classify()`, which reveals _break_ patterns in employees' weeks. If you are unsure with where to start, you can also run `workpatterns_report()` which combines a number of plots based on the classified archetypes. You will need an Hourly Collaboration query.
- Flexibility Index: `flex_index()` computes a **Flexibility Index** which is a measure of how flexible teams are working, based on three components, i.e. (i) whether they take breaks during the week, (ii) whether they start their date at different times compared to the official time, and (iii) whether they keep their total working hours under control. 

## Coaching

### Essentials

You can start with the `coaching_report()` which provides a baseline of this view.

- `mgrrel_matrix()`: a two-by-two matrix describing manager relations styles experienced by the analysis group.
- Any of the functions in the `one2one_*()` family, i.e.:
  - `one2one_dist()`
  - `one2one_fizz()`
  - `one2one_line()`
  - `one2one_rank()`

### Advanced

## Connectivity

### Essentials

You can start with the `connectivity_report()` which provides a baseline of this view.

- Visualize external and internal networks with `external_network_plot()` and `internal_network_plot()`

### Advanced

- Group-to-group network with `network_g2g()`
- Person-to-person network with `network_p2p()`
- Community detection with `network_p2p()`

## Special topics

- Feature importance / Information Value
- Time series analysis
- Identifying related keywords in subject lines
- Churn / org movement analysis

_Coming soon..._