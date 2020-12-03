# Distribution Functions

**Distribution** functions allow you to go beyond aggregated averages, and diagnose whether metrics are skewed, evenly distributed, or contain outliers. 

## Grouping individuals

The `collaboration_dist()` function allows you to produce a 100% stacked bar chart an Standard Person Query, with employees distributed in groups according to their amount of collaboration hours. 

```R
sq_data %>% collaboration_dist() 
```

You can specify which HR attribute/variable to use as a grouping variable with the `hrvar` argument, and what output to obtain (either "plot" or "table") with the `return` argument.

```R
sq_data %>% collaboration_dist(return="table") 
```
The package includes a wide range of equivalent functions, that create 100% stacked bar plots for specific metrics. These include:

- `email_dist()`
- `meeting_dist()`
- `one2one_dist()`
- `afterhours_dist()`
- `workloads_dist()`

## Fizzy Drink Plots 
You can also explore distributions by using jittered ('fizzy drink') scatter plot. The `collaboration_fizz()` function illustrate how different individuals are distributed in a common scale:

```R
sq_data %>% email_fizz() # Fizzy drink plot
```

These functions are also flexible, allowing you to specify an HR attribute grouping variable and the `return` argument.


```R
sq_data %>% collaboration_fizz(hrvar = "LevelDesignation", return = "table")
```

Other examples of distribution fizzy drink plots include:

- `email_fizz()`
- `meeting_fizz()`
- `one2one_fizz()`
- `afterhours_fizz()`
- `workloads_fizz()`

## Custom bar charts and tables

For other metrics, you can obtain the equivalent graphs using the `create_dist()`,  `create_fizz()` and `create_boxplot()`. These functions requires you to include a character string containing the name of the metric you want to analyze, e.g. "Generated_workload_email_hours":


```
sq_data %>% create_dist(metric = "Generated_workload_email_hours")

sq_data %>% create_fizz(metric = "Generated_workload_email_hours")

sq_data %>% create_boxplot(metric = "Generated_workload_email_hours")

```


##  Exploring changes over time

In our next section, we will cover [**Trend Functions**](analyst_guide_trend.html) that are useful to explore time dyanmics across a wide range of metrics.

## Gallery

<html>
<head>
<style>
div.gallery {
  margin: 5px;
  border: 1px solid #ccc;
  float: left;
  width: 180px;
}

div.gallery:hover {
  border: 1px solid #777;
}

div.gallery img {
  width: 100%;
  height: auto;
}

div.desc {
  padding: 15px;
  text-align: center;
}
</style>
</head>
<body>

<div class="gallery">
  <a target="_blank" href="https://raw.githubusercontent.com/microsoft/wpa/main/.github/gallery/email_dist.png">
    <img src="https://raw.githubusercontent.com/microsoft/wpa/main/.github/gallery/email_dist.png" alt="Email Distribution Function" width="600" height="400">
  </a>
  <div class="desc">email_dist()</div>
</div>

</body>
</html>




