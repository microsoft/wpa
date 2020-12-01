# Summary functions

This section describes the use of  summary functions, that allow you to compare averages across organizational attributes.


## Summary of Key Metrics

The `keymetrics_scan()` function allows you to produce a summary table with a wide range of metrics from the Standard Person Query data. Similar to most of the functions in this package, you can specify what output to obtain (either "plot" or "table") with the `return` argument . In addition, you have to specify which HR attribute/variable to use as a grouping variable with the `hrvar` argument.


```R
sq_data %>% keymetrics_scan(hrvar = "Organization", return = "plot")
sq_data %>% keymetrics_scan(hrvar = "Organization", return = "table")
```

## Collaboration Summary

The `collaboration_summary()` function allows you to generate a stacked bar plot summarising the email and meeting hours by an HR attribute you specify (if none is specified, organization will be used by default)

```{r}
sq_data %>% collaboration_summary()
```

<img src="https://raw.githubusercontent.com/microsoft/wpa/main/.github/gallery/collab_sum.png" align ="center" width=80%>


By changing the `hrvar()` argument, you can change the data being shown easily:

```{r}
sq_data %>% collaboration_summary(hrvar = "LevelDesignation")
```

By default, all summary functions exclude groups with less that five individuals. This is also something that can be adjusted, using the `mingroup()` argument:

```
sq_data %>%
  collaboration_sum(hrvar = "LevelDesignation",
  					mingroup = 10)
```

Finally, you can also use "table" in the `return` argument, to obtain summary table instead of a plot. The `export()` function will copy all contents to the clipboard. 

```{r}
sq_data %>% collaboration_summary(hrvar = "LevelDesignation", return = "table")
```


## Other summary functions

Other similar functions include:

- `email_summary()`
- `meeting_summary()`
- `one2one_summary()`
- `workloads_summary()`
- `afterhours_summary()`

All of these functions use Standard Person Query data, and accept `hrvar`, `return` and `mingroup` arguments.

## Customizing plots

All plots in *wpa* are [ggplot objects](https://rafalab.github.io/dsbook/ggplot2.html). This means you can also make further customizations to it by appending ggplot parameters and layers. For instance, you can customize the title of a `collaboration_summary()` plot:

```R
sq_data %>%
	collaboration_summary() +
	ggtitle("This is a custom title")
```

## Going Beyond Averages?

In the next section, we will explore how we can analyse distributions from different Workplace Analytics Metrics in a similar way. Let's continue to [**Distribution Functions**](analyst_guide_distribution.html).


