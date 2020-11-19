# Summary

**Summary functions** allow you to compare averages across organizational attributes.

An instance of a summary function in action would be:

```R
sq_data %>% collaboration_summary()
```

You can use return a summary table rather than a plot:

```R
sq_data %>% collaboration_summary(return = "table")
```

There is also an option to change the threshold for excluding group size:

```
sq_data %>%
  collaboration_sum(hrvar = "LevelDesignation",
  					mingroup = 10,
  					return = "table")
```



Other similar functions include:

- `email_summary()`
- `meeting_summary()`
- `one2one_summary()`
- `workloads_summary()`
- `afterhours_summary()`
