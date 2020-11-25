# Summary

**Summary functions** allow you to compare averages across organizational attributes.

An instance of a summary function in action would be:

```R
sq_data %>% collaboration_summary()
```

<img src="https://raw.githubusercontent.com/microsoft/wpa/main/.github/gallery/collab_sum.png" align ="center" width=80%>

This returns a plot that you can either as a SVG (vector) or PNG (scalar) image by passing the output to `export()`.  Since the plot output is a ggplot object, you can also make further customizations to it by appending ggplot layers, for instance:

```R
sq_data %>%
	collaboration_summary() +
	ggtitle("This is a custom title")
```

You can use return a summary table rather than a plot:

```R
sq_data %>% collaboration_summary(return = "table")
```

To export the results to a clipboard, you can pass the outputs to `export()` again:

```R
sq_data %>%
	collaboration_summary(return = "table") %>%
	export()
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

See `create_bar()` for creating summary functions with other Workplace Analytics metrics. 

