# Distribution

**Distribution** functions allow you to go beyond aggregated averages, and diagnose whether metrics are skewed, evenly distributed, or contain outliers. 

You can use the `fizz` and `dist` family of functions for this. For email hours, you can run the following:

```R
sq_data %>% email_dist() # 100% horizontal stacked bar
```

<img src="https://raw.githubusercontent.com/microsoft/wpa/main/.github/gallery/email_dist.png" align ="center" width=80%>

You can also return a jittered scatter plot ('fizzy drink plot'). 

```R
sq_data %>% email_fizz() # Fizzy drink plot
```

## Functions

Other examples of distribution functions include:

### Dist

- `collaboration_dist()`
- `meeting_dist()`
- `one2one_dist()`
- `afterhours_dist()`
- `workloads_dist()`

### Fizz 

- `collaboration_fizz()`
- `meeting_fizz()`
- `one2one_fizz()`
- `afterhours_fizz()`
- `workloads_fizz()`

### Flexible functions
- `create_dist()`
- `create_fizz()`
- `create_boxplot()`