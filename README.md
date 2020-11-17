# wpa <img src="man/figures/logo2.png" align="right" width=15% />

  [![R build status](https://github.com/microsoft/wpa/workflows/R-CMD-check/badge.svg)](https://github.com/microsoft/wpa/actions) [![CodeFactor](https://www.codefactor.io/repository/github/microsoft/wpa/badge)](https://www.codefactor.io/repository/github/microsoft/wpa) [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT) 

## Analyze and Visualize Workplace Analytics data

This is an R package for analyzing and visualizing data from [Microsoft Workplace Analytics](https://www.microsoft.com/microsoft-365/partners/workplaceanalytics).

## With the **wpa** package, you can...

1. **Run prebuilt analysis and visualizations** off Workplace Analytics data with settings for HR variables, privacy threshold, etc.
2. **Generate prebuilt interactive HTML reports**, which cover specific areas e.g. collaboration, connectivity 
3. Leverage **advanced analytics functions**, such as text mining and hierarchical clustering, which are built for Workplace Analytics metrics
4. Integrate analysis of Workplace Analytics data with your R workflow seamlessly

Here are just some examples of the plots that you can create with **wpa**:

<img src="man/figures/plot-demo.gif" align="center" width=80% />

## Design Principles

- **Simple**: the functions ought to be simple and intuitive to maximise adoption.
- **Practical**: the functions should prioritise delivering against the most frequently used outputs and analyses.
- **Consistency**: functions should share a broadly consistent set of input arguments and naming conventions. This will help minimise unexpected results and errors when using the package. 
- **Parsimony**: in creating the package, as much of the existing code should be re-used if possible to minimise duplication of work and to make analysis reproducible.
- **Tidy**: the functions from the package are designed to be consistent with tidy principles, and work well with a **dplyr** pipe (`%>%`) workflow.

<img src="man/figures/api-demo.png" align="center" width=80% />

---

## :rocket: Quick start guide - For users

### Installing the package from GitHub

You can install **wpa** from GitHub with the following: 
```R
# Check if devtools is installed, if not then install it
if(!"devtools" %in% installed.packages()){
  install.packages("devtools")
}
devtools::install_git(url = "https://github.com/microsoft/wpa.git")
```
### Examples

The package comes shipped with a sample Standard Query dataset (`sq_data`), so you can start exploring functions without having to read in any data. Most functions in **wpa** share a consistent API, and enable you to return results for both a **plot** or a **table** (data frame):

```R
collaboration_sum(sq_data, return = "plot")
```
<img src="man/figures/collaboration_sum2.jpg" align="center" width=80% />

By passing 'table' to the `return` argument, the function will return a data frame with the relevant summary statistics. 

---

## :package: Package Structure

There are four main types of functions in **wpa**:
1. Standard Analysis 
2. Report Generation
3. Advanced / Support Functions
4. Sample datasets

### 1. Standard Analysis

**Standard Analysis** functions are the most common type of functions in **wpa**. They typically accept a data frame as an input (usually requiring a Standard Person Query), and can return either a pre-designed graph as a ggplot object, or a summary data table as a data frame. 

Examples:
- `collaboration_dist()`
- `meeting_summary()`
- `email_trend()`

For the standard functions, there are six basic **plot types** which could be paired with six different **key metrics**. The six plot types are:

1. `_summary()`: produces a summary bar plot of the metric. 
2. `_dist()`: produces a stacked bar plot of the metric. 
3. `_fizz()`: produces a jittered, 'fizzy drink' plot of the metric.
4. `_line()`: produces a time-series line plot of the metric, with organizational attributes shown as facets.
5. `_trend()`: produces heatmap bars of the metric to show intensity over time.
6. `_rank()`: produces a rank table of all sub-groups (as per a set of organizational attributes) for a given metric. This is the only exception where the function returns a data frame by default, rather than a plot. 

The six key metrics are:

1. `collab`: stands for Collaboration Hours, and uses the metric `Collaboration_hours`.
2. `email`: stands for Email Hours, and uses the metric `Email_hours`.
3. `meeting`: stands for Meeting Hours, and uses the metric `Meeting_hours`.
4. `afterhours`: stands for After-hours Collaboration Hours, and uses the metric `After_hours_collaboration_hours`.
5. `one2one`: stands for one-to-one collaboration hours with direct manager. Uses the metric `Meeting_hours_with_manager_1_on_1`.
6. `workloads`: stands for Work Week Span, and uses the metric `Workweek_span`.

You can combine the **plot types** and the **key metrics** (as prefixes and suffixes) to generate the desired output, e.g. `email_` and `dist` for `email_dist()`.

For more advanced users, there are also a number of **flexible analysis** functions which allow you to generate the plots with _any_ Workplace Analytics metric, where the metric name needs to be supplied in addition to the function. For instance, 

```R
create_bar(sq_data, metric = "Email_hours")
```

would return a similar result as `email_summary(sq_data)`, but where you can replace the metric with one of your own choice. Here are some of the available flexible analysis functions, which are typically prefixed with `create_`:

- `create_bar()`
- `create_bar_asis()`
- `create_boxplot()`
- `create_dist()`
- `create_fizz()`
- `create_line()`
- `create_line_asis()`
- `create_plot_scatter()`
- `create_rank()`
- `create_stacked()`

You can find out more about the feature of each individual function by running `?function` once you have the package loaded.

### 2. Report Generation
**Report Generation** functions are a special class of functions within **wpa** which outputs an interactive HTML report on a specific area based on the data you supply. 

**Examples:**

- `collaboration_report()`
- `capacity_report()`
- `coaching_report()`
- `connectivity_report()`
- `meeting_tm_report()`
- `validation_report()`

### 3. Advanced / support functions
This group consists of miscellaneous functions which either perform a specific piece of analysis (e.g. computing the Information Value score), or are designed to be used with Standard Analysis functions. 

A significant example of this is `export()`, which you can use with a Standard Analysis function to:

- Copy a data frame to clipboard (which can be pasted into Excel)
- Save the generated plot as a PNG or a SVG image file
- Save the data frame to a CSV file

### 4. Sample datasets
There are several pre-loaded demo Workplace Analytics datasets that you can use straight away from the package, to help you explore the functions more easily. Here is a list of them:

- `sq_data`: Standard Person Query
- `mt_data`: Standard Meeting Query
- `em_data`: Hourly Collaboration Query
- `g2g_data`: Group-to-group Query

You can explore the structure of these datasets by running `?sq_data` or `dplyr::glimpse(sq_data)`, for instance.

Check out our package cheat sheet for more information:

<a href="man/figures/wpa cheatsheet_20201116.pdf"><img src="man/figures/wpa cheatsheet_20201116.png" align="center" width=50% /></a>

---

## Vignette

You can browse the vignette by running the following in R:
```R
vignette(topic = "intro-to-wpa", package = "wpa")
```

---

## :hammer: Developers

We welcome contributions to the package! 

### Contributing code
If you would like contribute code to the repo, please read our [Contributor Guide](CONTRIBUTING.md) and [Developer Guide](.github/developer_guide.md). This documentation should provide you all the information you will need to get started.

### Issues or Feature Requests
If you would like to log an issue or submit a feature request, please create a new issue or comment on an existing issue on [GitHub Issues](https://github.com/microsoft/wpa/issues) on this repo.

### Reporting Security Issues
Please do not report security vulnerabilities through public GitHub issues. Please read our Security document [for more details](.github/reporting_security_issues.md).

### Changelog
See [NEWS.md](NEWS.md) for the package changelog.

---

## Code of Conduct

We would ask you to please read the [Microsoft Open Source Code of Conduct](https://opensource.microsoft.com/codeofconduct) prior to engaging with this package.


**Trademarks** 

This project may contain trademarks or logos for projects, products, or services. Authorized use of Microsoft trademarks or logos is subject to and must follow [Microsoft's Trademark & Brand Guidelines](https://www.microsoft.com/en-us/legal/intellectualproperty/trademarks/usage/general). Use of Microsoft trademarks or logos in modified versions of this project must not cause confusion or imply Microsoft sponsorship. Any use of third-party trademarks or logos are subject to those third-party's policies.
