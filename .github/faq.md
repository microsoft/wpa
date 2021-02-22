# Frequently Asked Questions

## General

### Why should I use R for Workplace Analytics?

R is an open-source statistical programming language and one of the most popular toolkits for data analysis and data science. There are four key reasons why a Workplace Analytics analyst might choose to use R instead of other alternatives:

1.	R has an **immense package eco-system** with over [17,000 packages](https://cran.r-project.org/web/packages/) for advanced applications from building random forest models to Organizational Network Analysis (ONA) graph visualizations. This enables analysts to perform specialized and more in-depth analysis for specific Workplace Analytics use cases, such as predicting employee churn by analyzing Workplace Analytics metrics. 
2.	There are **no licensing costs** with R, which enables analysts to leverage the powerful functionality with no additional cost.  
3.	R has a code-oriented workflow (as opposed to point-and-click), which promotes **reproducibility**. This is valuable for improving the quality of the analysis and for efficiently replicating analysis with different data samples.
4.	R also has a **substantial user community**, which analysts can access to support and augment their analysis capabilities.

## Installation and Setup

### I cannot install the package with `install.packages()`. Why is that?

The reason why `install.packages()` will not work with **wpa** is because **wpa** is currently only available on GitHub and not on CRAN, and CRAN is where the function `install.packages()` downloads and installs R packages from. Instead, you should use `install_git()`:
```R
remotes::install_git(url = "https://github.com/microsoft/wpa.git") 
```
For more information regarding installation, please see our [Getting Started](https://microsoft.github.io/wpa/analyst_guide_getting_started.html) page.

### I see a warning message about the installation of Rtools. What should I do?

You may see the following message during installation:
```
WARNING: Rtools is required to build R packages, but is not currently installed.
```
This message appears when R is trying to install or update packages that require compilation from binaries, which requires the installation of Rtools. To get around this, we recommend selecting `No` when prompted the question `Do you want to install from sources the packages which needs compilation?` when updated the packages. Alternatively, you can also choose to install [Rtools](https://cran.r-project.org/bin/windows/Rtools/). 

The reason why you may be prompted the _install from sources_ question is because one of the packages that **wpa** is dependent on has updated recently on CRAN, but the binary is not available for your operating system (yet). If you choose `No`, you will not get the most recent version, which in most cases will not be a problem. If you choose `Yes`, the package will be built from source locally, and you will need **Rtools** to do that. [^1]

[^1]: This answer references Jenny Bryan's original response [here](https://community.rstudio.com/t/meaning-of-common-message-when-install-a-package-there-are-binary-versions-available-but-the-source-versions-are-later/2431/2) on RStudio Community.

### I'm still having trouble installing the package. What can I do?

The most common cause for package installation failures is when users try to install the package when some of the dependent packages are loaded. As a best practice, you should always install or update packages in a _fresh_ R Session (no packages loaded and a clear workspace). In RStudio, you can refresh your session with the shortcut `Ctrl + Shift + F10`. You can try the installation command again in a new R Session. 

Make sure you follow the recommend installation steps listed on our [Getting Started](https://microsoft.github.io/wpa/analyst_guide_getting_started.html) page. If installation problems persist, please create an issue at <https://github.com/microsoft/wpa/issues/> and describe the error message that you see on your console. 

### Which version of R should I use?

You are recommended to use the latest stable version of R. You can find the latest version on <https://www.r-project.org/>. 

### How do I install from a developmental / experimental branch of the package?

If you wish to install a version of the package from any branch **other than the main branch**, you can run the following code:
```R
devtools::install_git(url = "https://github.com/microsoft/wpa.git",
                      branch = "<BRANCH-NAME>", # Replace
                      build_vignettes = TRUE)
```
You should replace the `<BRANCH-NAME>` with the name of your target branch, such as `"feature/network-functions"`.

## Import / Export

### Which Workplace Analytics flexible queries can I use with the R package?

Currently, you can use the following flexible queries with the R package:
- Standard Person Query (includes Ways of Working Assessment Query)
- Standard Meeting Query
- Hourly Collaboration Query
- Group-to-Group Query

You can explore functions in **wpa** with [demo datasets](https://microsoft.github.io/wpa/analyst_guide_getting_started.html#demo-data).

### How do I export the outputs of my analysis to Excel?

The `export()` function allows you to export the outputs of your analysis to Excel in two ways.   
1. By setting `method` to `"clipboard"` and passing a data frame through to `export()` , the results will be copied to your clipboard and you can paste it through to Excel. 
2. By setting `method` to `"csv"`, a CSV file will be saved in the specified path relative to your current working directory. The CSV file can then be opened in Excel. 

If you would like to export a _list_ of data frames to Excel where (i) each data frame in the list corresponds to a _Worksheet_ and (ii) the name of each list member corresponds to the _Sheet name_, you can use `writexl::write_xlsx()`.  **wpa** does not depend on functions from **writexl**, so you may need to load and install it separately. 

## Analysis and Visualization

### How do I customize a visual generated from a {wpa} function?

With a few exceptions, most plot outputs returned from **wpa** functions are `ggplot` objects. What this means is that you can edit or add layers to the outputs. See [here](https://microsoft.github.io/wpa/articles/intro-to-wpa.html#customizing-plot-outputs) for an example of how you can make customizations. 

### How do I create a bar chart with a custom metric?

Sometimes you may wish to create an equivalent bar chart of `email_sum()`, but for a custom metric or another Workplace Analytics metric that does not form part of the `*_sum()` family. This is where **flexible functions** are helpful, where you can use `create_bar()` to produce the same visualization by supplying the custom metric name to the `metric` argument. For instance, a bar chart for `"Generate_workload_email_hours"` could be run with:
```R
create_bar(sq_data, metric = "Generate_workload_email_hours")
```
The same person-average computation is used and the same minimum group threshold argument is also available with `create_bar()`. The same is equivalent for the other visualizations:

  - `email_line()` --> `create_line()`
  - `email_trend()` --> `create_trend()`
  - `email_dist()` --> `create_dist()`
  - `email_fizz()` --> `create_fizz()`
  - `email_rank()` --> `create_rank()`

Some functions also act as wrappers around **ggplot2** code where the data is directly plotted without additional person average computation. Examples include `create_bar_asis()` and 
`create_line_asis()`.

### I'm looking for analysis inspirations. Any ideas?

For analysis inspirations, we recommend having a look through our [R playbook](https://microsoft.github.io/wpa/playbook_intro.html) identifying useful analyses or functions for exploring a particular area of Workplace Analytics.

If you have an idea for an analysis and would like this to be documented in the Playbook, you are very welcome to create an issue at <https://github.com/microsoft/wpa/issues/> where we can add your idea and we will credit you as a contributor. You can also use the issue to invite discussion on your analysis idea, although we ask to please keep the discussion relevant to analysis via the R package. 

## Contributing and Engaging

### How do I find help if I have questions about using the package?

You can create an issue at <https://github.com/microsoft/wpa/issues/> if you have questions about using the R package. There are no silly questions: chances are that other users have the same question as you, and creating an issue helps us make the package documentation more user-friendly and help other users such as yourself. 

### Uh-oh, I've noticed a bug with the package. How do I report this?

You can create a bug report at <https://github.com/microsoft/wpa/issues/> if you think you have found a bug with our package. We really appreciate user feedback and we will credit all contributions on our package website. 

### I have some ideas regarding a new feature for the R package. How can I get involved?

If you have an idea in which you would like to collaborate, you can create an issue at <https://github.com/microsoft/wpa/issues/>. If there is code that is ready to be reviewed, you can also fork our repository and create a pull request. We'd like to ask you to read our [Contributor Guide](https://microsoft.github.io/wpa/CONTRIBUTING.html) and [Developer Guide](https://microsoft.github.io/wpa/developer_guide.html) prior to contributing. We will credit your contributions accordingly when your contribution is successfully merged with the package. 

