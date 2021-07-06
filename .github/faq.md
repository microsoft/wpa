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

Since 6th April 2021, `install.packages()` should work with **wpa** as it is made available on CRAN on that date. If you continue to experience issues, please create an issue at https://github.com/microsoft/wpa/issues/, and install with the development version in the mean time: 

```R
remotes::install_github(repo = "microsoft/wpa", upgrade = "never") 
```
If the above does not work, please also try:

```R
remotes::install_git(url = "https://github.com/microsoft/wpa.git", upgrade = "never")
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

### How do I install from a development / experimental branch of the package?

If you wish to install a version of the package from any branch **other than the main branch**, you can run the following code:
```R
devtools::install_git(url = "https://github.com/microsoft/wpa.git",
                      branch = "<BRANCH-NAME>", # Replace
                      build_vignettes = TRUE)
```
You should replace the `<BRANCH-NAME>` with the name of your target branch, such as `"feature/network-functions"`.

### Should I install R to my OneDrive or a cloud folder? 

No, you should not. You should never install the default directory for R to OneDrive, as R is a program and you are likely to experience significant cloud syncing issues if you do so. If you use Windows and have experienced this issue, please uninstall and follow the below steps for re-installation:

1. Install RStudio to the default directory, e.g. `C:\Program Files\R\R-4.X.X\library\base\R\Rprofile` 
2. Change the default RStudio package installation location so that it does not save to OneDrive, or another cloud drive. This ensures that OneDrive doesn’t attempt to upload the thousands of files that will be downloaded via the installation of the packages. The R folder where these packages install to is by default created in the Documents folder, which by automatically syncs with OneDrive. We will be moving the installation path to our local `C:` drive instead to prevent this.
3. Create a local folder. Open `File Explorer` -> `C:` drive -> `Create New Folder` -> Name it `"R"`
4. Change permissions:
    - If you installed RStudio in the default directory, the path to your RProfile file is `C:\Program Files\R\R-4.X.X\library\base\R\Rprofile`
    - Navigate to `C:\Program Files\R\R-4.X.X\library\base\`
    - Double-click the base folder so that you’re looking at its contents, which include the R subfolder.
    - Right-click on the R subfolder and select Properties.
    - Click on the `Security` tab.
    - Click on the `Edit` button.
    - Scroll down the Group or user names pane and click on the Users line near the bottom of that list.
    - In the `Permissions for users` pane, click on the `Full Control` checkbox under `Allow`.
    - Click OK and OK again to save and exit these windows.
5. Edit the `Rprofile` file:
    - Ensure that RStudio is closed.
    - Navigate to `C:\Program Files\R\R-4.X.X\library\base\R\`
    - Open the `Rprofile` file with your preferred text editor.
    - Save a copy of the file in the same directory and name it `"RProfileBackup.txt"`.
    - Reopen the `Rprofile` file with your preferred text editor.
    - Replace the string `Sys.getenv("R_USER")` with the string `"C:/R"`
      - Ensure you are replacing the string `Sys.getenv("R_USER")` and NOT the string `Sys.getenv("R_LIBS_USER")`
      - The string `"C:/R"` should align with the directory where you created the folder in step 3a
      - Reference the `RProfileBackup.txt` as a backup if OneDrive uploads the package files after installation
    - Save the file and close out of your text editor and File Explorer.

You should then be able to install the **wpa** library by opening RStudio and running the following code:
```R
# Check if remotes is installed, if not then install it
if(!"remotes" %in% installed.packages()){
  install.packages("remotes")
}
remotes::install_github(repo = "microsoft/wpa", upgrade = "never")

```

You can then restart RStudio with `Ctrl` + `Shift` + `F10` in Windows.

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

### How do I filter by specific date ranges?

We recommend using **dplyr** (which is loaded in as part of **tidyverse**) for data manipulation. To filter down to a specific date range in a Person Query, you can run:

```R
library(wpa)
library(tidyverse) # Or load dplyr

# Read Standard Person Query from local directory
# Assign it to `raw_spq`
raw_spq <- import_wpa("data/standard person query.csv")

# Assign filtered data frame to `clean_spq`
clean_spq <-
	raw_spq %>%
	filter(Date >= as.Date("08/30/2020", "%m/%d/%Y")) %>%
  	filter(Date <= as.Date("12/19/2020", "%m/%d/%Y"))
```

The above example filters the date range from the week commencing 30th of August 2020 to the week ending 19th of November 2020 inclusive. Note that the first date is a Sunday (beginning of the week) and the second date is a Saturday (end of the week). If you query is run by **day**, you should specify the _after_ filter as the exact last day, rather than the Saturday of the week. 

In some scenarios, you may also want to exclude a particular week from the data. You can use a similar approach:

```R
clean_spq2 <-
	clean_spq %>%
	filter(Date != as.Date("11/22/2020", "%m/%d/%Y"))
```

The above line of code excludes the week of 22nd of November 2020, using the operator `!=` to denote _not equal to_. Conversely, you can isolate that single week by replacing `!=` with `==`.

### How do I create a custom HR variable?

The most common way to create a 'custom HR variable' is to create a categorical variable from one or more numeric variables. You may want to do this if you are trying to _bin_ a numeric variable or create a custom rule-based segmentation with Workplace Analytics metrics. 

Here is an example of how to create a categorical variable (`N_DirectReports_NET`) with a numeric variable representing  the number of direct reports, using `dplyr::mutate()` and `dplyr::case_when()` .

```R
library(wpa)
library(tidyverse) # Or load dplyr

clean_spq_with_new_var <-
  clean_spq %>% # Standard Person Query
  mutate(N_DirectReports_NET =
         case_when(NumberofDirectReports == 0 ~ "0",
                   NumberofDirectReports == 1 ~ "1",
                   NumberofDirectReports <= 5 ~ "2 to 5",
                   NumberofDirectReports <= 10 ~ "6 to 10",
                   NumberofDirectReports <= 20 ~ "Up to 20",
                   NumberofDirectReports >= 21 ~ "21 +",
                   TRUE ~ "Not classified"))
```

`dplyr::mutate()` creates a new column, whereas `dplyr::case_when()` runs an if-else operation to classify numeric ranges to the right hand side of the `~` symbol. When the expression on the left hand side evaluates to `TRUE`, the value on the right hand side is assigned to the new column. At the end of the code, you will see that anything that doesn't get classified gets an 'error handling' value. If a value ends up as "Not classified", you should check whether there may be gaps in your `dplyr::case_when()` chunk that is not capturing all the values. 

Once you have created this new variable and checked that the classifications are correct, you can further your analysis by using it as an HR attribute, such as:

```R
clean_spq_with_new_var %>%
	keymetrics_scan(hrvar = "N_DirectReports_NET")
```

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

