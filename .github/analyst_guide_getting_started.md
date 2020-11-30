# Getting started
This section contains the detailed installation instructions, and a first overview on how functions work in the **wpa** package. 

## Installation

The latest stable version of **wpa** is available in Github. You can automatically download and install it in your local device, by running the following code in R:

```R
# Check if devtools is installed, if not then install it
if(!"devtools" %in% installed.packages()){
  install.packages("devtools")
}
devtools::install_git(url = "https://github.com/microsoft/wpa.git")

```

This package is not yet released on CRAN, and therefore `install.packages()` will not work. If you prefer to proceed with a local installation, you can download a installation file [here](https://github.com/microsoft/wpa/releases). 

## Loading the wpa package
Once the installation is complete, you can load the package with:

```R
library(wpa)
```

You only need to install the package once; however, you will need to load it every time you start a new R session. 

**wpa** is designed to work side by side with other Data Science R packages from [tidyverse](https://www.tidyverse.org/). We generally recommend to load that package too:

```R
library(tidyverse)
```

## Importing Workplace Analytics data
To read data into R, you can use the [import_wpa()](https://microsoft.github.io/wpa/reference/import_wpa.html) function. This funciton accepts any query file in .CSV format.

Assuming you have a file called *myquery.csv* on your desktop, you can import it into R using:

```R
setwd("C:/Users/myuser/Desktop/")
person_data <- import_wpa("myquery.csv") 
```

In the code above, [setwd()](https://stat.ethz.ch/R-manual/R-patched/library/base/html/getwd.html) will  set the working directory to the Desktop, then [import_wpa()](https://microsoft.github.io/wpa/reference/import_wpa.html) will read the source CSV. The contents will be saved to the object person_data (using "<-" as an [Assignment Operator](https://stat.ethz.ch/R-manual/R-devel/library/base/html/assignOps.html)).


## Demo data
The **wpa** package includes a set of demo Workplace Analytics datasets that you can use to explore the functionality of this package. We will also use them extensively in this guide. The included datasets are:

1. *sq_data*: A Standard Person Query
2. *dv_data*: A Standard Person Query with outliers
3. *mt_data*: A Standard Meeting Query
4. *em_data*: An Hourly Collaboration Query
5. *g2g_data*: A Group-to-Group Query

## Exploring a person Query 
We can explore the sq_data person query using the [analysis_scope()](https://microsoft.github.io/wpa/reference/hrvar_count.html) function from **wpa**. This function create a basic bar plot, with the count of the distinct individuals within a specified group (an HR attribute in your query). 

For example, if we want to know the number of individuals in sq_data per organization, we can use:

```R
analysis_scope(sq_data, hrvar = "Organization")
```

This function requires you to provide a person query (sq_data) and the HR variable that you will be used to group the data (hrvar). As we have specifified the Organization attribute, the resulting bar chart will show the number of individuals per organization.

The same R code can be written using a Forward-Pipe Operator (%>%) to feed our query into a funciton. The notation is common in R data science applications, and is the one we will use moving forward. 


```R
sq_data %>% analysis_scope(hrvar = "Organization") 
```

Let's now use this function to explore of other groups. For example:

```R
sq_data %>% analysis_scope(hrvar = "LevelDesignation")
sq_data %>% analysis_scope(hrvar = "TimeZone")
```

We can combine analysis_scope() with the [filter()](https://dplyr.tidyverse.org/reference/filter.html) function from tiyverse. This will allows us to drill into a specific subset of the data. This is where the Forward-Pipe Operators (%>%) become very useful, as we can write a single line that takes the original data, applies a filter, and then creates the plot:

```R
sq_data %>% filter(LevelDesignation=="Support")  %>%  analysis_scope(hrvar = "Organization")
```

Most functions in **wpa** create plot by default, but can change their behaviour by adding a `return` argument. If you add `return`="table" to this function it will now produce a table with the count of the distinct individuals by group.

```R
sq_data %>% analysis_scope(hrvar="LevelDesignation", return="table")
```
## Function structure and additional parameters

All functions in **wpa** follow a similar behaviour, including many common arguments. The following illustrates the basic API of standard analysis functions:

<img src="https://raw.githubusercontent.com/microsoft/wpa/main/man/figures/api-demo.png" align="center" width=80% />

So far we have explored the `hrvar` and `return` arguments. We will use the `mingroup` in the next section. 

## Exporting Plots and Tables
Tables and plots can be saved with the [export()](https://microsoft.github.io/wpa/reference/export.html) function. This functions allows you to save plots and tables into your local drive.

One again, adding an additional forward-Pipe operator we can write:

```R
sq_data %>% analysis_scope(hrvar = "Organization") %>% export()

```

##  Summary: Four steps from data to output

The examples above illustrate how the use of **wpa** can be summarized in 4 simple steps: Load the package, read-in query data, run functions and export results. The script below illustrates this funcitonality:

```R
library(wpa) # Step 1

person_data <- import_wpa("myquery.csv") # Step 2

person_data %>% analysis_scope() # Step 3

person_data %>% analysis_scope() %>% export() # Step 4

```

## Ready to learn more?

Let's go to the [**Summary Functions**](analyst_guide_summary.html) section, to see how we can analyse different Workplace Analytics Metrics in a similar way.
