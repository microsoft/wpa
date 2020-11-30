# Getting Started
This section contains the detailed installation instructions, and a first introduction on how functions work in the **wpa** package. 

## Installation

The latest stable version of **wpa** is available in Github. You can automatically download and install the latest version it in your local device, by using the following script in R to:

```R
# Check if devtools is installed, if not then install it
if(!"devtools" %in% installed.packages()){
  install.packages("devtools")
}
devtools::install_git(url = "https://github.com/microsoft/wpa.git")

```

This package is not yet released on CRAN, and therefore `install.packages()` will not work. If you prefer to do a local installation, you can download a installation file [here](https://github.com/microsoft/wpa/releases). 

## Loading the wpa package
One the installation is complete, you can load the package with:

```R
library(wpa)
```

You only need to install the package once; however, you will need to load it every time you start a new R session. 

**wpa** is designed to work side by side with other Data Science R packages from [tidyverse](https://www.tidyverse.org/). We generally recommend to load that package too. 

```R
library(tidyverse)
```

## Importing Workplace Analytics Data
All functions in **wpa** are designed to work with query data from Workplace Analytics. To read data into R, you can use the [import_wpa()](https://microsoft.github.io/wpa/reference/import_wpa.html) function, that accepts any query file in .CSV format.

Assuming you have a file called *myquery.csv* on your desktop, you can import it into R using:

```R
setwd("C:/Users/myuser/Desktop/")
person_data <- import_wpa("myquery.csv") 
```

In the code above, [setwd()](https://stat.ethz.ch/R-manual/R-patched/library/base/html/getwd.html) will  set the working directory to the Desktop, then [import_wpa()](https://microsoft.github.io/wpa/reference/import_wpa.html) will read the source CSV that is there, and the contents will be saved to the object person_data (using <- an [Assignment Operator](https://stat.ethz.ch/R-manual/R-devel/library/base/html/assignOps.html)).

The [import_wpa()](https://microsoft.github.io/wpa/reference/import_wpa.html) function works with all workplace analytics query types.

## Demo Data
**wpa** includes a set of pre-loaded Workplace Analytics datasets that you can use to explore the functionality  of this package. We will also use them extensively in this guide. The included data frames are:

1. *sq_data*: A Standard Person Query
2. *dv_data*: A Standard Person Query with outliers
3. *mt_data*: A Standard Meeting Query
4. *em_data*: An Hourly Collaboration Query
5. *g2g_data*: A Group-to-Group Query

## Exploring a Person Query 
We can explore a person query using some of the basic functions from **wpa**. The function [analysis_scope()](https://microsoft.github.io/wpa/reference/hrvar_count.html) can help you to create a basic bar plot, with the count of the distinct individuals by a specified group / HR attribute. 

For example, if we want to know the number of individuals per organization, you can use:

```R
analysis_scope(sq_data, hrvar = "Organization")
```

The [analysis_scope()](https://microsoft.github.io/wpa/reference/hrvar_count.html) function requires you to provide the person query to be used (sq_data) and the HR variable that you will like to cut the data by (hrvar). As we decided to cut the data by organisation, the resulting bar chart will show the number of individuals per organization.

The same comman can also be written using pipes (%>%) to feed a speciifc data frame into a funciton. 

```R
sq_data %>% analysis_scope(hrvar = "Organization") 
```

The notation above is common in tidyverse, and is the one  we will use moving forward. 

Let's now use the  [analysis_scope()](https://microsoft.github.io/wpa/reference/hrvar_count.html) function to explore of other groups. For example, for LevelDesignation:

```R
sq_data %>% analysis_scope(hrvar = "LevelDesignation")
```

Combining this function with the [filter()](https://dplyr.tidyverse.org/reference/filter.html) function from tiyverse, allows us to drill into a specific subset of the data. This is where pipes become powerful, as we can write a single line that takes the original data, applies a filter, and then creates the plot:


```R
sq_data %>% filter(LevelDesignation=="Support")  %>%  analysis_scope(hrvar = "Organization")
```

The default behaviour of most functions in **wpa** is to create a plot. However, this can be altered by adding a `return` argument. If you use `return`="table", the function will now produce a table / data frame with the relevant summary statistics. 

```R
sq_data %>% analysis_scope(hrvar="LevelDesignation", return="table")
```
## Function Structure and additional parameters

All functions in **wpa** follow a similar behaviour, including many common arguments. The following illustrates the basic API of standard analysis functions:

<img src="https://raw.githubusercontent.com/microsoft/wpa/main/man/figures/api-demo.png" align="center" width=80% />

## Exporting Plots and Tables
Tables and plots can be saved with the [export()](https://microsoft.github.io/wpa/reference/export.html) function. This functions allows you to save plots and tables into your local drive:

```R
sq_data %>% analysis_scope(hrvar = "Organization") %>% export()

```

##  Summary: 4 simple steps from data to output

The examples above illustrate how the use of **wpa** can be summarized in 4 simple steps: Load the package, read-in query data, run functions and export results:

```R
library(wpa) # Step 1

person_data <- import_wpa("myquery.csv") # Step 2

person_data %>% analysis_scope() # Step 3

person_data %>% analysis_scope() %>% export() # Step 4

```


Continue to the [**Summary Functions**](analyst_guide_summary.html) section, to explore how we can analyse different Workplace Analytics Metrics in the same way.
