# Getting Started
This section contains the detailed installation instructions, and a general overview of how functions work in the **wpa** package. 

## Installation

The latest stable version of **wpa** is available in Github. Use the following script in R to automatically download and install it in your local device:

```R
# Check if devtools is installed, if not then install it
if(!"devtools" %in% installed.packages()){
  install.packages("devtools")
}
devtools::install_git(url = "https://github.com/microsoft/wpa.git")

```

This package is not yet released on CRAN, and therefore `install.packages()` will not work. If you prefer to do a local installation, you can download a .tar file [here](https://github.com/microsoft/wpa/). 

## Loading the wpa package
One the installation is complete, you can load the package with:

```R
library(wpa)
```

You only need to install the package once, but will need to load it everytime you start a new R session. 

## Importing Workplace Analytics Data
All functions in **wpa** are designed to work with query data from Workpalce Analytics. To read data into R, you can use the [import_wpa()](https://microsoft.github.io/wpa/reference/import_wpa.html) function, that accepts any query file in .CSV format.

Assuming you have a file called *myquery.csv* on your desktop, you can import it into R using:

```R
person_data <- import_wpa("Desktop/myquery.csv") 
```

The [import_wpa()](https://microsoft.github.io/wpa/reference/import_wpa.html) function works with all workplace analytics query types.


## Demo Data
**wpa** includes a set of pre-loaded workplace analytics datasets that you can use to explore the functionalty of this package. We will also use them extensively in this guide. The included dataframes are:

1. *sq_data*: A Standard Person Query
2. *mt_data*: A Standard Meeting Query
3. *em_data*: An Hourly Collaboration Query
4. *g2g_data*: A Group-to-Group Query

## Exploring a Person Query 
We can explore a person query using some of the basic functions from **wpa**. The function [analysis_scope](https://microsoft.github.io/wpa/reference/hrvar_count.html) can help you to create a basic bar plot, with the count of the distinct individuals by a specified group / HR attribute. 

For example, if we want to know the number of individuals per organization, you can use:

```R
analysis_scope(sq_data, hrvar = "Organization") # Option 1
sq_data %>% analysis_scope(hrvar = "Organization") # Option 2 

```

The syntaxis in option 2, illustrates how you can use pipes (%>%). This is the common notation in tidyverse, and the one we will use in this guide moving forward. 

This function can be use to explore of other groups. For example, for LevelDesignation:

```R
sq_data %>% analysis_scope(hrvar = "LevelDesignation")
```

The default behaviour of most functions in **wpa** is to create a plot. However, this can be altered by adding a `return` argument. If you use `return`="table", the function will now produce a table / data frame with the relevant summary statistics. 

```R
sq_data %>% analysis_scope(hrvar="LevelDesignation", return="table")
```
## Function Structure and additional parameters

All funcitons in **wpa** follow a similar behaviou, and include many common arguments. The following illustrates the basic API of standard analysis functions:

<img src="https://github.com/microsoft/wpa/blob/main/man/figures/api-demo.png" align="center" width=80% />

## Exporting Plots and Tables
_Coming soon..._
