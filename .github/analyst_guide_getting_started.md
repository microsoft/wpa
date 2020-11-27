# Getting Started
This section contains the detailed installation instructions, and a general overview of how functions work in the **wpa** package. 

## Installation

The latest stable version of **wpa** is available in Github. Use the following script in R to automatically download and install it in your local installation of R:

```R
# Check if devtools is installed, if not then install it
if(!"devtools" %in% installed.packages()){
  install.packages("devtools")
}
devtools::install_git(url = "https://github.com/microsoft/wpa.git")

```

The **wpa** is not yet released on CRAN, and therefore `install.packages()` will not work. If you prefer to do a local installation, you can download a .tar file from here. 

## Loading the wpa package
One the installation is complete, you can can load the package with:

```R
library(wpa)
```

You will need to load the package everytime you start a new R session. 


## Importing Workplace Analytics Data
All functions in **wpa** are designed to work with query data from Workpalce Analytics. To load in some data you can use wpa::import_wpa, loading any query file in .CSV format:

```R

person_data <- import_wpa("Desktop/myquery.csv") # Step 2
```

The wpa::import_wpa function works with all workplace analytics query types.

## Exploring the dataset
_Coming soon..._

## Function Structure and additional parameters
_Coming soon..._

## Exporting Plots and Tables
_Coming soon..._
