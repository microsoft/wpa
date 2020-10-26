# wpa <img src="man/figures/logo2.png" align="right" width=15% />

  [![R build status](https://github.com/martinctc/wpa/workflows/R-CMD-check/badge.svg)](https://github.com/martinctc/wpa/actions)

## Analyze and Visualize Workplace Analytics data

This is an R package for analyzing and visualizing data from Microsoft Workplace Analytics.

## Design Principles

- **Simple**: the functions ought to be simple and intuitive, to maximise adoption.
- **Practical**: the functions should prioritise delivering against the most frequently used outputs and analyses.
- **Consistency**: functions should share a broadly consistent set of input arguments and naming conventions. This will help minimise unexpected results and errors when using the package. 
- **Parsimony**: in creating the package, as much of the existing code should be re-used if possible to minimise duplication of work and to make analysis reproducible.
- **Tidy**: the functions from the package are designed to be consistent with tidy principles, and work well with a {dplyr} pipe workflow.

## What you can do with this package

- Run analysis and visualisations off WpA data with prebuilt settings for HR variables, privacy threshold, etc.
- Create pre-designed ggplot visualisations that can be further customised, or inserted straight into your analysis flow as required
- Generate interactive HTML reports that summarise a dimension of your WpA data, such as **collaboration**. 

<img src="man/figures/output.gif" align="center" width=80% />

## :rocket: Quick start guide - For users

### Cloning / downloading the package

Click the green 'Clone or download' button on this repository, and save the package in a directory of your choice. It doesn't matter where you save this, as the installation process will install the package together with the rest of R packages.

If you have GitHub Desktop installed, simply click 'Open in Desktop' and follow the instructions to save the repository into a local directory.

### How to install the package from local directory - method #1

1. Either open the RProject file in the directory of the {wpa} package (the one titled _wpa.Rproj_), or navigate to it within R using `setwd()` by providing an absolute filepath. For instance, if you've downloaded the package to _C:/Users/Documents/wpa_, you'd need to run:

```R
setwd("C:/Users/Documents/wpa")
```
This step is the same regardless of whether you manually cloned the repository or did this via GitHub Desktop.

2. Make sure you have the package {devtools} installed. If you don't or unsure whether you do, run:

```R
install.packages("devtools")
```

Once that's done, run `devtools::install()`. You may be asked to update your packages, which is optional for the installation. We recommend just entering a blank line for a quick installation.

3. Once the {wpa} package is installed, you can load the package by running:

```R
library(wpa)
```
### How to install the package from local directory - with a tarball via RStudio

This is a method to install the package with a tarball release.

1. Download the latest version of {wpa} to your desktop (make sure it has the ".tar.gz" extension).
2. Open RStudio.
3. Click on the Packages tab in the bottom right window.
4. Click "Install."
5. Select install from "Package Archive File."
6. Select the {wpa} package file from your desktop.
7. Click install.
8. Load the library with the command `library(wpa)`.

### Examples

The package comes shipped with a Standard Query dataset (`sq_data`), so you can start exploring functions without having to read in any data. Most functions in {wpa} share a consistent API, and enable you to return results for both a **plot** or a **table** (data frame):

```R
collaboration_sum(sq_data, return = "plot")
```
<img src="man/figures/collaboration_sum2.jpg" align="center" width=80% />

By passing 'table' to the `return` argument, the function will return a data frame with the relevant summary statistics. 

---
## :hammer: Developers

We welcome contributions to the package! If you would like contribute code to the repo, please read our [developer / contribution guide](.github/developer_guide.md). This documentation should provide you all the information you will need to get started.

---

## Vignette

You can browse the vignette by running the following in R:
```R
vignette(topic = "intro-to-wpa", package = "wpa")
```
---

## Code of Conduct

We would ask you to please read the [Microsoft Open Source Code of Conduct](https://opensource.microsoft.com/codeofconduct) prior to engaging with this package.


**Trademarks** 

This project may contain trademarks or logos for projects, products, or services. Authorized use of Microsoft trademarks or logos is subject to and must follow [Microsoft's Trademark & Brand Guidelines](https://www.microsoft.com/en-us/legal/intellectualproperty/trademarks/usage/general). Use of Microsoft trademarks or logos in modified versions of this project must not cause confusion or imply Microsoft sponsorship. Any use of third-party trademarks or logos are subject to those third-party's policies.
