# :hammer: Developer Guide

This is guide for "developers", or anyone who is looking to contribute code to the {wpa} R package. 

## Design Principles

Please try to adhere to the following package design principles before proceeding with code contribution.

- **Simple**: the functions ought to be simple and intuitive to maximise adoption.
- **Practical**: the functions should prioritise delivering against the most frequently used outputs and analyses.
- **Consistency**: functions should share a broadly consistent set of input arguments and naming conventions. This will help minimise unexpected results and errors when using the package. 
- **Parsimony**: in creating the package, as much of the existing code should be re-used if possible to minimise duplication of work and to make analysis reproducible.
- **Tidy**: the functions from the package are designed to be consistent with tidy principles, and work well with a **dplyr** pipe (`%>%`) workflow.

## Pre-requisites

You should have the following installed before starting:

1. [R](https://www.r-project.org/)
2. [RStudio Desktop](https://rstudio.com/products/rstudio/download/#download)
3. [GitHub Desktop](https://desktop.github.com/) (Highly recommended, unless you are very familiar with git)

You should also have the following packages installed. These should all be available from CRAN:
1. {devtools}
2. {tidyverse}
3. {roxygen2}

Once these are all installed, you should be ready to roll!

---

## :hammer: Quick start guide - For developers

### Update package and installing to local directory

Once you've made changes to a script, here is how to update the package and install it locally on your computer. Run:

#### 1. Generating documentation

```R
roxygen2::roxygenise()
```
`roxygenise()` generates documentation files for the R package, and makes them available to the user.

#### 2. R CMD Checks
Next, run this to build and check the package. You will need {devtools} installed.

```R
devtools::check()
```
This step runs a R CMD check against the package, to check that there will be no errors and issues if somebody installs the package. This step can take around 5 minutes long, and usually it is worth running this only after you've implemented all the desired changes towards the end. 

#### 3. Install the package

If everything runs smoothly, run this final line to install the local package. Make sure that your working directory is set to the package!

```R
devtools::install()
```

---

## To increment or change a package version

Each package release has a corresponding package version (e.g. 1.4.2.) To change that package version number for a particular release, simply go into the `DESCRIPTION` at the project root folder of the {wpa} package. You can edit this with any notepad software or RStudio. 

Change the values that come after `Version: ` to change the package version. Prior to incrementing the package version, please align with the main developer team on the agreed version release details. 

## How to release a package version

Our current protocol for package release is to distribute it as a zipped file or a tar ball. The advantage of this approach is that users do not need to have a GitHub account, and this package release can be distributed via SharePoint or even email. Our current approach is to offer **both** options for installation. 

Once you are happy with the package and all checks have passed, you can run the following to generate a **zipped** distribution of the package. If your platform is Windows, this should generate a zipped file one level above your project directory:

```R
devtools::build(binary = TRUE)
```

The code to generate a **tarball** distribution is similar:
```R
devtools::build(binary = FALSE)
```

After the **zipped file** and the **tarball** files have been generated, they are ready for distribution. 

## How to build the PDF documentation

It is good practice to distribute a package with an updated version of the documentation, so users won't be using outdated documentation. You can generate the familiar PDF documentation by running the following:
```R
devtools::build_manual()
```

## A short guide to adding a function to the package

1. Let us assume that you have written a new function, e.g. a checking function that returns TRUE if PersonId exists, and returns FALSE if it does not).
1. Once you check the function and ensure that it works, save this into a R file under the "R" folder of the package directory.
    - Ensure that your current working directory is set to the package directory. You can either open the RStudio session with the package .Rproj file, or run `setwd()` to change you working directory. Run `getwd()` to check what your current working directory is.
    - Ensure that the relevant roxygen headers are present (see [here](https://roxygen2.r-lib.org/articles/rd.html) for more reference).
    - Whilst it is possible to have more than one function saved in a single R file, we recommend saving them in their own files with their own names for clarity. For instance, the function `collaboration_dist()` would be saved in a file called _collaboration_dist.R_.
1. Follow the steps in the Developer Guide above to generate the documentation (`roxygen2::roxygenise()`), check the package (`devtools::check()`), and install the package (`devtools::install()`). 
1. Once you are sure that everything works, open **GitHub Desktop**, and **check that you have selected the relevant branch**. If you are unsure what your changes are, it is always recommended that you work on a new branch so that you do not introduce any breaking changes to the master branch.
1. _Fetch / pull_ from the branch. This ensures that your local package is updated with changes that others have made to the code.
1. _Commit_ your changes, and add an intuitive commit message so others will know what you have changed to the code. Please see the wiki page of this repo for a style guide on the commit messages.
1. _Push_ your changes to the branch. If there are no errors, you should be able to see the changes on the GitHub repository!

## Using GitHub
*It's on its way...*

## Documenting functions with {roxygen2}
*It's on its way...*

## :link: Useful links

- [Sharing Internal R Packages](https://support.rstudio.com/hc/en-us/articles/115000239587-Sharing-Internal-R-Packages)
- [Writing an R package from scratch by Hilary Parker](https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/)
- [Semantic versioning](https://semver.org/)
