# wpa <img src="man/figures/logo2.png" align="right" width=15% />

  [![R build status](https://github.com/martinctc/wpa/workflows/R-CMD-check/badge.svg)](https://github.com/martinctc/wpa/actions) [![CodeFactor](https://www.codefactor.io/repository/github/microsoft/wpa/badge)](https://www.codefactor.io/repository/github/microsoft/wpa)

## Analyze and Visualize Workplace Analytics data

This is an R package for analyzing and visualizing data from [Microsoft Workplace Analytics](https://www.microsoft.com/microsoft-365/partners/workplaceanalytics).

## Design Principles

- **Simple**: the functions ought to be simple and intuitive, to maximise adoption.
- **Practical**: the functions should prioritise delivering against the most frequently used outputs and analyses.
- **Consistency**: functions should share a broadly consistent set of input arguments and naming conventions. This will help minimise unexpected results and errors when using the package. 
- **Parsimony**: in creating the package, as much of the existing code should be re-used if possible to minimise duplication of work and to make analysis reproducible.
- **Tidy**: the functions from the package are designed to be consistent with tidy principles, and work well with a **dplyr** pipe (`%>%`) workflow.

## With the **wpa** package, you can...

1. **Run prebuilt analysis and visualizations** off Workplace Analytics data with settings for HR variables, privacy threshold, etc.
2. **Generate prebuilt interactive HTML reports**, which cover specific areas e.g. collaboration, connectivity 
3. Leverage **advanced analytics functions**, such as text mining and hierarchical clustering, which are built for Workplace Analytics metrics
4. Integrate analysis of Workplace Analytics data with your R workflow seamlessly

<img src="man/figures/output.gif" align="center" width=80% />

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
## :hammer: Developers

We welcome contributions to the package! If you would like contribute code to the repo, please read our [Contributor Guide](CONTRIBUTING.md) and [Developer Guide](.github/developer_guide.md). This documentation should provide you all the information you will need to get started.

---

## :package: Package Structure

There are three main types of functions in **wpa**:
1. Standard Analysis 
2. Report Generation
3. Advanced / Support Functions

### 1. Standard Analysis

**Standard Analysis** functions are the most common type of functions in **wpa**. They typically accept a data frame as an input (usually requiring a Standard Person Query), and can return either a pre-designed graph as a ggplot object, or a summary data table as a data frame. 

Examples:
- `collaboration_dist()`
- `meeting_summary()`
- `email_trend()`
- `mgrrel_matrix()`

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
This final group consists of miscellaneous functions which either perform a specific piece of analysis (e.g. computing the Information Value score), or are designed to be used with Standard Analysis functions. 

A significant example of this is `export()`, which you can use with a Standard Analysis function to:

- Copy a data frame to clipboard (which can be pasted into Excel)
- Save the generated plot as a PNG or a SVG image file
- Save the data frame to a CSV file

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

## Security

Microsoft takes the security of our software products and services seriously, which includes all source code repositories managed through our GitHub organizations, which include [Microsoft](https://github.com/Microsoft), [Azure](https://github.com/Azure), [DotNet](https://github.com/dotnet), [AspNet](https://github.com/aspnet), [Xamarin](https://github.com/xamarin), and [our GitHub organizations](https://opensource.microsoft.com/).

If you believe you have found a security vulnerability in any Microsoft-owned repository that meets Microsoft's [Microsoft's definition of a security vulnerability](https://docs.microsoft.com/en-us/previous-versions/tn-archive/cc751383(v=technet.10)), please report it to us as described below.

## Reporting Security Issues

**Please do not report security vulnerabilities through public GitHub issues.**

Instead, please report them to the Microsoft Security Response Center (MSRC) at [https://msrc.microsoft.com/create-report](https://msrc.microsoft.com/create-report).

If you prefer to submit without logging in, send email to [secure@microsoft.com](mailto:secure@microsoft.com).  If possible, encrypt your message with our PGP key; please download it from the the [Microsoft Security Response Center PGP Key page](https://www.microsoft.com/en-us/msrc/pgp-key-msrc).

You should receive a response within 24 hours. If for some reason you do not, please follow up via email to ensure we received your original message. Additional information can be found at [microsoft.com/msrc](https://www.microsoft.com/msrc).

Please include the requested information listed below (as much as you can provide) to help us better understand the nature and scope of the possible issue:

  * Type of issue (e.g. buffer overflow, SQL injection, cross-site scripting, etc.)
  * Full paths of source file(s) related to the manifestation of the issue
  * The location of the affected source code (tag/branch/commit or direct URL)
  * Any special configuration required to reproduce the issue
  * Step-by-step instructions to reproduce the issue
  * Proof-of-concept or exploit code (if possible)
  * Impact of the issue, including how an attacker might exploit the issue

This information will help us triage your report more quickly.

If you are reporting for a bug bounty, more complete reports can contribute to a higher bounty award. Please visit our [Microsoft Bug Bounty Program](https://microsoft.com/msrc/bounty) page for more details about our active programs.

## Preferred Languages

We prefer all communications to be in English.

## Policy

Microsoft follows the principle of [Coordinated Vulnerability Disclosure](https://www.microsoft.com/en-us/msrc/cvd).
