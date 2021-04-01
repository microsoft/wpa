## Test environments
* local R installation, R 3.6.3
* ubuntu 16.04 (on travis-ci), R 3.6.3
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 note

* This is a new release.

## Submission 1.4.2

Spotted and fixed an issue with package names in single quotes in title and 
description.

## Submission 1.4.1

Addressed following comments from previous CRAN submission

- Having package names in single quotes in title and description.
- Add \value to .Rd files and explain function results.
- Removed examples for unexported functions.
- Removed examples that write in user's home filespace.
- Removed usage of `installed.packages()`, using `find.package()` instead.
