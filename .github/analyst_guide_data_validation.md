# Data validation
This section covers the topic of data validation before performing your analysis. Data Analytics is a powerful tool for gleaning knowledge from data. But before you can apply any functions to your data set and can start looking for new insights, **data validation** is an essential part of data handling whether you’re in the field collecting information, analyzing data, or preparing to present your data to stakeholders.

## Why Validate?

If your data isn’t accurate from the start, your results will lack of accuracy either. That’s why it’s necessary to verify and validate your data before it is used.

## How to Perform Data Validation?
Before you start you need to ensure data integrity (accuracy and consistency of data over its lifecycle). This can be done with certain base R functions:

1. Data type, e.g. with `str()`
2. Range e.g. with `str()`
3. Uniqueness e.g. with `length(unique(sq_data$PersonId))`
4. Consistent expressions
5. No null values e.g. with `which(is.na(sq_data$PersonId))`

Validating the structure of your data is just as important as validating the data itself. 

The **wpa** package provides additional data validation **functions** to be used prior to embarking on a new analysis:

### Organizational attributes

`hrvar_count()` enables you to create a count of the distinct people by the specified HR attribute:

```R
hrvar_count(sq_data, hrvar = "LevelDesignation")
```

You may also run `check_query()`, which performs a check on a query (data frame) and returns a diagnostic message about the data query to the R console, with information such as date range, number of employees, HR attributes identified, etc.

```R
check_query(sq_data)
```

### Checks on Workplace Analytics metrics

```R
identify_holidayweeks()
```
This function scans a standard query output for weeks where collaboration hours is far
outside the mean. Returns a list of weeks that appear to be holiday weeks. By default, missing values are excluded.

```R
identify_nkw()
```
This function scans a standard query output to identify employees with consistently low
collaboration signals. Returns the % of non-knowledge workers identified by Organization.

```R
identify_inactiveweeks()
```
This function scans a standard query output for weeks where collaboration hours is far
outside the mean for any individual person in the dataset. Returns a list of weeks that
appear to be inactive weeks.

```R
identify_tenure()
```
This function calculates employee tenure based on different input dates. By default it selects the latest Date available with end date= "Date", but you also have flexibility to select a specified date, e.g. "1/1/2020".

### Reports 
Run data validation **reports** prior to starting your analysis:
```R
validation_report()
```
The function generates an interactive HTML report using Standard Person Query data as an input. The report contains a checks on Workplace Analytics query outputs, to provide diagnostic information for the Analyst pre-analysis. This function contains an option to supply an additional meeting query.

```R
subject_validate_report()
```
This functions creates a text mining report in HTML based on Meeting Subject Lines for data validation. It scans a meeting query and highlights meetings with subjects that include common exlusion terms. It is intended to be used by an analyst to validate raw data before conducting additional analysis. Returns a HTML report by default.


## Ready to learn more?

Let's go to the [**Summary Functions**](analyst_guide_summary.html) section, to see how we can analyse different Workplace Analytics Metrics in a similar way.
