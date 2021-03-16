# Data validation

This section covers the topic of **how to validate Workplace Analytics data**. Before you apply any functions to your data set and start looking for new insights, it is recommended that you perform data validation. This best practice is applicable regardless of whether your aim is to explore the data, to establish a baseline, or to perform advanced analytics.

## Why validate?

There are several reasons why you should validate your Workplace Analytics data.

1. There may be gaps, anomalies, or errors in the organizational data, such as missing data or excessive / insufficient granularity. This may require rectifying at source, or the resulting data should be interpreted differently, e.g. any biases caveated or accounted for. 
2. Outliers may exist in Workplace Analytics data, and often for very legitimate reasons. For instance, collaboration hours for a particular week or employee may be significantly low due to public holidays or paid time off. If these outliers are not surfaced and addressed accordingly, the interpretation of the data may be incorrect.

In a nutshell, it is good practice to have a comprehensive understanding of the data context and checks for common biases, errors, and anomalies prior to analysis, as otherwise we would risk the quality and the reliability of the analysis. 

## Know what you are looking at

Before you begin with data validation, it's helpful to know what data set you are looking at, which includes information such as: 

  - What query type is loaded
  - Number of unique employees in the dataset
  - Date range of the dataset
  - Organizational attributes int he dataset

This can all be done with `check_query()`:

```R
library(wpa)
check_query(sq_data)
```

The resulting output would look something like the following:

```
The data used is a Person Query.

There are 1034 employees in this dataset.

Date ranges from 2019-11-03 to 2020-01-26.

There are 9 (estimated) HR attributes in the data:
`Domain`, `FunctionType`, `LevelDesignation`, `Region`, `Organization`, `attainment`, `TimeZone`, `IsInternal`, `IsActive`

There are 1034 active employees out of all in the dataset.

Variable name check:

`Collaboration_hours` is used instead of `Collaboration_hrs` in the data.

`Instant_Message_hours` is used instead of `Instant_message_hours` in the data.
```

The below functions are also helpful for exploring your data:

1. Get all column names, e.g. `names(sq_data)`
2. Check object type, e.g. `class(sq_data$Date)`
2. Get summary statistics, e.g. `summary(sq_data)`
3. Compute number of unique values, e.g. `length(unique(sq_data$PersonId))`
5. Get an overview of the data, e.g. `dplyr::glimpse(sq_data)`, or `skimr::skim(sq_data)`.

Validating the structure of your data is just as important as validating the data itself. You may wish to check that your data is correctly imported into R if you observe certain anomalies, such as: 

- Unexpected / misspelt column names
- Unexpected number of rows in the data
- Unexpectedly high number of missing or unique values
- `Date` variable is showing up as a variable type that is neither _character_ nor _Date_ type

## Data Validation Report

The easiest way to perform data validation with the **wpa** package is to run the data validation report:

```R
 # `spq_df` is your Standard Person Query data
validation_report(spq_df)
```
This generates an HTML report in your working directory which contains a series of checks against your data, including:

- Workplace Analytics Settings
- Organizational Data Quality
- M365 Data Quality

You can find a demo output of the validation report [here](https://microsoft.github.io/wpa/report-demo/validation-report-demo.html).  Note that `validation_report()` only runs with a Standard Person Query, but you can supply an optional meeting query to include checks against meeting subject lines. To do so, you should run: 

```R
# Assuming:
# `spq_df` is your Standard Person Query data
# `mt_df` is your Standard Meeting Query data
validation_report(spq_df, 
                  meeting_data = mt_df)
```

The data validation report provides you with a series of recommendations on whether you should adjust certain settings or consider certain assumptions before proceeding with your analysis. After you've made the relevant adjustments, you can run the 'cleaned' dataset through `validation_report()` again to make sure that the potential issues have been caught out. 

Note that `validation_report()` only provides recommendations based on common scenarios observed with Workplace Analytics data. When deciding whether to make an adjustment, you should consider other factors such as quality of organizational data, context, and other known collaboration norms within the organization. 

## Individual functions

The **wpa** package provides additional data validation functions to be used prior to embarking on a new analysis. These functions make up the majority of the automated checks of `validation_report()`, where you can run them individually to extract more detailed information (for instance, the report may identify certain employees as _non-knowledge workers_, but does the distribution of these workers with respect to organization make sense? ). The key data validation functions are described below.

### Organizational attributes

 `check_query()` performs a check on a query (data frame) and returns a diagnostic message about the data query to the R console, with information such as date range, number of employees, HR attributes identified, etc.

```R
check_query(sq_data)
```

`hrvar_count()` enables you to create a count of the distinct people by the specified HR attribute:

```R
hrvar_count(sq_data, hrvar = "LevelDesignation")
```

To run a blanket analysis for all the organizational attributes in the dataset, you can run `hrvar_count_all()` instead. 

Also check out: 

- `identify_privacythreshold()`
- `track_HR_change()`
- `identify_tenure()`

Click on the linked functions to find out more.

### M365 Data Quality

There are three common reasons for removing certain employees or weeks from the data:

1. A given _week_ is likely a **public holiday** which impacts a significant proportion of the organization, e.g. Christmas, New Year.
2.  An _employee_ is a **non-knowledge worker** - in the sense that collaboration via emails and meetings is not a key part of their role. 
3. An employee is off work for _certain weeks_ due to annual leave, sabbaticals, etc. which do not necessarily coincide with public holidays.  

There are three functions in **wpa** to address each these respective scenarios:

1. `identify_holidayweeks()` identifies weeks where collaboration hours are low outliers for the entire organization. 
2. `identify_nkw()` identifies employees with overall low collaboration signals, based on average collaboration hours. In addition to non-knowledge workers, this method would also capture any effective part-timers or freelancers, where their collaboration hours are significantly low. 
3. `identify_inactiveweeks()` identifies individual weeks where collaboration hours are low outliers for the entire organization. 

Functions (1) to (3) all come with options to return only the 'clean' dataset or the original dataset with an appended flag to identify the anomalous persons/weeks. As per above, you can click on the linked functions to find out more. 

Below is an example of one might create a 'clean' dataset using the functions above:

```R
library(wpa)
library(tidyverse) # You may also just load dplyr

clean_spq <-
  raw_wowa %>% # Loaded in as a Ways of Working query
  standardise_pq() %>%  # Standardize variable names - not necessary if SPQ
  # Force the use of Teams metrics - optional
  mutate(Collaboration_hours =
           select(.,
                  Email_hours,
                  Meeting_hours,
                  Unscheduled_Call_hours,
                  Instant_Message_hours) %>%
           apply(1, sum)) %>%
  identify_nkw(collab_threshold = 4.99999, return = "data_cleaned") %>% # >= 5 CH
  identify_inactiveweeks(sd = 2, return = "data_cleaned") %>% # 2 SD
  filter(Date >= as.Date("08/30/2020", "%m/%d/%Y")) %>% # Trim start date
  filter(Date <= as.Date("03/07/2021", "%m/%d/%Y")) %>% # Trim end date
  filter(Date != as.Date("11/22/2020", "%m/%d/%Y")) # Remove certain weeks
```

### Reports 

In addition to the main `validation_report()`, there is also an additional report you can run for validating whether you set your meeting exclusion rule appropriately. 

`subject_validate_report()` creates a text mining report in HTML based on a standard meeting query. Meeting subject lines from the `Subject` variable are analyzed, and meetings with subjects that include common exclusion terms are highlighted. The report will help you understand whether your data may be including a significant proportion of non-meetings in your meeting collaboration data. 


## Ready to learn more?

Let's go to the [**Summary Functions**](analyst_guide_summary.html) section, to see how we can analyse different Workplace Analytics Metrics in a similar way.
