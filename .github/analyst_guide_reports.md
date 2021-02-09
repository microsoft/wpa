# Reports

**wpa** comes with a number of functions that allow you to generate automated HTML reports that are based off Workplace Analytics flexible queries. You can find a list of the reports available below.

Text in _italics_ indicate the required flexible query to run the report. _SPQ_ stands for _Standard Person Query_, _SMQ_ for _Standard Meeting Query_, and _HCQ_ stands for _Hourly Collaboration Query_.

### Reports for Baseline Analysis

- Report on Collaboration - `collaboration_report()` - [Demo](https://microsoft.github.io/wpa/report-demo/collaboration-report.html) - _SPQ_
- Report on Capacity - `capacity_report()` - [Demo](https://microsoft.github.io/wpa/report-demo/capacity-report.html) - _SPQ_
- Report on Coaching - `coaching_report()` - [Demo](https://microsoft.github.io/wpa/report-demo/coaching-report.html) - _SPQ_
- Report on Connectivity - `connectivity_report()` - [Demo](https://microsoft.github.io/wpa/report-demo/connectivity-report.html) - _SPQ_

### Reports for Data Validation
- Automated Data Validation Report - `validation_report()` - [Demo](https://microsoft.github.io/wpa/report-demo/validation-report-demo.html) - _SPQ_ + _SMQ_ (Optional)
- Subject Line Validation Report - `subject_validate_report()` - _SMQ_

### Reports for Specific Analyses
- Meeting Text Mining Report - `meeting_tm_report()` - [Demo](https://microsoft.github.io/wpa/report-demo/meeting-text-mining-report.html) - _SMQ_
- Working Patterns Report - `workpatterns_report()` - [Demo](https://microsoft.github.io/wpa/report-demo/workpatterns-report.html) - _HCQ_

## Example Code

You can generate a report on collaboration by running the following code:

```R
library(wpa)
sq_data %>% collaboration_report()
```

This will generate an HTML report in the location of your working directory, and can take a few minutes to run if you have a large dataset loaded. The resulting HTML report can be opened with any web browser. 

For best and consistent results, please ensure that your dataset is loaded in using the `import_wpa()` function, which ensures that your variables are loaded in as the correct types required for **wpa** functions. 