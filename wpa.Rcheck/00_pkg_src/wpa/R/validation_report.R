# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Generate a Data Validation report in HTML
#'
#' @description
#' The function generates an interactive HTML report using
#' Standard Person Query data as an input. The report contains checks on
#' Workplace Analytics query outputs to provide diagnostic information
#' for the Analyst prior to analysis.
#'
#' An additional Standard Meeting Query can be provided to perform meeting
#' subject line related checks. This is optional and the validation report can
#' be run without it.
#'
#' @details
#' For your input to `data` or `meeting_data`, please use the function
#' `wpa::import_wpa()` to import your csv query files into R. This function will
#' standardize format and prepare the data as input for this report.
#'
#' If you are passing a Ways of Working Assessment query instead of a Standard
#' Person query to the `data` argument, please also use `standardise_pq()` to
#' make the variable names consistent with a Standard Person Query.
#'
#' Since `v1.6.2`, the variable `Call_hours` is no longer a pre-requisite to run
#' this report. A note is returned in-line instead of an error if the variable
#' is not available.
#'
#' @section Checking functions within `validation_report()`:
#'   - `check_query()`
#'   - `flag_ch_ratio()`
#'   - `hrvar_count_all()`
#'   - `identify_privacythreshold()`
#'   - `identify_nkw()`
#'   - `identify_holidayweeks()`
#'   - `subject_validate()`
#'   - `identify_tenure()`
#'   - `flag_outlooktime()`
#'   - `identify_shifts()`
#'   - `track_HR_change()`
#'
#' You can browse each individual function for details on calculations.
#'
#' @param data A Standard Person Query dataset in the form of a data frame.
#' @param meeting_data An optional Meeting Query dataset in the form of a data
#'   frame.
#' @param hrvar HR Variable by which to split metrics, defaults to "Organization"
#'  but accepts any character vector, e.g. "Organization"
#' @param path Pass the file path and the desired file name, _excluding the file
#'   extension_.
#' @param hrvar_threshold Numeric value determining the maximum number of unique
#'   values to be allowed to qualify as a HR variable. This is passed directly
#'   to the `threshold` argument within `hrvar_count_all()`.
#' @param timestamp Logical vector specifying whether to include a timestamp in
#'   the file name. Defaults to `TRUE`.
#' @param na_values Character vector of values to be treated as missing in addition
#'   to NA values. Defaults to c("NA", "N/A", "#N/A", " ").
#'
#' @section Creating a report:
#'
#' Below is an example on how to run the report.
#'
#' ```
#' validation_report(dv_data,
#'                   meeting_data = mt_data,
#'                   hrvar = "Organization")
#' ```
#'
#'
#' @importFrom purrr map_if
#' @importFrom dplyr `%>%`
#'
#' @family Reports
#' @family Data Validation
#'
#' @inherit generate_report return
#'
#' @export
validation_report <- function(data,
                              meeting_data = NULL,
                              hrvar = "Organization",
                              path = "validation report",
                              hrvar_threshold = 150,
                              timestamp = TRUE,
                              na_values = c("NA", "N/A", "#N/A", " ")){

  ## Create timestamped path (if applicable)
  if(timestamp == TRUE){
    newpath <- paste(path, wpa::tstamp())
  } else {
    newpath <- path
  }

  ## Handle variable name consistency
  data <- qui_stan_c(data)
  data <- qui_stan_im(data)

  ## Dynamic: if meeting data is not available
  if(is.null(meeting_data)){
    subline_obj <- "[Note] Subject line analysis is unavailable as no meeting query is supplied."
    subline_obj2 <- ""
  } else {
    subline_obj <- meeting_data %>% subject_validate(return = "text")
    subline_obj2 <- meeting_data %>% subject_validate(return = "table")
  }

  ## Dynamic: if `HireDate` is not available
  if("HireDate" %in% names(data)){
    tenure_obj <- data %>% identify_tenure(return = "text") %>% suppressWarnings()
    tenure_obj2 <- data %>% identify_tenure(return = "plot") %>% suppressWarnings()
  } else {
    tenure_obj <- "[Note] Tenure analysis is unavailable as the data has no `HireDate` variable."
    tenure_obj2 <- ""
  }

  ## Dynamic: if `WorkingStartTimeSetInOutlook` and `WorkingEndTimeSetInOutlook` is not available
  wktimes_var <- c("WorkingStartTimeSetInOutlook", "WorkingEndTimeSetInOutlook")
  wktimes_msg <- "[Note] Outlook hours analysis is unavailable as the data does not have the following variables:"

  if(all(wktimes_var %in% names(data))){
    wktimes_obj <- data %>% flag_outlooktime(return = "text")
    shift_obj <- data %>% identify_shifts(return = "plot")
  } else {
    wktimes_obj <- paste(wktimes_msg, paste(wrap(wktimes_var, "`"), collapse = ", "), collapse = "\n")
    shift_obj <- paste(wktimes_msg, paste(wrap(wktimes_var, "`"), collapse = ", "), collapse = "\n")
  }

  ## Dynamic: Track HR changes
  mtry <- try(track_HR_change(data, hrvar = hrvar), silent = TRUE)

  if (!inherits(mtry, "try-error")) {
    trackhr_obj <- data %>% track_HR_change(hrvar = hrvar, return = "plot")
    trackhr_obj2 <- data %>% track_HR_change(hrvar = hrvar, return = "table")
  } else {
    trackhr_obj <- "[Error!] unable to parse HR changes."
    trackhr_obj2 <- ""
  }

  ## Dynamic: create mock `Call_hours` variable
  callthres_p <- NULL
  callthres <- NULL


  if("Call_hours" %in% names(data)){
    callthres_p <- paste(
      ">",
      data %>%
        flag_extreme(
          metric = "Call_hours",
          threshold = 40,
          person = TRUE,
          return = "text"
        )
    )

    callthres <-
      paste(
        ">",
        data %>% flag_extreme(
          metric = "Call_hours",
          threshold = 40,
          person = FALSE,
          return = "text"
        )
      )

  } else {

    callthres_p <-
      "> [Note] Checks for `Call_hours` is not available due to missing variable."
    callthres <- callthres_p

  }


  ## Outputs as accessed here
  ## Can be data frames, plot objects, or text
  output_list <-
    list(read_preamble("blank.md"), # Header - Data Available
         data %>% check_query(return = "text", validation = TRUE),

         read_preamble("blank.md"), # Header - 1.1 Workplace Analytics Settings
         read_preamble("outlook_settings_1.md"),

         shift_obj, # See `identify_shifts()` dynamic treatment above

         read_preamble("outlook_settings_2.md"),
         paste(">", wktimes_obj),
         paste(">", data %>% flag_ch_ratio(return = "text")),
         read_preamble("outlook_settings_3.md"),

         read_preamble("meeting_exclusions_1.md"), #item 9, Header - 1.2 Meeting Exclusions
         paste(">", subline_obj),
         subline_obj2,
         read_preamble("meeting_exclusions_2.md"),

         read_preamble("organizational_data_quality.md"), #13, Header - 2. Organizational Data Quality
         read_preamble("attributes_available.md"),#14
         data %>% hrvar_count_all(return = "table", threshold = hrvar_threshold, na_values = na_values),

         read_preamble("groups_under_privacy_threshold_1.md"), #16, Header - 2.2 Groups under Privacy Threshold
         paste(">", data %>% identify_privacythreshold(return="text")),
         read_preamble("groups_under_privacy_threshold_2.md"),
         data %>% identify_privacythreshold(return="table"),

         read_preamble("distribution_employees_key_attributes.md"), #20, Header - 2.3 Distribution employees key attributes
         data %>% hrvar_count(hrvar = hrvar, return = "plot"),
         data %>% hrvar_count(hrvar = hrvar, return = "table"),

         read_preamble("updates_organizational_data.md"), #23, Header - 2.4 Updates to Organizational Data
         read_preamble("blank.md"), #placeholder for track_HR_change message obj,
		     trackhr_obj,
		     trackhr_obj2,

         read_preamble("quality_tenure_data.md"), #27, Header - 2.5 Quality Tenure Data
         paste(">", tenure_obj), # Text
         tenure_obj2, # Plot

         read_preamble("m365_data_quality.md"), #30, Header - 3. M365 Data Quality
         read_preamble("population_over_time.md"), #Header - 3.1
         data %>% hr_trend(return = "plot"),

         read_preamble("nonknowledge_workers.md"), #33, Header - 3.2 Non-knowledge workers
         paste(">", data %>% identify_nkw(return = "text")),
         data %>% identify_nkw(return = "data_summary"),

         read_preamble("holiday_weeks_1.md"), #36, Header - 3.3 Company Holiday weeks
         paste(">", data %>% identify_holidayweeks(return = "text")),
         read_preamble("holiday_weeks_2.md"),
         data %>% identify_holidayweeks(return = "plot"),

         read_preamble("inactive_weeks_1.md"), #40, Header - 3.4 Inactive weeks
         paste(">", data %>% identify_inactiveweeks(return = "text")),
         read_preamble("inactive_weeks_2.md"),

         read_preamble("extreme_values.md"), #43, Header - 3.5 Extreme values
         paste(">",data %>% flag_extreme(metric = "Email_hours", threshold = 80, person = TRUE, return = "text")),
         paste(">",data %>% flag_extreme(metric = "Email_hours", threshold = 80, person = FALSE, return = "text")),

         paste(">",data %>% flag_extreme(metric = "Meeting_hours", threshold = 80, person = TRUE, return = "text")),
         paste(">",data %>% flag_extreme(metric = "Meeting_hours", threshold = 80, person = FALSE, return = "text")),

         callthres_p,
         callthres,

         paste(">",data %>% flag_extreme(metric = "Instant_Message_hours", threshold = 40, person = TRUE, return = "text")),
         paste(">",data %>% flag_extreme(metric = "Instant_Message_hours", threshold = 40, person = FALSE, return = "text")),

         paste(">",data %>% flag_extreme(metric = "Conflicting_meeting_hours", threshold = 70, person = TRUE, return = "text")),
         paste(">",data %>% flag_extreme(metric = "Conflicting_meeting_hours", threshold = 70, person = FALSE, return = "text")),

		     paste(">",data %>% flag_extreme(metric = "Collaboration_hours", threshold = 0, person = TRUE, mode = "equal", return = "text")),
		     paste(">",data %>% flag_extreme(metric = "Collaboration_hours", threshold = 0, person = FALSE, mode = "equal", return = "text"))
		     ) %>%

    purrr::map_if(is.data.frame, create_dt, rounding = 0) %>%
    purrr::map_if(is.character, md2html)

  ## Title of the outputs
  title_list <-
    c("Data Available",
      "Query Check",

      "1. Workplace Analytics Settings",
      "1.1 Outlook Settings",
      "",
      "",
      "",
      "",
      "",
      "1.2 Meeting Exclusion Rules",
      "",
      "",
      "",

      "2. Organizational Data Quality",
      "2.1 Attributes Available",
      "",
      "2.2 Groups Under Privacy Threshold",
      "",
      "",
      "",

      "2.3 Distribution of Employees in Key Attributes",
      "",
      "",

      "2.4 Updates to Organizational Data",
      "",
      "",
      "",

      "2.5 Quality of Tenure Data",
      "",
      "",

      "3. M365 Data Quality",
      "3.1 Population Over Time",
      "",
      "3.2 Non-knowledge Workers",
      "",
      "",
      "3.3 Company Holiday Weeks",
      "",
      "",
      "",
      "3.4 Inactive Weeks",
      "",
      "",
      "3.5 Extreme Values",
      "3.5.1 Extreme values: Email",
      "",
      "3.5.2 Extreme values: Meeting",
      "",
      "3.5.3 Extreme values: Calls",
      "",
      "3.5.4 Extreme values: IM",
      "",
      "3.5.5 Extreme values: Conflicting Meetings",
      "",
      "3.5.6 Extreme values: Zero collaboration",
      ""
      )

  # Set header levels
  n_title <- length(title_list)
  levels_list <- rep(4, n_title)
  levels_list[c(1, 3, 14, 31)] <- 2 # Section header

  generate_report(title = "Data Validation Report",
                  filename = newpath,
                  outputs = output_list,
                  titles = title_list,
                  subheaders = rep("", n_title),
                  echos = rep(FALSE, n_title),
                  levels = levels_list,
                  theme = "cosmo",
                  preamble = read_preamble("validation_report.md")) # See inst/preamble/validation_report.md
}
