#' @title Scan meeting subject and highlight items for review
#'
#' @description
#' This functions scans a meeting query and highlights meetings with subjects that include common exlusion terms. It is intended to be used by an analyst to validate raw data before conducting additional analysis.
#' Returns a summary in the console by default.
#' Additional option to return the underlying data with a flag of items for review.
#'
#'
#' @family Data Validation
#'
#' @param data A meeting query in the form of a data frame.
#' @param return A string specifying what to return.
#' Returns a message in the console by default, where 'text' is passed in `return`.
#' When 'table' is passed, a summary table with common terms found is printed.
#' When 'data' is passed, a the original data with an additional flag column is returned as a data frame.
#'
#' @return
#' Returns a message in the console by default, where 'text' is passed in `return`.
#' When 'table' is passed, a summary table with common terms found is printed.
#' When 'data' is passed, a the original data with an additional flag column is returned as a data frame.
#'
#' @export

subject_validate <- function(data, return = "text"){

## Check inputs
required_variables <- c("Subject")

## Error message if variables are not present. Nothing happens if all present
data %>% check_inputs(requirements = required_variables)

# Define common "test" words:
reminders <-
  c(
    "departure",
    "flight",
    "cancelled",
    "room",
    "booking",
    "placeholder",
    "save the date",
    "reminder",
    "change password",
    "time sheet",
    "timesheet",
    "workday time",
    "dental",
    "dentist",
    "doctor",
    "dr" ,
    "dr." ,
    "medical" ,
    "physical therapy" ,
    "surgery" ,
    "leave" ,
    "day off" ,
    "from home" ,
    "half day" ,
    "office closed" ,
    "maternity",
    "OOF" ,
    "OOO" ,
    "ooto" ,
    "out of office" ,
    "paternity" ,
    "PTO" ,
    "telework" ,
    "sick leave" ,
    "time off" ,
    "vacation" ,
    "WFH",
    "enter time",
    "timecard",
    "time card",
    "log in",
    "log out",
    "payday",
    "pay day",
    "go home",
    "fill out",
    "clock in",
    "clock out",
    "pay bills"
  )

leisure <-
  c(
    "baseball",
    "basketball",
    "bball",
    "tennis",
    "book club",
    "football",
    "pilates",
    "soccer",
    "swim",
    "yoga",
    "zumba",
    "game",
    "gym",
    "meditation",
    "walk dog",
    "haircut",
    "toastmasters"
  )

holidays <-
  c(
    "diwali",
    "easter",
    "holiday",
    "independence day",
    "labor day",
    "labour day",
    "new year",
    "yom kippur"
  )

socials <-
  c(
    "party",
    "birthday",
    "bday",
    "b day",
    "church",
    "dinner",
    "school",
    "happy hour",
    "potluck",
    "bagel",
    "baby shower"
  )

test_words  <- append(reminders, leisure)
test_words  <- append(test_words, holidays)
test_words  <- append(test_words, socials)

# take suubjet lines to lower case.names
data$Subject <- tolower(data$Subject)

# 3.3 subject lines to lower case.names
results <-
  test_words %>%
  purrr::map(function(x){

    index <- grepl(pattern = x, x = data$Subject)
    CheckSum <- sum(index)
    CheckMean <- mean(index) * 100

    data.frame(Word = x,
               Cases = CheckSum,
               Perc = round(CheckMean, digits=2))
  }) %>% bind_rows()


results <-
  results %>%
  arrange(desc(Cases)) %>%
  filter(Cases > 0)

# 3.3 Flag meetings that have an issue:
Pattern <- paste(test_words, collapse="|")
data$subjectFlag <- grepl(Pattern, data$Subject)
table(data$subjectFlag)

# 3.4 Display error
## Get statistics
TotalN <- nrow(data)
FlagN <- sum(data$subjectFlag, na.rm = TRUE)
FlagProp <- mean(data$subjectFlag, na.rm = TRUE)
FlagPropF <- paste0(round(FlagProp * 100, 1), "%") # Formatted

## Flag Messages

Warning_Message <- paste("[Warning]  ", FlagN," meetings (",FlagPropF, "of ", TotalN, ") require your attention as they contain common exclusion terms.")

Pass_Message <-  paste("[Pass] No subject lines with common exclusion terms are present in the ", TotalN, " meetings analysed.")

  if(FlagN >= 0 ){
    FlagMessage <- Warning_Message
  } else if(FlagN == 0){
    FlagMessage <- Pass_Message
  }

  if(return == "text"){

  FlagMessage

    } else if(return == "table"){

    return(results)

    } else if(return == "data"){

    return(data)

    }else {

    stop("Please enter a valid input for `return`.")

    }

}

