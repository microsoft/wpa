#' @title Identify the WPA metrics that have the biggest change between two periods.
#'
#' @description
#' This function uses the Information Value algorithm to predict which WPA metrics are most explained by the change in dates.
#'
#' @param data Person Query as a dataframe including date column named "Date"
#' This function assumes the data format is MM/DD/YYYY as is standard in a WpA query output.
#' @param before_start Start date of "before" time period in YYYY-MM-DD. Defaults to earliest date in dataset.
#' @param before_end End date of "before" time period in YYYY-MM-DD
#' @param after_start Start date of "after" time period in YYYY-MM-DD. Defaults to day after before_end.
#' @param after_end End date of "after" time period in YYYY-MM-DD. Defaults to latest date in dataset.
#' @param mybins Number of bins to cut the data into for Information Value analysis. Defaults to 10.
#' @param return  Specify whether to return a summary table or detailed Excel and PDF files
#' Specify "table" or "detailed" for outputs. Defaults to table.
#'
#' @import dplyr
#' @import Information
#'
#'
#' @family Flexible Input
#'
#' @export

IV_by_period <-
  function(data,
           before_start = min(as.Date(data$Date, "%m/%d/%Y")),
           before_end,
           after_start = as.Date(before_end) + 1,
           after_end = max(as.Date(data$Date, "%m/%d/%Y")),
           mybins = 10,
           return = "table") {
    ## Check inputs
    required_variables <- c("Date",
                            "PersonId")
    ## Error message if variables are not present
    ## Nothing happens if all present
    data %>%
      check_inputs(requirements = required_variables)

    daterange_1_start <- as.Date(before_start)
    daterange_1_end <- as.Date(before_end)
    daterange_2_start <- as.Date(after_start)
    daterange_2_end <- as.Date(after_end)

    WpA_dataset <- data %>% mutate(Date = as.Date(Date, "%m/%d/%Y"))


    # Check for dates in data file
    if (daterange_1_start < min(WpA_dataset$Date) |
        daterange_1_start > max(WpA_dataset$Date) |
        daterange_1_end < min(WpA_dataset$Date) |
        daterange_1_end > max(WpA_dataset$Date) |
        daterange_2_start < min(WpA_dataset$Date) |
        daterange_2_start > max(WpA_dataset$Date) |
        daterange_2_end < min(WpA_dataset$Date) |
        daterange_2_end > max(WpA_dataset$Date)) {
      stop('Dates not found in dataset')
      geterrmessage()
    }

    # Create variable => Period
    WpA_dataset_table <-
      WpA_dataset %>%
      mutate(
        Period = case_when(
          Date >= daterange_1_start &
            Date <= daterange_1_end ~ "Before",
          Date >= daterange_2_start &
            Date <= daterange_2_end ~ "After"
        )
      ) %>% filter(Period == "Before" | Period == "After")

    WpA_dataset_table <-
      WpA_dataset_table %>% mutate(outcome = case_when(Period == "Before" ~ "0",
                                                       Period == 'After' ~ "1"))

    # Make the use of exponential notation less likely
    options(scipen = 10)

    # De-select character columns
    train <-
      WpA_dataset_table %>%
      transform(outcome = as.numeric(outcome)) %>%
      select_if(is.numeric)

    # Filter out NAs
    train <- train %>%
      filter(rowSums(is.na(.[, ])) < 1)

    # Rename Outcome Variable
    # train <- transform(train, outcome = as.numeric(outcome))
    train <- rename(train, 'Outcome' = "outcome")
    colnames(train)


    # Calculate Odds
    odds <-
      sum(train$Outcome) / (length(train$Outcome) - sum(train$Outcome))
    lnodds <- log(odds)

    # IV Analysis
    IV <- create_infotables(data = train, y = "Outcome", bins = mybins)

    # if(return == "detailed"){
    #   # Ranking variables using  IV
    #   wb <- createWorkbook()
    #   addWorksheet(wb, "Ranking")
    #   writeDataTable(wb, "Ranking", x = data.frame(IV$Summary))
    #
    #   # Export Individual Tables
    #   for(i in names(IV$Tables)){
    #     print(i)
    #     addWorksheet(wb, substr(i, start = nchar(i) - 30, stop = nchar(i)))
    #     temp <- IV$Tables[[i]]
    #     temp$ODDS <- exp(temp$WOE + lnodds)
    #     temp$PROB <- (temp$ODDS / (temp$ODDS + 1))
    #     writeDataTable(wb, substr(i, start = nchar(i) - 30, stop = nchar(i)) , x = data.frame(temp))
    #   }
    #
    #   # Save Workbook
    #   saveWorkbook(wb, "Output_IV_v2.xlsx", overwrite = TRUE)
    #
    #   # Plot Graph
    #   pdf("Output_IV_v2.pdf")
    #   plot_infotables(IV, IV$Summary$Variable[], same_scale=TRUE)
    #   dev.off()
    # } else
    if (return == "table") {
      # Store all individual dataframes
      Tables <- c()
      Summary <- data.frame(IV$Summary)
      Tables$Summary <- Summary
      for (i in names(IV$Tables)) {
        temp <- IV$Tables[[i]]
        temp$ODDS <- exp(temp$WOE + lnodds)
        temp$PROB <- (temp$ODDS / (temp$ODDS + 1))
        Tables[[i]] <- create_dt(temp, rounding = 2)
      }

      # Return ranking table
      return(create_dt(Tables$Summary, rounding = 2))
      # print("Access individual metrics via Outputs[[metric_name]], e.g., Outputs[[Workweek_span]]")

      # # Store each variable's plot
      # plots <- c()
      # for (i in names(IV$Tables)) {
      #   plots[[i]] <- plot_infotables(IV, i)
      # }

     } else {
      stop("Please enter a valid input for `return`, either detailed or table.")
    }

  }
