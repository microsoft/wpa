# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Produce a skim summary of meeting hours
#'
#' @description
#' This function returns a skim summary in the console
#' when provided a standard query in the input.
#'
#' @param data A standard person query data in the form of a data frame.
#'
#' @param return String specifying what to return. This must be one of the following strings:
#'   - `"message"`
#'   - `"text"`
#'   - `"table"`
#'
#' See `Value` for more information.
#'
#' @return
#' A different output is returned depending on the value passed to the `return` argument:
#'   - `"message"`: message in console.
#'   - `"text"`: string.
#'   - `"table"`: data frame.
#'
#' @import dplyr
#'
#' @family Meetings
#'
#' @examples
#' meeting_skim(sq_data)
#'
#' @export
meeting_skim <- function(data, return = "message"){

  required_vars <-
    c(
      "PersonId",
      "Meeting_hours",
      "Conflicting_meeting_hours",
      "Multitasking_meeting_hours",
      "Redundant_meeting_hours__lower_level_",
      "Redundant_meeting_hours__organizational_",
      "Low_quality_meeting_hours"
    )

  used_vars <- dplyr::intersect(names(data), required_vars)

  key_output <-
    data %>%
    select(used_vars) %>%
    summarise_at(vars(-PersonId), ~sum(.)) %>% # sum total
    tidyr::gather(HourType, Hours, -Meeting_hours) %>%
    mutate_at(vars(Hours), ~./Meeting_hours) %>%
    mutate(RawHours = Hours * Meeting_hours)

  mh_total <- round(key_output$Meeting_hours[1])

  mh_lowqualityprop <-
    key_output %>%
    filter(HourType == "Low_quality_meeting_hours") %>%
    pull(Hours) %>%
    `*`(100) %>%
    round() %>%
    paste0("%")

  bracket <- function(text){
    paste0("(", text, ")")
  }

  extract_prop <- function(filt_chr){
    key_output %>%
      filter(HourType == filt_chr) %>%
      pull(Hours) %>%
      `*`(100) %>%
      round() %>%
      paste0("%")
  }

  extract_raw <- function(filt_chr){

    key_output %>%
      filter(HourType == filt_chr) %>%
      pull(RawHours) %>%
      round()
  }

  combine_extracts <- function(filt_chr, keyword){

    out <-
      key_output %>%
      filter(HourType == filt_chr)

    if(nrow(out) == 0){

      return("")

    } else {

      paste(
        ">>>",
        extract_raw(filt_chr),
        "are",
        keyword,
        bracket(extract_prop(filt_chr))
      )

    }
  }

  print_text <-
    paste("There are",
          mh_total,
          "total meeting hours across the analysis population.\n",

          combine_extracts(
            filt_chr = "Low_quality_meeting_hours",
            keyword = "low quality"
          ),
          "\n",

          combine_extracts(
            filt_chr = "Redundant_meeting_hours__organizational_",
            keyword = "redundant"
          ),
          "\n",

          combine_extracts(
            filt_chr = "Conflicting_meeting_hours",
            keyword = "conflicting"
          ),
          "\n",

          combine_extracts(
            filt_chr = "Multitasking_meeting_hours",
            keyword = "multitasking"
          )
          )

  if(return == "message"){

    message(print_text)

    } else if(return == "text"){

      print_text <- gsub(pattern = ">>>", replacement = " - ", x = print_text)
      print_text

    } else if(return == "table"){

      key_output

    } else {

      stop("Please check `return`")

    }
}
