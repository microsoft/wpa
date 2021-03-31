# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Read a Workplace Analytics query in '.csv' using and create a '.fst'
#'   file in the same directory for faster reading
#'
#' @description Uses `import_wpa()` to read a Workplace Analytics query in
#'   '.csv' and convert this into the serialized '.csv' format which is much
#'   faster to read. The 'fst' package must be installed, or an error message is
#'   returned.
#'
#' @details
#' The [fst](https://www.fstpackage.org/) package provides a way to serialize
#' data frames in R which makes loading data much faster than CSV.
#' `import_to_fst()` converts a CSV file into a FST file in the specified
#' directory.
#'
#' Once this FST file is created, it can be read into R using
#' `fst::read_fst()`. Since `import_to_fst()` only does conversion but not
#' loading, it should normally only be run once at the beginning of each piece
#' of analysis, and `fst::read_fst()` should take over the job of data loading
#' at the start of your analysis script.
#'
#' Internally, `import_to_fst()` uses `import_wpa()`, and additional arguments
#' to `import_wpa()` can be passed with `...`.
#'
#' @param path String containing the path to the Workplace Analytics query to be
#'   imported. The input file must be a CSV file, and the file extension must be
#'   explicitly entered, e.g. `"/files/standard query.csv"`. The converted FST
#'   file will be saved in the same directory with a different file extension.
#' @param ... Additional arguments to pass to `import_wpa()`.
#'
#' @return
#' There is no return value. A file with '.fst' extension is written to the same
#' directory where the '.csv' file is read in.
#'
#' @family Import and Export
#'
#' @export
import_to_fst <- function(path, ...){

  # Check if fst is installed
  if(!"fst" %in% utils::installed.packages()){

    stop('`fst` is not installed.
         Run `install.packages("fst")` to install the package.')

  }

  temp_df <- import_wpa(path, ...)

  path_fst <- gsub(pattern = "csv$",
                   replacement = "fst",
                   x = path)

  fst::write_fst(temp_df, path_fst)

  message("\nFST file sucessfully created at ", path_fst, ".")

  message("\nYou can now load the FST file with `fst::read_fst()`.")
}
