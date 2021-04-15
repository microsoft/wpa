# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Export 'wpa' outputs to CSV, clipboard, or save as images
#'
#' @description
#' A general use function to export 'wpa' outputs to CSV, clipboard, or save as
#' images. By default, `export()` copies a data frame to the clipboard. If the
#' input is a 'ggplot' object, the default behaviour is to export a PNG.
#'
#' @author Martin Chan <martin.chan@@microsoft.com>
#'
#' @param x Data frame or 'ggplot' object to be passed through.
#' @param method Character string specifying the method of export.
#' Valid inputs include:
#'   - `"clipboard"` (default if input is data frame)
#'   - `"csv"`
#'   - `"png"` (default if input is 'ggplot' object)
#'   - `"svg"`
#'   - `"jpeg"`
#'   - `"pdf"`
#' @param path If exporting a file, enter the path and the desired file name,
#'   _excluding the file extension_. For example, `"Analysis/SQ Overview"`.
#' @param timestamp Logical vector specifying whether to include a timestamp in
#'   the file name. Defaults to `TRUE`.
#' @param width Width of the plot
#' @param height Height of the plot
#'
#' @return
#' A different output is returned depending on the value passed to the `method`
#' argument:
#'   - `"clipboard"`: no return - data frame is saved to clipboard.
#'   - `"csv"`: CSV file containing data frame is saved to specified path.
#'   - `"png"`: PNG file containing 'ggplot' object is saved to specified path.
#'   - `"svg"`: SVG file containing 'ggplot' object is saved to specified path.
#'   - `"jpeg"`: JPEG file containing 'ggplot' object is saved to specified path.
#'   - `"pdf"`: PDF file containing 'ggplot' object is saved to specified path.
#'
#' @importFrom utils write.csv
#'
#' @family Import and Export
#'
#' @export

export <- function(x,
                   method = "clipboard",
                   path = "wpa export",
                   timestamp = TRUE,
                   width = 12,
                   height = 9){

  ## Create timestamped path (if applicable)

  if(timestamp == TRUE){
    newpath <- paste(path, wpa::tstamp())
  } else {
    newpath <- path
  }

  ## Force method to png if is.ggplot and method not appropriate
  if(is.ggplot(x) & method %in% c("clipboard", "csv")){
    message("Input is a 'ggplot' object. Defaulted to exporting as PNG...")
    method <- "png"
  }

  ## Main export function
  if(method == "clipboard"){
    copy_df(x)
    message(c("Data frame copied to clipboard.\n",
              "You may paste the contents directly to Excel."))

  ## Export option: CSV
  } else if(method == "csv"){

    newpath <- paste0(newpath, ".csv")

    write.csv(x = x, file = newpath)

  ## Export option: any ggsave methods
  } else if(method %in% c("png", "svg", "jpeg", "pdf")){

    newpath <- paste0(newpath, ".", method)

    ggsave(filename = newpath, plot = x, width = width, height = height)

  } else {

    stop("Please check inputs. Enter `?export` for more details.")

  }
}
