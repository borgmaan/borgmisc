#' Trim leading and trailing whitespace
#'
#' Similar to \emph{trim} functions in other languages, \code{trim} removes
#' leading and trailing whitespace from character vectors.
#' @export
#' @examples
#' spacey_string = " hi there  "
#' trim(spacey_string)

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#' Wrapper to xlsx2csv command line client
#'
#' \link[=https://github.com/dilshod/xlsx2csv]{xlsx2csv} is an excellent
#' command line utility for converting \emph{Excel} files to CSV or TSV format.
#' This function provides an easy interface to that utility. Users must have
#' xlsx2csv installed via \emph{pip}.
#'
#' @param infile Path to xlsx file to convert.
#' @param outfile Path to output file.
#' @param delimiter Field separator to be used in the output file.
#' @param sheet_num Sheet number to convert. Users must provide this or
#' @export
#' @examples
#' spacey_string = " hi there  "
#' trim(spacey_string)

xlsx2csv <- function(infile, outfile, delimiter = 'tab', sheet_num = NULL) {

  if (!file.exists('/usr/local/bin/xlsx2csv')) {
    stop("Please install xlsx2cxv in /usr/local/bin/ or symlink it there.")
  }

  if (is.null(sheet_num)) {
    stop("You must specify either a sheet name or a sheet number...")
  }

  if (!file.exists(infile)) {
    stop("Provided input file does not exist.")
  }

  if (file.exists(outfile)) {
    warning("Provided outfile already exists. Overwriting...")
  }

  if (delimiter %in% c('tab', 'comma')) {

    if (delimiter == 'tab') dstring = "-d 'tab'"
    else dstring = ""

    cmd = paste("xlsx2csv", dstring, "-s", sheet_num, infile, ">", outfile)
    system(cmd)


  } else {
    stop("Delimiter must be either 'tab' or 'comma'.")
  }
}
