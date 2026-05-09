#' @title Read xml peaklists generates ultrahigh-resolution MS analyses
#' @name read_xml_peaklist
#' @description This function reads multiple FTMS peaklist files in XML format. The function requires the package 'xml2'.
#' that are generated from Bruker FTICRMS and Thermo Orbitrap instruments.
#' A single peaklists containing the file_paths is returned as a data.table
#' A dialog window requests the path to the required directory (recursive = FALSE by default).

#' @inheritParams main_docu
#' @param folder_path (Optional) The path to the directory containing the XML files.
#' If not provided, the user will be prompted to choose a folder path interactively.

#' @family internal functions
#' @keywords misc internal
#' @import data.table utils
#' @return
#' A `data.table` containing the combined peaklists extracted from all XML files
#' in the selected folder. Each row represents a single peak. The table includes:
#'
#' \itemize{
#'   \item `filename` – name of the XML file from which the peak originates.
#'   \item `mz` – mass-to-charge ratio of the peak.
#'   \item `sn` – signal-to-noise ratio (if available in the XML).
#'   \item `res` – peak resolution (if available in the XML).
#'   \item `i_magnitude` – peak intensity.
#' }
#'
#' Files that contain no peak entries return a row with `filename` only.
#' If the package `xml2` is not installed, the function returns `NULL`
#' after printing an informative message.

read_xml_peaklist <- function(folder_path = NULL, ...) {

  if (requireNamespace("xml2", quietly = TRUE)) {
    # Define the folder path
    # folder_path <- r"(\\smb.isibhv.dmawi.de\projects-noreplica\p_ume\Spektren FTMS\UFZ 2023.11 Bareth Mixing Exp\_peaklists)"

    # Select a folder if folder_path = NULL
    #if (is.null(folder_path)) folder_path <- utils::choose.dir()

    if (is.null(folder_path)) {
      folder_path <- file.choose()
    }

    folder_path <- dirname(folder_path)

    # Get list of XML files in the folder
    file_list <-
      list.files(path = folder_path,
                 pattern = "\\.xml$",
                 full.names = TRUE,
                 ...)

    # Initialize a list to store data tables
    dt_list <- list()

    # Function to parse individual XML files
    parse_xml_file <- function(file) {
      # Parse the XML file
      xml_data <- xml2::read_xml(file)

      # Find the peak list node and extract data
      peaks <- xml2::xml_find_all(xml_data, "//ms_peaks/pk")

      # Check if peaks are found
      if (length(peaks) == 0) {
        return(data.table::data.table(filename = basename(file)))
      }

      # Verify attributes extraction
      mz_test <- xml2::xml_attr(peaks[1], "mz")
      sn_test <- xml2::xml_attr(peaks[1], "sn")
      res_test <- xml2::xml_attr(peaks[1], "res")
      i_test <- xml2::xml_attr(peaks[1], "i")

      .msg("mz: %i", mz_test)
      .msg("sn: %i", sn_test)
      .msg("res: %i", res_test)
      .msg("i: %i", i_test)

      # Extract individual values from the attributes of <pk> nodes
      mz_values <- xml2::xml_attr(peaks, "mz") |>  as.numeric()
      sn_values <- xml2::xml_attr(peaks, "sn") |>  as.numeric()
      res_values <- xml2::xml_attr(peaks, "res")  |>  as.numeric()
      i_values <- xml2::xml_attr(peaks, "i") |>  as.numeric()

      # Create a data.table with the extracted values
      dt <- data.table(
        filename = basename(file),
        mz = mz_values,
        sn = sn_values,
        res = res_values,
        i_magnitude = i_values
      )

      return(dt)
    }

    # Loop over each file and parse
    for (file in file_list) {
      message("Processing file: ", basename(file))
      dt <- parse_xml_file(file)
      dt_list[[file]] <- dt
    }

    # Combine all data.tables into one
    pl <- data.table::rbindlist(dt_list, use.names = TRUE, fill = TRUE)

    # Save the final data.table
    # save(pl, file = "peaklist_uncalibrated.Rdata")
    return(pl)
  } else {
    message("The package 'xml2' is not installed.")
  }
}
