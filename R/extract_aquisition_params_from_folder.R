#' Extract Acquisition Parameters from All PDF Files in a Folder
#'
#' This function processes all PDF files in a specified folder, extracting acquisition
#' parameters from each Bruker PDF report and returns them as a combined `data.table`.
#'
#' @param folder_path Character. Path to the folder containing the PDF files.
#' @return A `data.table` containing the acquisition parameters for all PDF files.
#' @import data.table
#' @family internal functions
#' @keywords internal

extract_aquisition_params_from_folder <- function(folder_path = NULL) {

  Wert <- Parameter <- value <- filename <- Spectrum_Filename <- NULL

  if(is.null(folder_path)) {
   folder_path <- file.choose()
   folder_path <- dirname(folder_path)
  }

  # Get a list of all PDF files in the folder
  pdf_files <- list.files(folder_path, pattern = "\\.pdf$", full.names = TRUE)

  if (length(pdf_files) == 0) stop("No PDF files found in the folder.")

  # Apply the extraction function to each PDF
  all_params <- rbindlist(lapply(pdf_files, function(pdf_file) {
    tryCatch({
      dt <- extract_aquisition_params(pdf_file)
      dt[, Spectrum_Filename := basename(pdf_file)]  # Add filename column
      return(dt)
    }, error = function(e) {
      message("Error processing file: ", pdf_file, "\n", e$message)
      return(NULL)  # Return NULL if there's an error with a particular file
    })
  }), fill = TRUE)

  all_params[Parameter == "Analysis Method", .N, value]

  return(all_params)
}
