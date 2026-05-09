#' @title Calculate Number of Molecular Formula Assignments per Peak
#' @name calc_number_assignment
#' @family calculations
#' @description This function calculates the number of molecular formula (mf) assignments
#' for each individual peak (peak_id) within a specified mass spectrum (ms_id). It counts
#' the occurrences of molecular formulas assigned to each peak and returns a vector of
#' counts corresponding to the number of assignments for each unique combination of
#' mass spectrum ID, peak ID, and molecular formula.
#'
#' @inheritParams main_docu
#' @param ms_id A vector containing the mass spectrum ID for each peak.
#' @param peak_id A vector containing the peak ID for each peak.
#' @keywords misc utils
#' @return A vector of integer counts representing the number of
#' molecular formula assignments for each unique combination
#' of mass spectrum ID, peak ID, and molecular formula.
#' @examples
#' ms_ids <- c("file1", "file1", "file2", "file2", "file3")
#' peak_ids <- c(1, 2, 2, 3, 4)
#' mfs <- c("C10H10N2O8", "C10H12N2O8", "C10H10N2O8", "C10H11NOS4", "C10H24N4O2S")
#' n_assignments <- calc_number_assignment(ms_id = ms_ids, peak_id = peak_ids, mf = mfs)
#' print(n_assignments)
#'
#  Using the molecular formula demo data:
#' mf_data_demo[, calc_number_assignment(file_id, peak_id, mf)]
#' @export

calc_number_assignment <- function(ms_id, peak_id, mf, ...) {

  # Check that all vectors have equal length and are longer than 0
  if (length(ms_id) != length(peak_id) || length(ms_id) != length(mf) || length(ms_id) == 0) {
    stop("The lengths of ms_id, peak_id, and mf must all be the same and greater than 0.")
  }

  # Create a data.table from the input vectors
  dt <- data.table(ms_id = ms_id, peak_id = peak_id, mf = mf)

  # Create a warning for duplicate entries (same molecular formula for different peaks in a spectrum)
  dups <- dt[, .N, .(ms_id, mf)][N>1]
  if (nrow(dups)>0) {
    warning("There are duplicate assignments for some combinations of ms_id, peak_id, and mf. ",
            "Examples:\n",
            utils::head(dups, n= 5))
  }

  # Count occurrences of each combination of ms_id, peak_id, and mf
  dt[, n_assignments:= .N, by = .(ms_id, peak_id)]

  # # Merge back with original to preserve order and return the counts
  # result <- dt[, .(n_assignments = .N), by = .(ms_id, peak_id, mf)]
  # n_assignments_vector <- result[match(paste(ms_id, peak_id, mf),
  #                                      paste(result$ms_id, result$peak_id, result$mf)), n_assignments]

  return(dt$n_assignments)
}
