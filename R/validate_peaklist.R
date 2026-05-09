#' Validate UME peaklist structure
#'
#' @description
#' Internal structural validator for UME peaklists.
#' Ensures that a peaklist has the correct columns, types,
#' and unique identifiers required for downstream processing
#' such as formula assignment.
#'
#' Unlike `as_peaklist()`, this function does not modify the input
#' except for returning it unchanged if validation succeeds.
#' Instead, it raises informative errors that indicate what
#' structural issue was found.
#'
#' This validator is called automatically inside `as_peaklist()`
#' and should not be used directly by end-users.
#'
#' @param x A `data.table` representing a peaklist.
#'
#' @return The input `data.table` (invisibly) if validation passes.
#'
#' @details
#' A valid UME peaklist must satisfy the following:
#'
#' ## Required columns
#' The following columns must exist:
#' - `file_id` (integer)
#' - `file`    (character; optional for minimal peaklists)
#' - `peak_id` (integer)
#' - `mz` (numeric, >= 0)
#' - `i_magnitude` (numeric)
#' - `s_n` (numeric; optional)
#' - `res` (numeric; optional)
#'
#' Missing optional columns are allowed if they are not explicitly
#' required for downstream operations.
#'
#' ## Type requirements
#' - `file_id` and `peak_id` must be integer-like
#' - `mz`, `i_magnitude`, `s_n`, `res` must be numeric
#'
#' ## Uniqueness
#' The pair `(file_id, peak_id)` must be unique.
#'
#' @keywords internal

validate_peaklist <- function(x) {

  # --- basic object type ---------------------------------------------------
  if (!data.table::is.data.table(x)) {
    stop("Peaklist must be a data.table.", call. = FALSE)
  }

  # --- required columns ----------------------------------------------------
  required <- c("file_id", "peak_id", "mz", "i_magnitude")
  missing <- setdiff(required, names(x))

  if (length(missing) > 0) {
    stop(
      sprintf("Peaklist is missing required columns: %s",
              paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }

  # --- type checks ---------------------------------------------------------
  if (!is.numeric(x$mz)) {
    stop("Column 'mz' must be numeric.", call. = FALSE)
  }
  if (!is.numeric(x$i_magnitude)) {
    stop("Column 'i_magnitude' must be numeric.", call. = FALSE)
  }

  if (!(is.integer(x$file_id) || is.numeric(x$file_id))) {
    stop("Column 'file_id' must be integer or numeric.", call. = FALSE)
  }
  if (!(is.integer(x$peak_id) || is.numeric(x$peak_id))) {
    stop("Column 'peak_id' must be integer or numeric.", call. = FALSE)
  }

  # --- uniqueness ----------------------------------------------------------
  if (any(duplicated(x[, .(file_id, peak_id)]))) {
    stop("Peaklist contains non-unique (file_id, peak_id) pairs.",
         call. = FALSE)
  }

  # --- mz domain check -----------------------------------------------------
  if (any(x$mz < 0, na.rm = TRUE)) {
    stop("Column 'mz' contains negative values, which is invalid.", call. = FALSE)
  }

  # --- return validated input ---------------------------------------------
  return(x)
}
