# Check format of each type of data table ####

# check_formula_library -----------------------

#' @title Check format of formula library
#' @name check_formula_library
#' @family Formula assignment
#' @family check ume objects
#' @description Verify the correct usage of UME column names, existence of a unique peak identifier (peak_id),
#' and a unique file/analysis name (file_id).
#' Remove rows having missing values for either m/z (mz) or peak magnitude (i_magnitude).
#' @inheritParams main_docu
#' @import data.table
#' @return data.table
#' @references
#' Leefmann, T., Frickenhaus, S., Koch, B.P., 2019. UltraMassExplorer: a browser-based application
#' for the evaluation of high-resolution mass spectrometric data. Rapid Communications in Mass Spectrometry 33, 193-202.
#' @author Boris P. Koch
#' @keywords internal

check_formula_library <- function(formula_library, ...) {

# Detect official UME libraries (lib_02, lib_05)
  official_libs <- c("lib_02.rds", "lib_05.rds")
  lib_name <- attr(formula_library, "ume_library_name")

  if (!is.null(lib_name) && lib_name %in% official_libs) {
    message("Skipping format checks for official UME library: ", lib_name)
    return(invisible(formula_library))
  }

  if (inherits(formula_library, "ume_library") &&
      isTRUE(attr(formula_library, "official"))) {
    message("Skipping checks for official UME library.")
    return(invisible(formula_library))
  }

# From here on: only custom/non-official libraries get checked
  # Ensure it's a data.table
  if (!inherits(formula_library, "data.table")) {
    stop("The 'formula_library' must be a data.table.")
  }

  # Required isotope columns
  element_cols <- c("12C", "1H", "16O")

  # Fix isotope names if necessary
  if (!all(element_cols %in% names(formula_library))) {
    iso_names <- get_isotope_info(formula_library)
    setnames(formula_library,
             iso_names$orig_name,
             iso_names$label,
             skip_absent = TRUE)
  }

  # Check schema
  check_table_schema(formula_library, .ume_schema_library, "formula library")

  # Required columns
  required_cols <- c(
    "mf" = "character",
    "12C" = "numeric",
    "1H" = "numeric",
    "16O" = "numeric",
    "mass" = "numeric"
  )

  # Missing columns
  missing_cols <- setdiff(names(required_cols), names(formula_library))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Column type checks
  invalid_types <- sapply(names(required_cols), function(col) {
    required_type <- required_cols[col]
    col_type <- class(formula_library[[col]])[1]
    if (required_type == "numeric") {
      !(col_type %in% c("numeric", "integer"))
    } else {
      !inherits(formula_library[[col]], required_type)
    }
  })
  if (any(invalid_types)) {
    stop("Incorrect types for columns: ",
         paste(names(invalid_types)[invalid_types], collapse = ", "))
  }

  # Add vkey if missing
  if (!("vkey" %in% colnames(formula_library))) {
    formula_library[, vkey := .I]
  }

  # Missing mass values
  if (nrow(formula_library[is.na(mass)]) > 0) {
    warning("Rows with missing 'mass' were removed.")
    formula_library <- formula_library[!is.na(mass)]
  }

  # Duplicate vkeys
  if (anyDuplicated(formula_library$vkey)) {
    stop("Duplicate entries found in 'vkey'.")
  }

  # Validate element counts
  invalid_elements <- formula_library[, lapply(.SD, function(x) any(x < 0)),
                                      .SDcols = element_cols]
  if (any(unlist(invalid_elements))) {
    stop("Negative values in element count columns.")
  }

  # Positive masses only
  if (any(formula_library$mass <= 0)) {
    stop("Non-positive masses detected.")
  }

  # Basic MF validation
  if (any(!grepl("^([A-Z][a-z]?\\d*)+$", formula_library$mf))) {
    warning("Some 'mf' entries contain unexpected characters.")
  }

  # vkey finite and non-negative
  if (!all(is.finite(formula_library$vkey))) {
    stop("Non-finite values in 'vkey'.")
  }
  if (any(formula_library$vkey < 0)) {
    stop("Negative values in 'vkey'.")
  }

  return(invisible(formula_library))
}


# .load_peaklist_file -----------------------

#' Load a peaklist file into a data.table (internal)
#' @name .load_peaklist_file
#' @description
#' Internal helper used by [as_peaklist()] to load peaklist data from
#' disk. Supports the most common text-based formats used for exporting
#' mass spectrometry peaklists, including:
#'
#' - **CSV** (`.csv`)
#' - **TSV** (`.tsv`)
#' - **TXT** (`.txt`)
#' - **Generic delimited files** (auto-detected)
#' - **RDS** (`.rds`)
#'
#' The function returns a `data.table` object with column names exactly as
#' found in the file. Column name normalization and schema validation are
#' handled later by [as_peaklist()] via internal helpers such as
#' `.normalize_column_aliases()` and `check_table_schema()`.
#'
#' @details
#' For text-based files, the function uses `data.table::fread()` which is
#' robust, fast, and automatically detects delimiters (`,`, `;`, `\t`,
#' whitespace).
#'
#' RDS files are loaded using `readRDS()` and are expected to contain a
#' `data.table` or `data.frame`. Any other type results in an error.
#'
#' Unsupported file formats produce a clear and instructive error message.
#'
#' @param path Character string with a valid file path.
#'
#' @return A `data.table` containing the raw peaklist data.
#'
#' @keywords internal
#' @noRd

.load_peaklist_file <- function(path) {

  if (!file.exists(path)) {
    stop("[UME] File does not exist: ", path)
  }

  ext <- tolower(tools::file_ext(path))

  # ----------- RDS files -----------------------------------------------------
  if (ext == "rds") {
    obj <- readRDS(path)

    if (is.data.frame(obj) || data.table::is.data.table(obj)) {
      return(data.table::as.data.table(obj))
    }

    stop(
      "[UME] RDS file does not contain a data.frame/data.table: ", path,
      "\n  Found object of class: ", paste(class(obj), collapse = ", ")
    )
  }

  # ----------- Delimited text files (csv, tsv, txt, etc.) --------------------
  if (ext %in% c("csv", "tsv", "txt", "dat", "peaklist")) {
    dt <- tryCatch(
      data.table::fread(path),
      error = function(e) {
        stop("[UME] Could not read peaklist file '", path, "' via fread():\n  ", e$message)
      }
    )

    if (nrow(dt) == 0L) {
      stop("[UME] Peaklist file '", path, "' was read successfully but contains zero rows.")
    }

    return(dt)
  }

  # ----------- Unsupported file types ----------------------------------------
  stop(
    "[UME] Unsupported peaklist file type: *.", ext, "\n",
    "Supported extensions: csv, tsv, txt, dat, peaklist, rds"
  )
}


# .recover_original_colnames ------------------

#' Recover original column names for export (internal)
#' @name .recover_original_colnames
#' @description
#' Internal helper that restores the original column names of a table
#' using the `"original_colnames"` attribute created by
#' `.normalize_column_aliases()`.
#'
#' This ensures that exported peaklists or MFD tables use the same column
#' names that were present in the user's input, even if UME internally
#' operated with canonical (standardized) names.
#'
#' @details
#' The `"original_colnames"` attribute is a named character vector where
#' **names = canonical column names**
#' **values = user-supplied original names**
#'
#' Example:
#' ```
#' attr(dt, "original_colnames")
#' # $mz = "m/z"
#' # $i_magnitude = "intensity"
#' ```
#'
#' If a canonical column name does not appear in the attribute, its
#' current name is kept (UMEs internal name) without modification.
#'
#' If a conflict occurs (e.g. two columns map to the same original name),
#' an informative error is thrown.
#'
#' @param dt A `data.table` with an optional `"original_colnames"` attribute.
#' @param drop_attr Logical (default: TRUE).
#'   If TRUE, removes the `"original_colnames"` attribute after restoring.
#'
#' @return The modified `data.table` (invisible),
#' with restored column names.
#'
#' @keywords internal
#' @noRd
.recover_original_colnames <- function(dt, drop_attr = TRUE) {

  if (!data.table::is.data.table(dt)) {
    stop(".recover_original_colnames(): Input must be a data.table.")
  }

  original <- attr(dt, "original_colnames")

  # Nothing to do
  if (is.null(original) || length(original) == 0) {
    return(invisible(dt))
  }

  current_names <- names(dt)
  restore_map <- list()

  # Build mapping: canonical -> original
  for (canon in names(original)) {
    if (canon %in% current_names) {
      restore_map[[canon]] <- original[[canon]]
    }
  }

  # Detect conflicts (two canonical names mapping to the same original name)
  if (length(restore_map) > 0) {
    duplicates <- duplicated(unlist(restore_map))
    if (any(duplicates)) {
      problematic <- unique(unlist(restore_map)[duplicates])
      stop(
        "[UME] Conflicting original column names in export: ",
        paste(problematic, collapse = ", "),
        "\n-> Two canonical names attempt to restore to the same output name."
      )
    }
  }

  # Apply renaming
  for (canon in names(restore_map)) {
    original_name <- restore_map[[canon]]
    data.table::setnames(dt, old = canon, new = original_name)
  }

  # Clean up attribute after export
  if (drop_attr) {
    data.table::setattr(dt, "original_colnames", NULL)
  }

  invisible(dt)
}

# check_mfd -----------------------------------

#' @title Check format of molecular formula data
#' @name check_mfd
#' @family check ume objects
#' @description Verify the correct usage of UME column names, existence of a unique peak identifier (peak_id),
#' and a unique file/analysis name (file_id).
#' Remove rows having missing values for either m/z (mz) or peak magnitude (i_magnitude).
# @inheritParams main_docu
#' @import data.table
#' @return
#' A `data.table` containing the validated and standardized molecular formula
#' data. The function checks column names, ensures the presence of essential
#' variables (`file_id`, `mz`, `m`, `ppm`), renames isotope columns when needed,
#' and adds missing columns if necessary. The returned `data.table` is the input
#' object `mfd`, potentially modified in place.
#' @keywords internal

check_mfd <- function(mfd, ...){

  check_table_schema(mfd, .ume_schema_formula_table, "formula table")

# Mandatory isotope columns
  element_cols <- c("12C", "1H", "16O")

# Check names of isotope columns
  if(!all(element_cols %in%  names(mfd))) {
    iso_names <- get_isotope_info(mfd)
    setnames(mfd,
             iso_names$orig_name,
             iso_names$label,
             skip_absent = T)
  }

# File_id existing?
  if (!"file_id" %in% names(mfd)) mfd[, file_id:=1]
  if (!"ppm" %in% names(mfd)) stop("No mass accuracy values in mfd.")
  if (!"mz" %in% names(mfd)) stop("No mass to charge (mz) values in mfd.")
  if (!"m" %in% names(mfd)) stop("No mass values (m) in mfd.")

  return(mfd)
}


# check_table_schema -----------------------------------

#' @title Check data.table structure
#' @description Internal helper to verify if a table matches a defined ume schema.
#' @param dt A data.table to check.
#' @param schema A schema list object as defined in `.ume_schema_*`.
#' @param name Optional: name of the table (for clearer error messages)
#' @return Logical TRUE/FALSE invisibly.
#' @keywords internal

check_table_schema <- function(dt, schema, name = "table") {
  if (!data.table::is.data.table(dt)) {
    stop(sprintf("%s must be a data.table", name), call. = FALSE)
  }

  # Check for missing columns
  missing_cols <- setdiff(schema$cols, names(dt))
  if (length(missing_cols)) {
    stop(sprintf("%s is missing required columns: %s",
                 name, paste(missing_cols, collapse = ", ")), call. = FALSE)
  }

  # Flexible type checking: allow multiple possible types per column
  types_ok <- mapply(function(col, allowed_types) {
    # Ensure allowed_types is always a character vector
    allowed_types <- as.character(allowed_types)
    actual_type <- class(dt[[col]])[1]
    actual_type %in% allowed_types
  }, schema$cols, schema$types)

  if (!all(types_ok)) {
    wrong <- schema$cols[!types_ok]
    expected <- vapply(schema$types[!types_ok],
                       function(x) paste(x, collapse = " or "), character(1))
    actual <- vapply(schema$cols[!types_ok],
                     function(col) class(dt[[col]])[1], character(1))

    msg <- paste0(
      sprintf("%s columns have wrong type:\n", name),
      paste0("  - ", wrong, ": expected ", expected,
             ", got ", actual, collapse = "\n")
    )
    stop(msg, call. = FALSE)
  }

  invisible(TRUE)
}


# revert_column_names -----------------------------

#' @title Revert data.table column names
#' @description Restore the original column names recorded in `col_history`.
#' @param dt data.table previously normalized with `normalize_columns`
#' @return data.table with original column names restored
#' @keywords internal
revert_column_names <- function(dt) {
  col_history <- attr(dt, "col_history")
  if (is.null(col_history)) return(dt)
  for (std_name in names(col_history)) {
    orig <- col_history[[std_name]]
    if (!is.na(orig) && std_name %in% names(dt)) {
      data.table::setnames(dt, std_name, orig)
    }
  }
  dt
}
