#' @title Check format of peaklist
#' @name as_peaklist
#' @family check ume objects
#' @description
#' Flexible entry point for UME. Accepts:
#'
#' - data.frame / data.table peaklists
#' - numeric m/z vectors
#' - file paths (csv, txt, tsv, rds)
#'
#' Normalizes column names, adds missing structural columns (file_id, peak_id),
#' removes invalid rows, validates schema, and assigns the UME peaklist class.
#' Creates a standardized `data.table` ready for formula assignment.
#'
#' @inheritParams main_docu
#'
#' @param pl Input object representing a peaklist. Can be:
#'   - data.frame or data.table
#'   - file path to a supported tabular format
#'   - numeric vector of m/z values
#' @param track_original_names Logical (default: TRUE). If TRUE,
#'   `as_peaklist()` stores a `"original_colnames"` attribute mapping
#'   canonical UME names (e.g. `"mz"`) to the user’s original column names
#'   (e.g. `"m/z"`). Internal functions that perform many `:=` operations
#'   (e.g. `assign_formulas()`) may set this to FALSE to avoid attribute-
#'   related shallow-copy warnings.
#'
#' @param ... Reserved for future extensions.
#'
#' @return A validated and normalized peaklist as a `data.table`
#'   with class `"ume_peaklist"`.
#' @export

as_peaklist <- function(pl,
                        verbose = FALSE,
                        track_original_names = TRUE,
                        ...) {

  .msg("COLUMN NAMES BEFORE NORMALIZATION: %s",
       paste(names(pl), collapse = ", "))

  from_numeric <- FALSE

  # 1) File input -----------------------------------------------------------
  if (is.character(pl) && length(pl) == 1 && file.exists(pl)) {
    .msg("[UME] Loading peaklist from file: %s", pl)
    pl <- .load_peaklist_file(pl)
  }

  # 2) Numeric vector input -------------------------------------------------
  if (is.numeric(pl) && is.vector(pl)) {
    .msg("[UME] Detected numeric m/z vector -> converting to minimal peaklist.")
    pl <- data.table::data.table(
      mz          = pl,
      i_magnitude = 1L,
      file_id     = 1L
    )
    from_numeric <- TRUE
  }

  # 3) Ensure data.table ----------------------------------------------------
  if (!data.table::is.data.table(pl)) {
    pl <- data.table::as.data.table(pl)
    .msg("[UME] Converted peaklist to data.table.")
  }

  # ---- CRITICAL CLEANUP TO PREVENT SHALLOW-COPY WARNINGS ----
  # setattr(pl, "names", names(pl))            # ensure real names copy
  # setattr(pl, "row.names", NULL)
  # setattr(pl, "sorted", NULL)
  # setattr(pl, ".internal.selfref", NULL)


  # 4) Normalize column aliases --------------------------------------------
  alias_map_peaklist <- list(
    mz          = c("mz", "m/z", "m.z", "mass", "m.z."),
    i_magnitude = c("i_magnitude", "intensity", "I", "signal", "height"),
    file_id     = c("file_id", "spectrum_id", "spec", "scan", "scan_id")
  )

  if (isTRUE(track_original_names)) {
    original_map <- .normalize_column_aliases(pl, alias_map_peaklist)
  } else {
    # Rename columns, but do not track original names
    .normalize_column_aliases(pl, alias_map_peaklist)
    original_map <- NULL
  }

  # 5) Ensure structural columns (file_id, peak_id) -------------------------
  pl <- .prepare_peaklist_columns(pl)

  # 6) Basic input filters --------------------------------------------------
  pl <- .filter_peaklist_basic(pl)

  # 7) Schema / structural validation --------------------------------------
  pl <- validate_peaklist(pl)

  # 8) Set key --------------------------------------------------------------
  data.table::setkeyv(pl, c("file_id", "mz"))

  # 9) Assign class ---------------------------------------------------------
  #class(pl) <- unique(c("ume_peaklist", class(pl)))
  setattr(pl, "class", c("data.table", "data.frame"))
  setattr(pl, "ume_type", "peaklist")


  # 10) Attach attributes *only now* (no more := after this) ---------------
  if (isTRUE(track_original_names) && length(original_map)) {
    data.table::setattr(pl, "original_colnames", original_map)
  }

  if (isTRUE(track_original_names) && from_numeric) {
    data.table::setattr(pl, "col_history", list(converted_from = "numeric_vector"))
  }

  .msg("[UME] Peaklist validation successful.")

  pl
}
