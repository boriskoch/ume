#' @title Remove molecular formulas detected in blanks
#' @name remove_blanks
#' @family Formula subsetting
#' @description Remove all molecular formulas that were detected in one or more blank analyses
#' (identified via `blank_file_ids`). Matching is always on `mf`. If a
#' retention-time column is present (or provided using `ret_time_col`), removal
#' is restricted to the corresponding LC segment.
#' @details
#' - Requires a unique integer `file_id` per analysis in `mfd`.
#' - Minimal required columns in `mfd`: `mf`, `file_id`.
#' - Optional column: a retention-time column (e.g. `"ret_time_min"`).
#' - If a retention-time column is used, formulas present in blanks are only
#'   removed for rows whose `mf` **and** retention time match
#' - The input `mfd` is **not** modified by reference; a subset is returned.
#' @inheritParams main_docu
#' @param blank_file_ids Integer vector of `file_id` values that represent blank analyses.
#' @param blank_prevalence Numeric between 0 and 1. Threshold for blank filtering:
#' the proportion of blanks in which a molecular formula must occur before it is
#' excluded from the sample data. For example, `blank_prevalence = 0` (default)
#' removes any formula detected in at least one blank, while `blank_prevalence = 0.5`
#' removes formulas detected in 50% or more of the blanks.
#' @param ret_time_col Character scalar. Name of the retention-time column that
#' contains the beginning of the retention time segment that corresponds to the
#' mass spectrum.
#' If `NULL` (default), the function will auto-detect the first column in
#' `c("ret_time_min","retention_time","rt","RT")` that exists in `mfd`.
#' If none is found, blanks are removed ignoring retention time.
#' @return `data.table`; subset of the original molecular formula table (`mfd`)
#' with blank formulas removed (globally or LC-segment-wise).
#' @section Backward compatibility:
#' The argument `LCMS` is deprecated and no longer used. Retention-time-aware
#' removal is now enabled automatically when a retention-time column is present
#' or explicitly provided via `ret_time_col`.
#' @examples
#' # Presence/absence removal, no retention time:
#' remove_blanks(mfd = mf_data_demo,
#'               remove_blank_list = "Blank",
#'               verbose = TRUE)
#' @import data.table
#' @keywords misc
#' @author Boris P. Koch
#' @export

remove_blanks <- function(mfd,
                          blank_file_ids = NULL,
                          blank_prevalence = 0.5,
                          ret_time_col = NULL,
                          verbose = FALSE,
                          ...) {

  # --- basic check ----------------------------------------------------------
  if (is.null(blank_file_ids) ||
      length(blank_file_ids) == 0L) {
    .msg("No file_id for blank spectra provided. Returning mfd unchanged.")
    return(mfd[])
  }

  .msg("Length of input formula list: %d formulas", nrow(mfd))

  mfd <- check_mfd(mfd)

  # --- soft deprecation for old 'LCMS' argument passed via ... --------------
  dots <- list(...)
  #message("dots: ", paste(names(dots), collapse = ", "))

  blank_occurrence_ratio <- blank_occurrence <- n_total_blanks_rt <- NULL

# Save the original number of formulas in mfd
  n_mf_orig <- mfd[, .N]

# Check if mfd represents LCMS data having a column for retention time.
  # if ("LCMS" %in% names(dots)) {
  #   warning(
  #     "Argument 'LCMS' is deprecated and ignored. ",
  #     "Retention-time-aware removal is automatic if a RT column is present or set via 'ret_time_col'.",
  #     call. = FALSE
  #   )
  # }

# --- validate blanks -------------------------------------------------------
  ids_present <- unique(mfd[["file_id"]])
  valid_blanks   <- unique(blank_file_ids[blank_file_ids %in% ids_present])
  invalid_blanks <- unique(blank_file_ids[!blank_file_ids %in% ids_present])

  if (length(valid_blanks) == 0L) {
    warning(
      "None of the provided blank file_id's are present in 'mfd'. Returning 'mfd' unchanged."
    )
    return(mfd[])
  }
  if (length(invalid_blanks) & verbose) {
    message("Ignored blanks not present in 'mfd': ",
            paste(invalid_blanks, collapse = ", "))
  }

  if (!blank_prevalence %between% c(0, 1) || is.null(blank_prevalence)) {
    warning(
      "The argument 'blank_prevalence' must be a value between 0 and 1. Returning 'mfd' unchanged."
    )
    return(mfd[])
  }

  .msg("Using %i file_id's for blank correction.", length(valid_blanks))

# --- determine RT column (if any) -----------------------------------------
  if (is.null(ret_time_col)) {
    candidates <- c("ret_time_min", "retention_time", "rt", "RT")
    ret_time_col <- candidates[candidates %in% names(mfd)][1]
    if (is.na(ret_time_col))
      ret_time_col <- NULL
  } else {
    if (!ret_time_col %in% names(mfd)) {
      warning("Specified 'ret_time_col' not found in 'mfd'. Proceeding without retention time.")
      ret_time_col <- NULL
    }
  }

  # copy: avoid mutating user input by reference
  out <- data.table::copy(mfd)

  # Table containing blank molecular formulas
  blanks_mf <- unique(out[file_id %in% valid_blanks, .N, mf])

  # --- build blank lookup ----------------------------------------------------
  if (is.null(ret_time_col)) {
    # presence/absence across full spectrum

    if (blank_prevalence > 0) {
      # Find the number of blank file_id's
        blank_n <- out[file_id %in% valid_blanks, length(unique(file_id))]

      # Calculate ratio: relative no. of occurrence of a blank in the entire dataset
      blanks_mf[, blank_occurrence_ratio := N / blank_n]

      # Filter by relative occurrence
      blanks_mf <- blanks_mf[blank_occurrence_ratio >= blank_prevalence]

      out <- out[!mf %in% unique(blanks_mf$mf)]

    } else {
      # fast anti-subset
      out <- out[!mf %in% blanks_mf$mf]
    }

    .msg("Removed %i blank formulas from the entire dataset (no retention time considered).\n Remaining rows: %i",
         n_mf_orig - out[, .N], nrow(out))

    .msg("Number of formulas after blank removal: %d", nrow(out))

    return(out[])
  }

  # --- retention-time-aware removal -----------------------------------------
  # ensure numeric RT
  if (!is.numeric(out[[ret_time_col]])) {
    # try coercion (safe-ish)
    out[, (ret_time_col) := as.numeric(get(ret_time_col))]
    # warning(
    #   "Retention-time column could not be coerced to numeric. Proceeding without retention time."
    # )
    blanks_mf <- unique(out[file_id %in% valid_blanks, mf])
    out <- out[!mf %in% blanks_mf]
    .msg("Removed blanks by mf only (RT not usable). Remaining rows: ",
              nrow(out))
    return(out[])
  }

  # extract unique blank (mf, rt) pairs
  blanks_rt <- unique(out[file_id %in% valid_blanks, .(blank_occurrence = .N), .(mf, ret_time_min = get(ret_time_col))])

  # assess how many blanks were measured for each given retention time
  total_n_blanks_rt <- out[file_id %in% valid_blanks, .N, .(file_id, ret_time_min)][, .(n_total_blanks_rt=.N), ret_time_min]
  blanks_rt <- total_n_blanks_rt[blanks_rt, on = ret_time_col]

  # Calculate the ratio of occurrence versus total number of blank analyses
  blanks_rt[, blank_occurrence_ratio := blank_occurrence / n_total_blanks_rt]

  if (nrow(blanks_rt) == 0L) {
    .msg("No (mf, RT) pairs found in blanks. Returning `mfd` unchanged.")
    return(out[])
  }

  # exact match vs tolerance window
    setDT(blanks_rt)  # ensure DT
    setkey(blanks_rt, mf)

    out <- blanks_rt[, .(mf, ret_time_min, blank_occurrence, blank_occurrence_ratio)][out, on = c("mf", ret_time_col)]

    if(blank_prevalence == 0){
      # Any blank formula in a retention time segment is removed.
      out <- out[is.na(blank_occurrence)]
      .msg("Removed all formulas that occurred in at least one blank at a given retention time.")
    } else {
      # Blank formulas are removed according to prevalence setting.
      out <- out[blank_occurrence_ratio <= blank_prevalence | is.na(blank_occurrence)]
      .msg("Removed %d formulas that occurred in at least %d %% of all analysed blanks at a given retention time.",
           n_mf_orig - out[, .N], blank_prevalence*100)
    }
  .msg("Number of formulas after blank removal: %d", nrow(out))
  return(out[])
}
