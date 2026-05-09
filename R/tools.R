
# Some small tools

# Remove Empty Columns from a data.table

#' @title Remove empty columns
#' @description
#' Removes columns that contain only `NA` values from a `data.table`.
#' Columns listed in `excl_cols` are retained even if they are empty.
#' @inheritParams main_docu
#' @param df A `data.table` from which empty columns should be removed.
#' @param excl_cols
#'   Optional character vector of column names that must be preserved,
#'   even if all values in those columns are missing.
#' @import data.table
#' @return
#' A `data.table` containing all original non-empty columns, plus any
#' columns listed in `excl_cols`, regardless of whether they are empty.
#' Columns that contain only `NA` values and are *not* explicitly preserved
#' are removed from the output.
#' @examples
#' dt <- data.table::data.table(
#'   c = c(2, 2, 2),
#'   x = c(NA, NA, NA),
#'   y = c(NA, NA, NA)
#' )
#' remove_empty_columns(dt, excl_cols = "y")
#' @export

remove_empty_columns <- function(df, excl_cols = NULL, ...) {

  # columns to examine (not excluded)
  cols_to_check <- setdiff(names(df), excl_cols)

  # columns that must be preserved
  cols_preserve <- intersect(names(df), excl_cols)

  # Identify non-empty columns using .SDcols
  nonempty <- vapply(
    df[, .SD, .SDcols = cols_to_check],
    function(x) !all(is.na(x)),
    logical(1)
  )

  cols_nonempty <- cols_to_check[nonempty]

  # Final column order
  final_cols <- c(cols_preserve, cols_nonempty)

  return(df[, final_cols, with = FALSE])
}


# remove_unknown_columns ------------------------

#' Remove columns that only have one specific value
#'
#' This function removes columns that exclusively contain the value defined in 'search_term' (such as " unknown" (default)).
#' @family Clean data output
#' @inheritParams main_docu
#' @param df data.table that contains empty columns
#' @param excl_cols List of column names that should not be removed, even if all values contain search_term
#' @param search_term String that uniquely occurs in one column

#' @import data.table
#' @keywords Remove columns that exclusively contain missing values.

remove_unknown_columns <- function(df, excl_cols = NULL, search_term = " unknown", ...){

  if(!is.null(excl_cols)){colnames <- names(df)[!(names(df) %in% excl_cols)]} else {
    colnames <- names(df)
  }
  colnames
  df <- cbind(df[, ..excl_cols], Filter(f = function(x) !all(x == search_term), x = df[, ..colnames])) # remove all empty columns
  return(df)
}


# remove_id_columns ------------------------

#' Remove columns that contain ID's
#'
#' This functions removes columns ID columns ('_id') and hierarchical search columns ('_lft', '_rgt') from a table.
#' Only exceptions are "sample_id" and "bottle_id that are always kept in the output table.
#' @family Clean data output

#' @inheritParams main_docu
#' @param df data.table that contains ID columns

#' @import data.table
#' @keywords Remove ID columns.

remove_id_columns <- function(df, ...){

  .. <- ..new_cols <- NULL

  del_cols <- c("_id|_lft|_rgt") # pattern for columns to be removed
  cols <- names(df); cols # names of orginal table

  new_cols <- cols[!(cols %like% del_cols)]; new_cols # column names of new table

  keep_cols <- c("sample_id", "bottle_id") # Always keep these columns in final table
  keep_cols <- names(df)[names(df) %in% keep_cols]

  if(length(keep_cols)>0){new_cols <- c(unique(keep_cols, new_cols))} # Always keep the sample_id

  df <- df[, ..new_cols] # remove id columns
  df <- ume::order_columns(df)
  return(df)
}

# order_columns ------------------------

#' @title Order columns
#' @name order_columns
#' @description Take most prominent columns required for data evaluation first - followed by all other columns.

#' @family tools
#' @inheritParams main_docu
#' @param col_order A list of column names that defines the order of columns of mfd. Default is:
#' cols = c("sample_tag", "sample_id", "file", "file_id", "peak_id", "i_magnitude", "norm_int", "m", "m_cal", "ppm", "nm", "mf", "dbe",
#' "c", "h", "n", "o", "p", "s", "hc", "oc", "nc", "sc", "ai", "z", "kmd")
#' If "col_order" is NULL the default order is applied.

#' @keywords misc
#' @import data.table
#' @return A data.table containing isotope data for those isotopes present in mfd.
#' @examples order_columns(mfd = mf_data_demo)
#' @export

order_columns <- function(mfd, col_order = NULL, ...){

isotope_info <- get_isotope_info(mfd)

# This list defines the priority order
  if(is.null(col_order)) {
    cols <- c("sample_tag", "sample_id", "file", "file_id", "peak_id",
              "i_magnitude", "norm_int", "m", "m_cal", "ppm", "nm", "mf", "dbe",
              isotope_info$label, "hc", "oc", "nc", "sc", "ai", "z", "kmd")
  }

# Select only those items of cols that are existing in mfd
  cols_sel <- col_order[col_order %in% names(mfd)]

# Order the columns of mfd
  mfd <- data.table::setcolorder(mfd, cols_sel)

  return(mfd)
}


# add_missing_element_columns ------------------------

#' @title Add Missing Isotope Columns to mfd
#' @name add_missing_element_columns
#' @description
#' This function ensures that missing isotope columns are added to the input data table (`mfd`), which is required for further data evaluation that considers isotope information. If any of the specified isotope columns are not already present in the data, they will be added with a default value of `0`.
#'
#' The function is typically used to standardize the dataset by ensuring that all expected isotopes (e.g., nitrogen-15, carbon-13) are represented, even if they are not initially present in the data. The function works by checking for the existence of each specified isotope column and adding the missing ones.
#'
#' @family tools

#' @inheritParams main_docu
#' @param missing_cols A character vector of isotope column names that should be checked and added if missing. By default, it includes `"15n"`, but additional isotopes can be specified as needed (e.g., `"na"`, `"d"`, `"35cl"`, etc.).
#'
#' @return
#' A `data.table` object with the missing isotope columns added,
#' where missing columns are populated with a default value of `0`.
#' The original `mfd` object is modified in place.
#'
#' @examples
#' # Add missing isotope columns to a demo dataset
#' mfd_with_isotopes <- add_missing_element_columns(mfd = mf_data_demo)
#'
#' # Add a specific isotope column for Nitrogen-15 (if missing)
#' mfd_with_15n <- add_missing_element_columns(mfd = mf_data_demo, missing_cols = c("15n", "na"))
#' @import data.table
#' @export

add_missing_element_columns <- function(mfd, missing_cols = "15n"){
  # Define the required isotope columns:
  # cols <- c("15n"
            #, "na"
            #, "d"
            #, "35cl", "37cl", "79br", "81br"
            #, "54fe", "56fe", "57fe", "59co", "63cu", "65cu", "232th")

  # If isotope columns are not existing yet add column with value "0"
  for(j in missing_cols) {if (j %in% colnames(mfd)==F) mfd[, (j):=0]}

  return(mfd[])
}


# Non-public functions for molecular formula assignment -------------------

#' @title Conditional message output for verbose functions
#' @description
#' Helper function for internal use to print formatted messages
#' when `verbose = TRUE`. It uses `sprintf()` for clean formatting.
#'
#' @param ... Character strings passed to `sprintf()` for formatted output.
#'
#' @details
#' This function standardizes how verbose messages are displayed across
#' package functions. It automatically checks if a variable `verbose`
#' exists in the calling environment and is `TRUE`.
#'
#' Use it inside functions like this:
#'
#' ```r
#' n <- 5
#' verbose <- TRUE
#' .msg("Processing %d samples...", n)
#' ```
#' If `verbose` is not defined or FALSE, no output is shown.
#' @keywords internal

.msg <- function(...) {
  # if (isTRUE(verbose)) {
  #   message(sprintf(...))
  # }
  # invisible()
  # Try to find 'verbose' in the calling function's arguments
  parent_call <- parent.frame()

  verbose <- tryCatch({
    # 1. Check if 'verbose' exists as a variable in the parent frame
    if (exists("verbose", envir = parent_call, inherits = FALSE)) {
      get("verbose", envir = parent_call)
    } else {
      # 2. Check if 'verbose' is a formal argument of the calling function
      f <- sys.function(-1)
      args <- formals(f)
      if ("verbose" %in% names(args)) {
        val <- eval(as.name("verbose"), envir = parent_call)
        if (!is.null(val)) val else FALSE
      } else {
        FALSE
      }
    }
  }, error = function(e) FALSE)

  if (!isTRUE(verbose)) return(invisible())

  args <- list(...)
  fmt <- args[[1]]

  if (is.character(fmt) && grepl("%", fmt, fixed = TRUE) && length(args) > 1) {
    message(do.call(sprintf, args))
  } else {
    message(paste(unlist(args), collapse = ""))
  }

  invisible()
}


# Peaklist helpers -----------------------------------

#' Load a peaklist from file
#'
#' @description
#' Internal helper for `as_peaklist()` that reads a peaklist from a file.
#' Supports common tabular formats, including:
#' - CSV (`.csv`)
#' - TSV (`.tsv`, `.txt`)
#' - RDS (`.rds`)
#'
#' Column names are not altered here; normalization happens later in
#' `as_peaklist()` via `.normalize_column_aliases()`.
#'
#' @family peaklist helpers
#' @param path Character string. Path to the file to be read.
#'
#' @return A `data.table` containing the raw peaklist data.
#'
#' @keywords internal
.load_peaklist_file <- function(path) {
  ext <- tolower(tools::file_ext(path))

  if (ext %in% c("csv")) {
    return(data.table::fread(path))
  }
  if (ext %in% c("txt", "tsv")) {
    return(data.table::fread(path, sep = "\t"))
  }
  if (ext == "rds") {
    out <- readRDS(path)
    return(data.table::as.data.table(out))
  }

  stop(sprintf("Unsupported peaklist file format: '%s'", ext), call. = FALSE)
}


#' Convert numeric m/z vector into minimal peaklist
#'
#' @description
#' Converts a simple numeric vector containing m/z values into a minimal
#' UME peaklist. This is useful when users want to perform direct
#' formula assignment on a single spectrum represented only by m/z values.
#'
#' The generated peaklist contains:
#' - `mz` (copied from input)
#' - `i_magnitude` (set to 1 for all peaks)
#' - `file_id` = 1L
#'
#' A `"col_history"` attribute is added to track that the object was
#' constructed from a numeric vector.
#' @family peaklist helpers
#' @param x Numeric vector of m/z values.
#' @return A minimal peaklist as a `data.table`.
#'
#' @keywords internal
.as_peaklist_from_numeric <- function(x) {
  .msg("[UME] Detected numeric m/z vector -> converting to minimal peaklist.")
  pl <- data.table::data.table(
    mz          = x,
    i_magnitude = 1L,
    file_id     = 1L
  )
  data.table::setattr(pl, "col_history", list(converted_from = "numeric_vector"))
  pl
}


#' Ensure required peaklist columns are present
#'
#' @description
#' Internal helper for `as_peaklist()` that ensures essential structural
#' columns required for UME processing are present. Specifically:
#'
#' - If `file_id` is missing but a character column such as `file` or
#'   `link_rawdata` exists, file_id is generated as a unique integer
#'   per distinct value in that column.
#' - If no such identifier exists, `file_id := 1L` is assigned.
#' - Adds `peak_id` if missing (using `.I`)
#' - Converts `file_id` to integer type
#'
#' @param pl A `data.table` representing a peaklist.
#'
#' @return A `data.table` with guaranteed core columns.
#'
#' @keywords internal
.prepare_peaklist_columns <- function(pl) {

  link_rawdata <- NULL

  # ---- CASE A: file_id exists -------------------------------------------------
  if ("file_id" %in% names(pl)) {
    pl[, file_id := as.integer(file_id)]

  } else {

    # ---- CASE B: file or link_rawdata exists ---------------------------------
    if ("file" %in% names(pl)) {

      .msg("[UME] Generating file_id from column 'file'.")
      pl[, file_id := .GRP, by = file]

    } else if ("link_rawdata" %in% names(pl)) {

      .msg("[UME] Generating file_id from column 'link_rawdata'.")
      pl[, file_id := .GRP, by = link_rawdata]

    } else {

      # ---- CASE C: fallback if no identifier exists ---------------------------
      .msg("[UME] No file identifier found. Assigning file_id = 1L.")
      pl[, file_id := 1L]
    }
  }

  # ---- Ensure peak_id exists -------------------------------------------------
  if (!"peak_id" %in% names(pl)) {
    .msg("[UME] Added peak_id column.")
    pl[, peak_id := .I]
  }

  return(pl)
}



#' Apply basic filters to peaklist
#'
#' @description
#' Removes entries that are clearly invalid for formula assignment:
#'
#' - `mz` is missing (`NA`) or negative
#' - `i_magnitude` is missing (`NA`)
#'
#' These checks ensure that downstream validation and formula assignment
#' receive only physically meaningful peaks.
#'
#' @family peaklist helpers
#' @param pl A `data.table` representing a peaklist.
#'
#' @return A filtered `data.table` with invalid rows removed.
#'
#' @keywords internal
.filter_peaklist_basic <- function(pl) {
  pl <- pl[!is.na(mz) & mz >= 0]
  pl <- pl[!is.na(i_magnitude)]
  pl
}


#' Normalize column names using an alias map (internal)
#'
#' @description
#' Internal helper used to translate user-supplied column names
#' (e.g. "m/z", "mass", "intensity") into canonical UME names
#' such as "mz" and "i_magnitude".
#'
#' It renames columns in-place (by reference) using data.table::setnames(),
#' and returns a mapping from canonical names to original names, but
#' **does not set any attributes** on `dt`. This avoids data.table
#' shallow-copy warnings during later `:=` operations.
#'
#' @param dt A data.table (modified by reference).
#' @param alias_map Named list: canonical names -> vectors of aliases.
#'
#' @return A list mapping canonical names to original column names.
#'         Example: list(mz = "m/z", i_magnitude = "intensity")
#'
#' @keywords internal
#' @noRd
.normalize_column_aliases <- function(dt, alias_map) {

  # Freeze original names once
  orig_names <- names(dt)

  # Will store: canonical -> original name
  original <- list()

  .msg("[UME] Starting column alias normalization: %s",
       paste(orig_names, collapse = ", "))

  for (canon in names(alias_map)) {

    aliases <- alias_map[[canon]]

    # match against original names (case-insensitive)
    hit <- which(tolower(orig_names) %in% tolower(aliases))

    if (length(hit) == 0L) {
      .msg("[UME] No alias found for '%s'.", canon)
      next
    }

    if (length(hit) > 1L) {
      stop(
        "[UME] Ambiguous alias mapping for canonical name '", canon,
        "'. Matching columns: ", paste(orig_names[hit], collapse = ", "),
        call. = FALSE
      )
    }

    old_name <- orig_names[hit]

    # Store mapping (canonical -> original)
    original[[canon]] <- old_name

    if (!identical(old_name, canon)) {
      .msg("[UME] Normalizing column name: '%s' -> '%s'", old_name, canon)
      data.table::setnames(dt, old = old_name, new = canon)
    } else {
      .msg("[UME] Column '%s' is already canonical.", canon)
    }
  }

  .msg("[UME] Column names after normalization: %s",
       paste(names(dt), collapse = ", "))

  original
}


#' Check whether an object is a UME peaklist
#'
#' @param x Any object
#' @return TRUE/FALSE
#' @export

is_ume_peaklist <- function(x) {
  !is.null(attr(x, "ume_type")) &&
    identical(attr(x, "ume_type"), "peaklist")
}



# Assign formulas helpers -------------------------

#' Chunked foverlaps (internal)
#' @keywords internal
#' @noRd
.foverlaps_chunked_lapply <- function(lib, pl, cols = c("m_min","m_max"),
                                     chunk_n = 5e5, progress = TRUE, show_ram = FALSE,
                                     prefix = "Overlaps") {
  stopifnot(all(cols %in% names(lib)), all(cols %in% names(pl)))
  if (!data.table::is.data.table(lib)) lib <- data.table::as.data.table(lib)
  if (!data.table::is.data.table(pl))  pl  <- data.table::as.data.table(pl)

  data.table::setkeyv(lib, cols)
  data.table::setkeyv(pl,  cols)

  n <- nrow(pl)
  if (n == 0L) {
    return(data.table::foverlaps(lib, pl, by.x = cols, by.y = cols, nomatch = 0L))
  }

  idx_starts <- seq.int(1L, n, by = max(1L, as.integer(chunk_n)))
  t0 <- proc.time()[["elapsed"]]
  if (isTRUE(progress)) {
    .msg(
      "%s: chunked overlap of %s rows (chunk ~%s)",
      prefix,
      format(n, big.mark = ","),
      format(chunk_n, big.mark = ",")
    )
  }

  out <- lapply(seq_along(idx_starts), function(i) {
    s <- idx_starts[i]
    e <- min(n, s + chunk_n - 1L)
    res <- data.table::foverlaps(lib, pl[s:e], by.x = cols, by.y = cols, nomatch = 0L)
    if (isTRUE(progress)) .print_progress(done = e, total = n, t0 = t0, prefix = prefix, show_ram = show_ram)
    res
  })

  data.table::rbindlist(out, use.names = TRUE)
}


# Format seconds
.format_duration <- function(sec) {
  sec <- as.numeric(sec)
  if (!is.finite(sec) || sec < 0) sec <- 0
  h <- floor(sec / 3600)
  rem <- sec - h * 3600
  m <- floor(rem / 60)
  s <- floor(rem - m * 60)
  if (h > 0) sprintf("%d:%02d:%02d", h, m, s) else sprintf("%02d:%02d", m, s)
}

.r_mem_mb <- function() {
  # 1) Portable: base::gc() sum "used (Mb)"
  mb <- tryCatch({
    g <- base::gc()
    # In recent R-Versions memory consumption is in column 2 (Mb)
    as.numeric(sum(g[, 2], na.rm = TRUE))
  }, error = function(e) NA_real_)

  # 2) Windows: memory.size() (if available and plausible)
  if (!is.finite(mb)) {
    ms <- try(suppressWarnings(memory.size()), silent = TRUE)
    if (!inherits(ms, "try-error") & is.finite(ms)) mb <- as.numeric(ms)
  }

  # 3) Linux: /proc/self/status -> VmRSS (kB)
  if (!is.finite(mb) & file.exists("/proc/self/status")) {
    x <- try(readLines("/proc/self/status", warn = FALSE), silent = TRUE)
    if (!inherits(x, "try-error")) {
      line <- x[grepl("^VmRSS:", x)]
      if (length(line)) {
        kb <- suppressWarnings(as.numeric(gsub(".*?([0-9]+)\\s*kB.*", "\\1", line[1])))
        if (is.finite(kb)) mb <- kb / 1024
      }
    }
  }

  if (is.finite(mb)) mb else NA_real_
}



.print_progress <- function(done,
                            total,
                            t0,
                            prefix   = "Overlaps",
                            show_ram = TRUE,
                            verbose  = TRUE) {

  # .msg() will see 'verbose' in this frame
  elapsed <- proc.time()[["elapsed"]] - t0
  frac    <- if (total > 0) done / total else 0
  eta     <- if (frac > 0) elapsed * (1 - frac) / frac else NA_real_

  msg <- sprintf(
    "%s %s/%s (%.1f%%)  elapsed %s  ETA %s",
    prefix,
    format(done,  big.mark = ","),
    format(total, big.mark = ","),
    100 * frac,
    .format_duration(elapsed),
    if (is.na(eta)) "..." else .format_duration(eta)
  )

  if (isTRUE(show_ram)) {
    ram <- .r_mem_mb()
    if (is.finite(ram)) {
      msg <- sprintf("%s  | R used ~ %.0f MB", msg, ram)
    }
  }

  .msg("%s", msg)  # uses your .msg(), respects 'verbose' in this frame

  invisible()
}


#' Lookup Pretty Labels for Column Names (Internal)
#'
#' @title Internal helper: pretty label lookup
#' @name .f_label
#' @description
#' Internal utility function to map a variable or column name to a more
#' descriptive, human-readable label based on a lookup table.
#'
#' The lookup table must contain two columns:
#'   * `name_pattern`    – Regular expressions to match column names
#'   * `name_substitute` – Human-readable label returned when pattern matches
#'
#' The function returns the first matching substitute label.
#' If no pattern matches, the input `colname` is returned unchanged.
#'
#' This function is not exported and is intended for use inside the `ume`
#' package (e.g., for automatic axis labeling in plotting functions).
#'
#' @param colname Character string. Column name to be matched.
#' @param lookup A `data.table` or `data.frame` with columns
#'        `name_pattern` and `name_substitute`.
#'
#' @return A character string: either the substitute label or the original
#'         `colname` if no pattern matches.
#'
#' @keywords internal

.f_label <- function(colname, lookup = ume::nice_labels_dt) {

  if (is.null(lookup) ||
      !all(c("name_pattern", "name_substitute") %in% names(lookup))) {
    warning("Lookup table is missing or malformed. Returning colname unchanged.")
    return(colname)
  }

  if (is.na(colname)) return(NA_character_)

  # Vectorised regex matching: pattern is a vector, colname is length 1
  matches <- vapply(
    lookup$name_pattern,
    function(p) grepl(p, colname, perl = TRUE),
    logical(1)
  )

  if (any(matches)) {
    return(lookup$name_substitute[which(matches)[1]])
  }

  return(colname)
}

#' @keywords internal
is_ume_library <- function(x) inherits(x, "ume_library")

#' @keywords internal
is_official_library <- function(x) isTRUE(attr(x, "official"))

#' @keywords internal
ume_library_name <- function(x) attr(x, "ume_library_name")


# .extract_library_version-----------
#' Extract UME library version from formula library object
#'
#' @param lib A formula library data.table or list.
#'
#' @return Numeric library version.
#' @keywords internal
.extract_library_version <- function(lib) {
  v <- lib$vkey[1]
  (v - 1) / 1e12
}

#' Internal helper to check required columns in molecular formula data
#'
#' @param mfd A data.table or data.frame.
#' @param required Character vector of required column names.
#' @param fun_name Optional name of the calling function for clearer error messages.
#'
#' @return Invisibly returns TRUE if all columns exist; otherwise stops.
#' @keywords internal

.uplots_require_columns <- function(mfd, required, fun_name = "") {

  # 1) flatten input safely
  if (is.list(required)) {
    required <- unlist(required, use.names = FALSE)
  }

  # 2) keep only unique, non-empty character names
  required <- unique(required)
  required <- required[is.character(required) & nzchar(required)]

  # 3) check presence
  missing <- setdiff(required, names(mfd))

  if (length(missing) > 0) {
    stop(
      sprintf(
        "%s requires the following columns, which are missing: %s",
        fun_name,
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}



# This is used for hover text
.aes_name <- function(x) {
  if (is.null(x)) return(NULL)

  # quosure or formula: extract RHS
  if (inherits(x, "formula")) {
    return(deparse(x[[2]]))
  }

  # symbol
  if (is.symbol(x)) {
    return(as.character(x))
  }

  # fallback (should rarely happen)
  as.character(x)
}


.get_aes_name <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.symbol(x)) return(as.character(x))
  if (is.call(x))   return(as.character(x[[2]]))
  NULL
}
