#' Check molecular formulas for neutral formula validity
#'
#' @title Check neutral molecular formulas
#'
#' @description
#' Checks whether character strings are valid neutral molecular formulas that
#' can be parsed by `convert_molecular_formula_to_data_table()`.
#'
#' The function is intended as a lightweight pre-check before converting
#' molecular formulas into element-count tables. It identifies common non-formula
#' entries such as InChIKeys, charged formulas, empty values, unsupported
#' isotope notation, and formulas containing unknown element or isotope labels.
#'
#' @param mf A character vector of molecular formulas.
#' @param masses A `data.table` containing valid element and isotope
#'   definitions. By default, `ume::masses` is used. The table must contain at
#'   least the columns `symbol` and `label`.
#'
#' @return
#' A `data.table` with one row per input entry and the following columns:
#' \describe{
#'   \item{mf}{Original input string.}
#'   \item{is_empty}{Logical; `TRUE` if the input is `NA` or empty.}
#'   \item{is_inchikey}{Logical; `TRUE` if the input resembles an InChIKey.}
#'   \item{has_charge}{Logical; `TRUE` if the formula ends with charge notation
#'   such as `+`, `-`, `+2`, `-3`, `2+`, or `3-`.}
#'   \item{is_parseable}{Logical; `TRUE` if the string can be fully tokenized
#'   using valid element and isotope labels from `masses`.}
#'   \item{is_neutral_mf}{Logical; `TRUE` if the input is non-empty, does not
#'   resemble an InChIKey, has no terminal charge notation, and can be fully
#'   parsed as a molecular formula.}
#'   \item{issue}{Character label describing the detected issue. Valid neutral
#'   formulas are labelled `"valid_neutral_mf"`.}
#' }
#'
#' @details
#' This function validates syntax only. It does not check chemical plausibility,
#' valence rules, isotope natural abundance, charge balance, or whether the
#' molecular formula corresponds to a real compound.
#'
#' The parser uses the valid element symbols and isotope labels provided in
#' `masses`. This avoids hard-coding element symbols and ensures that the
#' validation is consistent with `convert_molecular_formula_to_data_table()`.
#'
#' Supported isotope notation follows the convention used in `ume`, for example:
#' \itemize{
#'   \item `[13C]` for one carbon-13 atom
#'   \item `[13C2]` for two carbon-13 atoms
#'   \item `[18O2]` for two oxygen-18 atoms
#' }
#'
#' The alternative notation `[13C]2` is currently classified as unsupported
#' because the isotope count is placed outside the brackets.
#'
#' Charged formulas such as `"C10H13N2+"`, `"C11H18N2+2"`, or
#' `"C18H35CaO2Zn+3"` are classified as charged and therefore not neutral.
#'
#' InChIKeys such as `"IOVCWXUNBOPUCH-UHFFFAOYSA-M"` are detected separately
#' and classified as non-formula identifiers.
#'
#' @examples
#' mf <- c(
#'   "C6H6",
#'   "C6[13C2]HF15O2",
#'   "C6[13C]2HF15O2",
#'   "C4H5FeO4+",
#'   "C11H18N2+2",
#'   "IOVCWXUNBOPUCH-UHFFFAOYSA-M",
#'   NA_character_
#' )
#'
#' check_neutral_mf(mf)
#'
#' valid_mf <- check_neutral_mf(mf)[is_neutral_mf == TRUE, mf]
#'
#' @family molecular formula functions
#' @keywords chemistry molecular-formula
#' @export

check_neutral_mf <- function(mf, masses = ume::masses) {

  is_empty <- is_neutral_mf <- is_inchikey <- has_charge <- NULL
  is_parseable <- issue <- is_neutral_mf <- NULL

  if (is.factor(mf)) mf <- as.character(mf)
  if (!is.character(mf)) {
    stop("'mf' must be a character vector.", call. = FALSE)
  }

  if (!data.table::is.data.table(masses)) {
    masses <- data.table::as.data.table(masses)
  }

  needed_cols <- c("symbol", "label")
  missing_cols <- setdiff(needed_cols, names(masses))
  if (length(missing_cols) > 0L) {
    stop(
      "'masses' must contain the following columns: ",
      paste(needed_cols, collapse = ", "),
      call. = FALSE
    )
  }

  esc_regex <- function(x) {
    gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
  }

  valid_symbols <- unique(masses$symbol)
  valid_symbols <- valid_symbols[!is.na(valid_symbols) & nzchar(valid_symbols)]
  valid_symbols <- valid_symbols[order(-nchar(valid_symbols), valid_symbols)]

  valid_labels <- unique(masses$label)
  valid_labels <- valid_labels[!is.na(valid_labels) & nzchar(valid_labels)]
  valid_labels <- valid_labels[order(-nchar(valid_labels), valid_labels)]

  sym_alt <- paste0(esc_regex(valid_symbols), collapse = "|")
  label_alt <- paste0(esc_regex(valid_labels), collapse = "|")

  token_regex <- paste0(
    "\\[(", label_alt, ")(\\d*)\\]",
    "|",
    "(", sym_alt, ")(\\d*)"
  )

  x <- trimws(mf)
  x[is.na(x)] <- ""

  out <- data.table::data.table(
    mf = mf,
    is_empty = is.na(mf) | !nzchar(x),
    is_inchikey = FALSE,
    has_charge = FALSE,
    is_parseable = FALSE,
    is_neutral_mf = FALSE,
    issue = NA_character_
  )

  idx <- which(!out$is_empty)

  out[idx, is_inchikey := grepl(
    "^[A-Z]{14}-[A-Z]{10}-[A-Z]$",
    x[idx]
  )]

  out[idx, has_charge := grepl(
    "([+-][0-9]*|[0-9]+[+-])$",
    x[idx]
  )]

  candidate <- !out$is_empty & !out$is_inchikey & !out$has_charge

  # --- fast parseability check: deduplicate first ---------------------------
  if (any(candidate)) {
    x_candidate <- x[candidate]
    x_unique <- unique(x_candidate)

    matches <- regmatches(
      x_unique,
      gregexpr(token_regex, x_unique, perl = TRUE)
    )

    parseable_unique <- vapply(
      seq_along(x_unique),
      function(i) {
        length(matches[[i]]) > 0L &&
          identical(paste0(matches[[i]], collapse = ""), x_unique[i])
      },
      logical(1)
    )

    lut <- stats::setNames(parseable_unique, x_unique)

    out[candidate, is_parseable := unname(lut[x_candidate])]
  }

  out[is_empty == TRUE, issue := "empty_or_na"]
  out[is.na(issue) & is_inchikey == TRUE, issue := "likely_inchikey"]
  out[is.na(issue) & has_charge == TRUE, issue := "charged_formula"]
  out[is.na(issue) & is_parseable == FALSE, issue := "not_parseable_as_neutral_mf"]
  out[is.na(issue) & is_parseable == TRUE, issue := "valid_neutral_mf"]

  out[, is_neutral_mf := issue == "valid_neutral_mf"]

  out[]
}
