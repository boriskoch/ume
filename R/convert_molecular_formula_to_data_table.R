#' @title Convert Molecular Formulas to a Data Table of Element Counts
#' @name convert_molecular_formula_to_data_table
#' @description
#' Parses molecular formulas and returns a `data.table` where each row
#' represents one molecular formula and each element or isotope is represented
#' by a separate count column.
#'
#' @details
#' The function supports normal element notation such as `C6H12O6` and bracketed
#' isotope notation such as `[13C]`, `[13C2]`, and `[18O2]`.
#'
#' Input formulas are parsed using the element symbols and isotope labels
#' provided in `masses`. This avoids hard-coded element lists and allows rare
#' elements to be parsed as long as they are present in `masses`.
#'
#' By default, input formulas are checked with [check_neutral_mf()] before
#' parsing.
#'
#' The standardized molecular formula `mf` is generated using dynamic Hill
#' ordering:
#' \itemize{
#'   \item if carbon is present: `C`, then `H`, then all other elements alphabetically
#'   \item if carbon is absent: all elements alphabetically, including `H`
#' }
#'
#' @inheritParams main_docu
#' @param table_format A string controlling the output format. Either `"wide"`
#'   (default) or `"long"`.
#' @param keep_mf_old Logical. If `TRUE` (default), the original input formula is
#'   returned in a column named `mf_old`.
#' @param isotope_default A string defining which isotope should be used when
#'   an element is given without explicit isotope notation. Either
#'   `"most_abundant"` (default) or `"lightest"`.
#' @param check_neutral Logical. If `TRUE` (default = `FALSE`), input formulas
#'   are checked with [check_neutral_mf()] before parsing.
#'
#' @return
#' A `data.table` in wide or long format.
#'
#' @family molecular formula functions
#' @keywords chemistry molecular-formula
#' @import data.table
#' @export

convert_molecular_formula_to_data_table <- function(
    mf,
    masses = ume::masses,
    table_format = c("wide", "long"),
    keep_mf_old = TRUE,
    isotope_default = c("most_abundant", "lightest"),
    check_neutral = FALSE
) {

  table_format <- match.arg(table_format)
  isotope_default <- match.arg(isotope_default)

  i2 <- count <- mf_iso <- mf_old <- match <- m_iso <- m_iso_nm <- NULL
  exact_mass <- nm <- symbol <- mass <- nominal_mass <- NULL
  N <- issue <- is_neutral_mf <- NULL
  has_carbon <- hill_order_dyn <- NULL
  i2_out <- count_chr <- match_clean <- NULL
  input_id <- NULL

  # --- input checks ---------------------------------------------------------
  if (is.factor(mf)) {
    mf <- as.character(mf)
  }

  if (!is.character(mf)) {
    stop("'mf' must be a character vector or a string.", call. = FALSE)
  }

  if (anyNA(mf) || any(!nzchar(trimws(mf)))) {
    stop(
      "'mf' must be provided. Some entries in 'mf' are NA or empty strings.",
      call. = FALSE
    )
  }

  if (!data.table::is.data.table(masses)) {
    masses <- data.table::as.data.table(masses)
  }

  needed_cols <- c("label", "symbol", "exact_mass", "nm", "hill_order")
  missing_cols <- setdiff(needed_cols, names(masses))

  if (length(missing_cols) > 0L) {
    stop(
      "'masses' must contain the following columns: ",
      paste(needed_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # --- validate logical args ------------------------------------------------
  if (!is.logical(keep_mf_old) || length(keep_mf_old) != 1L || is.na(keep_mf_old)) {
    stop("'keep_mf_old' must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(check_neutral) || length(check_neutral) != 1L || is.na(check_neutral)) {
    stop("'check_neutral' must be TRUE or FALSE.", call. = FALSE)
  }

  # --- validate neutral molecular formulas ---------------------------------
  if (isTRUE(check_neutral)) {

    mf_check <- check_neutral_mf(mf, masses = masses)
    bad_mf <- mf_check[is_neutral_mf == FALSE]

    if (nrow(bad_mf) > 0L) {

      msg <- bad_mf[, .N, by = issue][order(-N)]

      stop(
        "Some entries in 'mf' are not valid neutral molecular formulas.\n",
        paste0(msg$issue, ": ", msg$N, collapse = "\n"),
        "\n\nExamples:\n",
        paste0(utils::head(bad_mf$mf, 20), collapse = "\n"),
        call. = FALSE
      )
    }
  }

  # --- preserve original input order ---------------------------------------
  mf <- trimws(mf)

  mf_input <- data.table::data.table(
    input_id = seq_along(mf),
    mf_old = mf
  )

  # --- build lookup table ---------------------------------------------------
  m1 <- masses[
    ,
    .(
      i2 = label,
      i2_out = label,
      symbol,
      exact_mass,
      nm,
      hill_order
    )
  ]

  if (isotope_default == "lightest") {

    m2 <- masses[
      ,
      .SD[which.min(exact_mass)],
      by = symbol
    ][
      ,
      .(
        i2 = symbol,
        i2_out = label,
        symbol,
        exact_mass,
        nm,
        hill_order
      )
    ]

  } else {

    m2 <- masses[
      ,
      .SD[which.max(mole_fraction)],
      by = symbol
    ][
      ,
      .(
        i2 = symbol,
        i2_out = label,
        symbol,
        exact_mass,
        nm,
        hill_order
      )
    ]
  }

  m3 <- data.table::rbindlist(
    list(
      m1,
      m2,
      m1[, .(i2 = tolower(i2), i2_out, symbol, exact_mass, nm, hill_order)],
      m2[, .(i2 = tolower(i2), i2_out, symbol, exact_mass, nm, hill_order)]
    ),
    use.names = TRUE
  )

  masses_new <- unique(m3, by = "i2")

  # --- regex helpers --------------------------------------------------------
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

  # --- parse formulas -------------------------------------------------------
  matches <- regmatches(mf, gregexpr(token_regex, mf, perl = TRUE))

  bad_parse <- mf[
    vapply(
      seq_along(matches),
      function(i) {
        length(matches[[i]]) == 0L ||
          !identical(paste0(matches[[i]], collapse = ""), mf[i])
      },
      logical(1)
    )
  ]

  if (length(bad_parse) > 0L) {

    stop(
      "Some formulas could not be parsed completely: '",
      paste0(utils::head(bad_parse, 20), collapse = "', '"),
      if (length(bad_parse) > 20L) "', ..." else "'",
      call. = FALSE
    )
  }

  # --- parse matched tokens -------------------------------------------------
  n_tokens <- lengths(matches)

  dt <- data.table::data.table(
    input_id = rep.int(mf_input$input_id, n_tokens),
    mf_old   = rep.int(mf_input$mf_old, n_tokens),
    mf_iso   = rep.int(mf_input$mf_old, n_tokens),
    match    = unlist(matches, use.names = FALSE)
  )

  is_bracket <- startsWith(dt$match, "[")

  dt[, match_clean := match]
  dt[is_bracket == TRUE, match_clean := substr(match, 2L, nchar(match) - 1L)]

  dt[
    ,
    c("i2", "count_chr") := {
      x <- match_clean
      has_count <- grepl("\\d+$", x)

      id <- x
      cnt <- rep(NA_character_, length(x))

      id[has_count] <- sub("\\d+$", "", x[has_count])
      cnt[has_count] <- sub("^.*?(\\d+)$", "\\1", x[has_count])

      list(id, cnt)
    }
  ]

  dt[, count := data.table::fifelse(
    is.na(count_chr) | count_chr == "",
    1L,
    as.integer(count_chr)
  )]

  dt[, c("match_clean", "count_chr") := NULL]

  # --- validate parsed labels ----------------------------------------------
  false_elements <- unique(dt[!i2 %in% masses_new$i2, i2])

  if (length(false_elements) > 0L) {

    stop(
      "Some formulas contain invalid element/isotope symbols: '",
      paste0(false_elements, collapse = "', '"),
      "'",
      call. = FALSE
    )
  }

  dup_check <- dt[, .N, by = .(mf_iso, i2)][N > 1L]

  if (nrow(dup_check) > 0L) {

    warn_mf <- unique(dup_check$mf_iso)

    warning(
      "Some formulas contain repeated element or isotope tokens. ",
      "Counts will be summed during formula standardization.\n",
      "Examples: ",
      paste0(utils::head(warn_mf, 20), collapse = ", "),
      if (length(warn_mf) > 20L) ", ..." else "",
      call. = FALSE
    )
  }

  # --- add mass metadata ----------------------------------------------------
  dt <- masses_new[dt, on = "i2"]

  dt[, i2 := i2_out]
  dt[, i2_out := NULL]

  # --- calculate masses -----------------------------------------------------
  dt[, m_iso := count * exact_mass]
  dt[, m_iso_nm := count * nm]

  formula_info <- dt[, .(
    mass = sum(m_iso),
    nominal_mass = sum(m_iso_nm)
  ), by = .(input_id, mf_iso, mf_old)]

  # --- standardized molecular formula --------------------------------------
  mf_std <- dt[, .(
    count = sum(count)
  ), by = .(input_id, mf_iso, mf_old, symbol)]

  mf_std[, has_carbon := any(symbol == "C"),
         by = .(input_id, mf_iso, mf_old)]

  mf_std[, hill_order_dyn := data.table::fifelse(
    has_carbon & symbol == "C",
    1L,
    data.table::fifelse(
      has_carbon & symbol == "H",
      2L,
      3L
    )
  )]

  data.table::setorder(
    mf_std,
    input_id,
    hill_order_dyn,
    symbol
  )

  mf_std[, mf := paste0(
    symbol,
    data.table::fifelse(count == 1L, "", as.character(count)),
    collapse = ""
  ), by = .(input_id, mf_iso, mf_old)]

  mf_std[, c("has_carbon", "hill_order_dyn") := NULL]

  mf_std <- unique(
    mf_std[, .(input_id, mf_iso, mf_old, mf)]
  )

  formula_info <- formula_info[
    mf_std,
    on = .(input_id, mf_iso, mf_old)
  ]

  # --- long format ----------------------------------------------------------
  if (table_format == "long") {

    dt <- formula_info[
      dt,
      on = .(input_id, mf_iso, mf_old)
    ]

    data.table::setorder(dt, input_id)

    if (!keep_mf_old) {
      dt[, mf_old := NULL]
    }

    dt[, input_id := NULL]

    data.table::setnames(dt, "nominal_mass", "nm")

    return(dt[])
  }

  # --- wide format ----------------------------------------------------------
  dt_counts <- dt[, .(
    count = sum(count)
  ), by = .(input_id, mf_iso, mf_old, i2)]

  dt_wide_in <- formula_info[
    dt_counts,
    on = .(input_id, mf_iso, mf_old)
  ]

  if (!keep_mf_old) {

    dt_wide_in[, mf_old := NULL]

    dt_wide <- data.table::dcast(
      dt_wide_in,
      input_id + mf_iso + mf + mass + nominal_mass ~ i2,
      value.var = "count",
      fill = 0,
      fun.aggregate = sum
    )

  } else {

    dt_wide <- data.table::dcast(
      dt_wide_in,
      input_id + mf_old + mf_iso + mf + mass + nominal_mass ~ i2,
      value.var = "count",
      fill = 0,
      fun.aggregate = sum
    )
  }

  data.table::setorder(dt_wide, input_id)

  dt_wide[, vkey := .I]

  iso_names <- get_isotope_info(dt_wide)

  data.table::setnames(
    dt_wide,
    iso_names$orig_name,
    iso_names$label,
    skip_absent = TRUE
  )

  first_cols <- c("vkey", "mf")

  if (keep_mf_old) {
    first_cols <- c(first_cols, "mf_old")
  }

  first_cols <- c(first_cols, "mf_iso", "mass", "nominal_mass")

  data.table::setcolorder(
    dt_wide,
    c(first_cols, iso_names$label)
  )

  data.table::setnames(dt_wide, "nominal_mass", "nm")

  dt_wide[, input_id := NULL]

  dt_wide[]
}
