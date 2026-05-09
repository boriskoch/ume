#' Calculate Theoretical Isotope Pattern
#'
#' @title Calculate Isotope Pattern
#' @name calc_isotope_pattern
#'
#' @description
#' Calculates the theoretical isotope pattern of a molecular formula based on
#' natural isotope abundances using multinomial/binomial isotope combinations.
#'
#' @details
#' The function calculates all relevant isotope combinations for each element
#' in a molecular formula and combines them into a theoretical isotope pattern.
#'
#' For each isotope peak, the function returns the exact mass, nominal mass,
#' absolute probability, relative abundance, elemental molecular formula (`mf`),
#' and isotope-specific molecular formula (`mf_iso`).
#'
#' The isotope-specific molecular formula uses bracket notation, for example
#' `[12C2][13C][1H6][16O]`.
#'
#' Very small isotope peaks can be removed using `threshold` and
#' `rel_threshold` to keep the output compact.
#'
#' @inheritParams main_docu
#' @param mf A character vector of molecular formulas or a `data.table`
#'   containing isotope count columns.
#' @param threshold Numeric. Minimum absolute isotope probability retained
#'   during intermediate calculations.
#' @param rel_threshold Numeric. Minimum relative abundance retained in the
#'   final isotope pattern.
#' @param max_peaks Integer. Maximum number of isotope peaks retained during
#'   intermediate calculations.
#' @param mass_digits Integer. Number of decimal places used to merge nearly
#'   identical masses during intermediate calculations.
#'
#' @return
#' A `data.table` with one row per isotope peak and the following columns:
#' \describe{
#'   \item{mf}{Elemental molecular formula.}
#'   \item{mf_iso}{Isotope-specific molecular formula.}
#'   \item{mass}{Exact mass of the isotope composition.}
#'   \item{nominal_mass}{Nominal mass of the isotope composition.}
#'   \item{prob}{Absolute probability of the isotope composition.}
#'   \item{relative_abundance}{Relative abundance normalized to the most
#'   abundant isotope peak.}
#'   \item{isotope_peak}{Peak number ordered by increasing mass.}
#' }
#'
#' @examples
#' calc_isotope_pattern("C2H6O")
#' calc_isotope_pattern("FeC10H10", rel_threshold = 1e-4)
#'
#' @family isotopes
#' @keywords isotopes molecular-formula
#' @import data.table
#' @export

calc_isotope_pattern <- function(mf,
                                 masses = ume::masses,
                                 threshold = 1e-12,
                                 rel_threshold = 1e-6,
                                 max_peaks = 5000L,
                                 mass_digits = 6L) {

  symbol <- label <- mole_fraction <- exact_mass <- nm <- NULL
  count <- mass <- nominal_mass <- prob <- mass_round <- NULL
  rel_abundance <- relative_abundance <- mf_iso <- NULL
  isotope_peak <- mf_old <- n <- NULL

  # --- checks ---------------------------------------------------------------
  if (!data.table::is.data.table(masses)) {
    masses <- data.table::as.data.table(masses)
  }

  needed_cols <- c(
    "label", "symbol", "exact_mass", "nm", "mole_fraction", "hill_order"
  )

  missing_cols <- setdiff(needed_cols, names(masses))

  if (length(missing_cols) > 0L) {
    stop(
      "'masses' must contain the following columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.numeric(threshold) || length(threshold) != 1L || is.na(threshold) ||
      threshold < 0) {
    stop("'threshold' must be a single non-negative numeric value.", call. = FALSE)
  }

  if (!is.numeric(rel_threshold) || length(rel_threshold) != 1L ||
      is.na(rel_threshold) || rel_threshold < 0) {
    stop("'rel_threshold' must be a single non-negative numeric value.", call. = FALSE)
  }

  if (!is.numeric(max_peaks) || length(max_peaks) != 1L ||
      is.na(max_peaks) || max_peaks < 1) {
    stop("'max_peaks' must be a positive integer.", call. = FALSE)
  }

  max_peaks <- as.integer(max_peaks)
  mass_digits <- as.integer(mass_digits)

  # --- input handling -------------------------------------------------------
  if (is.factor(mf)) {
    mf <- as.character(mf)
  }

  if (is.character(mf)) {
    mfd <- convert_molecular_formula_to_data_table(
      mf = mf,
      masses = masses
    )
  } else {
    if (!data.table::is.data.table(mf)) {
      mfd <- data.table::as.data.table(mf)
    } else {
      mfd <- data.table::copy(mf)
    }
  }

  iso_info <- get_isotope_info(mfd = mfd)
  iso_cols <- intersect(iso_info$label, names(mfd))

  if (length(iso_cols) == 0L) {
    stop("No isotope count columns detected in 'mf'.", call. = FALSE)
  }

  # --- helper: all integer isotope compositions -----------------------------
  make_compositions <- function(n, k) {
    if (k == 1L) {
      return(matrix(n, ncol = 1L))
    }

    out <- lapply(0:n, function(i) {
      cbind(i, make_compositions(n - i, k - 1L))
    })

    do.call(rbind, out)
  }

  # --- helper: convert composition row to mf_iso fragment -------------------
  make_mf_iso <- function(labels, counts) {
    keep <- counts > 0L

    if (!any(keep)) {
      return("")
    }

    paste0(
      "[",
      labels[keep],
      ifelse(counts[keep] == 1L, "", counts[keep]),
      "]",
      collapse = ""
    )
  }

  # --- helper: isotope pattern for one element ------------------------------
  element_pattern <- function(el, n) {

    if (n == 0L) {
      return(data.table::data.table(
        mass = 0,
        nominal_mass = 0,
        prob = 1,
        mf_iso = ""
      ))
    }

    iso <- masses[
      symbol == el &
        !is.na(mole_fraction) &
        mole_fraction > 0,
      .(label, symbol, exact_mass, nm, mole_fraction, hill_order)
    ]

    if (nrow(iso) == 0L) {
      stop("No isotope abundance information found for element: ", el)
    }

    data.table::setorder(iso, hill_order, label)

    iso[, mole_fraction := mole_fraction / sum(mole_fraction)]

    comp <- make_compositions(n, nrow(iso))

    log_prob <- lgamma(n + 1) -
      rowSums(lgamma(comp + 1)) +
      as.vector(comp %*% log(iso$mole_fraction))

    data.table::data.table(
      mass = as.vector(comp %*% iso$exact_mass),
      nominal_mass = as.vector(comp %*% iso$nm),
      prob = exp(log_prob),
      mf_iso = vapply(
        seq_len(nrow(comp)),
        function(i) make_mf_iso(iso$label, comp[i, ]),
        character(1)
      )
    )
  }

  # --- helper: combine two isotope-pattern tables ---------------------------
  combine_patterns <- function(a, b) {

    ii <- rep(seq_len(nrow(a)), each = nrow(b))
    jj <- rep(seq_len(nrow(b)), times = nrow(a))

    out <- data.table::data.table(
      mass = a$mass[ii] + b$mass[jj],
      nominal_mass = a$nominal_mass[ii] + b$nominal_mass[jj],
      prob = a$prob[ii] * b$prob[jj],
      mf_iso = paste0(a$mf_iso[ii], b$mf_iso[jj])
    )

    out <- out[prob >= threshold]

    out[, mass_round := round(mass, mass_digits)]

    out <- out[
      ,
      .(
        mass = sum(mass * prob) / sum(prob),
        nominal_mass = round(sum(nominal_mass * prob) / sum(prob)),
        prob = sum(prob),
        mf_iso = mf_iso[which.max(prob)]
      ),
      by = mass_round
    ][
      ,
      mass_round := NULL
    ]

    data.table::setorder(out, -prob)

    if (nrow(out) > max_peaks) {
      out <- out[seq_len(max_peaks)]
    }

    out[]
  }

  # --- calculate pattern row-wise ------------------------------------------
  res_list <- vector("list", nrow(mfd))

  for (i in seq_len(nrow(mfd))) {

    counts <- data.table::data.table(
      label = iso_cols,
      count = as.integer(unlist(mfd[i, c(iso_cols), with = FALSE]))
    )

    counts <- counts[!is.na(count) & count > 0L]

    counts <- iso_info[
      counts,
      on = "label"
    ]

    elem_counts <- counts[
      ,
      .(n = sum(count)),
      by = symbol
    ]

    elem_order <- unique(
      masses[
        symbol %in% elem_counts$symbol,
        .(symbol, hill_order = min(hill_order, na.rm = TRUE)),
        by = symbol
      ][order(hill_order, symbol)]$symbol
    )

    pattern <- data.table::data.table(
      mass = 0,
      nominal_mass = 0,
      prob = 1,
      mf_iso = ""
    )

    for (el in elem_order) {
      n_el <- elem_counts[symbol == el, n]
      ep <- element_pattern(el, n_el)
      pattern <- combine_patterns(pattern, ep)
    }

    pattern[, relative_abundance := prob / max(prob)]
    pattern <- pattern[relative_abundance >= rel_threshold]

    data.table::setorder(pattern, mass)

    pattern[, isotope_peak := seq_len(.N)]

    if ("mf" %in% names(mfd)) {
      pattern[, mf := mfd$mf[i]]
    } else {
      pattern[, mf := NA_character_]
    }

    if ("mf_old" %in% names(mfd)) {
      pattern[, mf_old := mfd$mf_old[i]]
    }

    data.table::setcolorder(
      pattern,
      c(
        intersect(c("mf_old", "mf"), names(pattern)),
        "mf_iso",
        "isotope_peak",
        "mass",
        "nominal_mass",
        "prob",
        "relative_abundance"
      )
    )

    res_list[[i]] <- pattern
  }

  data.table::rbindlist(res_list, fill = TRUE)
}
