#' @title Build Isotope Parent–Daughter Map from Molecular Formula Data
#' @name build_isotope_map
#' @description
#' Internal helper function that constructs an isotope substitution map
#' for elements present in a molecular formula data table (`mfd`).
#'
#' The function identifies all isotope columns (e.g. `"12C"`, `"13C"`, `"14N"`)
#' contained in `mfd`, determines which isotopes are actually present
#' (atom count > 0 in at least one formula), and then retrieves the two most
#' abundant stable isotopes per element from the global `masses` table.
#'
#' For each detected element, the most abundant isotope is defined as the
#' **parent isotope**, and the second most abundant isotope is defined as the
#' **daughter isotope**. These pairs are later used to generate single
#' isotope-substituted molecular formulas.
#'
#' @details
#' Only elements that occur in `mfd` and are represented in `masses$label`
#' are considered. If no isotope columns are detected or none contain
#' non-zero counts, an empty `data.table` is returned.
#'
#' The function assumes that a global object `masses` exists containing at
#' least the columns:
#' \itemize{
#'   \item `label` – isotope label (e.g. `"12C"`)
#'   \item `symbol` – element symbol (e.g. `"C"`)
#'   \item `mole_fraction` – natural isotopic abundance
#' }
#'
#' The resulting isotope map always contains at most two isotopes per element
#' (parent and daughter), ranked by natural abundance.
#'
#' @param mfd A `data.table` representing molecular formulas in wide format,
#'   containing isotope count columns (e.g. `"12C"`, `"13C"`, `"14N"`).
#'
#' @return A `data.table` with one row per detected element and the columns:
#' \describe{
#'   \item{element}{Element symbol.}
#'   \item{parent_label}{Most abundant isotope label.}
#'   \item{parent_mass}{Mass number of the parent isotope.}
#'   \item{parent_mf}{Natural mole fraction of the parent isotope.}
#'   \item{daughter_label}{Second most abundant isotope label.}
#'   \item{daughter_mass}{Mass number of the daughter isotope.}
#'   \item{daughter_mf}{Natural mole fraction of the daughter isotope.}
#' }
#'
#' If no eligible elements are found, an empty `data.table` with the same
#' column structure is returned.
#'
#' @family internal functions
#' @keywords internal isotopes
#' @import data.table
#' @author Boris Koch


build_isotope_map <- function(mfd) {

  parent_label <- daughter_label <- element <- mass_number <- NULL


# --- which elements are present in lib? ####
  # isotope columns are labels like "12C","13C","14N"... (same as masses$label)
  iso_cols_in_lib <- intersect(names(mfd), masses$label)
  if (!length(iso_cols_in_lib)) {
    return(data.table(
      element = character(),
      parent_label = character(), parent_mass = integer(), parent_mf = numeric(),
      daughter_label = character(), daughter_mass = integer(), daughter_mf = numeric()
    ))
  }

  # keep only isotope columns that actually occur (>0 in any row)
  present_iso_cols <- iso_cols_in_lib[colSums(mfd[, iso_cols_in_lib, with = FALSE] > 0) > 0]
  if (!length(present_iso_cols)) {
    return(data.table(
      element = character(),
      parent_label = character(), parent_mass = integer(), parent_mf = numeric(),
      daughter_label = character(), daughter_mass = integer(), daughter_mf = numeric()
    ))
  }

  elems_needed <- unique(masses[label %in% present_iso_cols, symbol])

# build parent/daughter from top-2 mole_fraction per element
  mm <- copy(masses)[symbol %in% elems_needed & !is.na(mole_fraction)]

  mm[, mass_number := as.integer(gsub("[^0-9]", "", label))]
  setorder(mm, symbol, -mole_fraction)

  top2 <- mm[, head(.SD, 2), by = .(element = symbol)]

# reshape into parent/daughter
  top2[, rank := seq_len(.N), by = element]

  parents <- top2[rank == 1, .(element,
                               parent_label = label,
                               parent_mass  = mass_number,
                               parent_mf    = mole_fraction)]
  daughters <- top2[rank == 2, .(element,
                                 daughter_label = label,
                                 daughter_mass  = mass_number,
                                 daughter_mf    = mole_fraction)]

  merge(parents, daughters, by = "element", all.x = TRUE)
}
