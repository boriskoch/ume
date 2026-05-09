# Calculate exact molecular mass from formula ####

#' @title Calculate Exact Monoisotopic Mass of a Molecule
#' @name calc_exact_mass
#' @family calculations
#' @description This function calculates the exact monoisotopic mass for each molecule
#' in a given data table based on the specified isotope composition. Exact masses of
#' elements and isotopes used in the calculation are retrieved from the `ume::masses` data,
#' based on data from NIST (https://www.nist.gov/pml/atomic-weights-and-isotopic-compositions-relative-atomic-masses).
#'
#' @inheritParams main_docu
#' @import data.table
#' @author Boris P. Koch
#' @return A numeric vector of the calculated exact monoisotopic mass.
#' @examples
#' # Example with demo data
#' calc_exact_mass(mfd = mf_data_demo)
#' # Custom example
#' calc_exact_mass(data.table::data.table(c = 3, h = 8, o = 1))
#' @export

calc_exact_mass <- function(mfd, ...) {
  orig_name <- NULL

  # Check if input is a character or character vector of molecular formulas
  if (is.character(mfd)) {
    mfd <- convert_molecular_formula_to_data_table(mfd)
    return(mfd[, mass])
  }

  # Check for empty input
  if (nrow(mfd) == 0) {
    stop("Input data.table 'mfd' is empty. Please provide a non-empty data.table.")
  }

  # Retrieve names of isotopes contained in mfd
  # ume::get_isotope_info() uses ume::masses for the mass values of each isotope
  isotope_info <- get_isotope_info(mfd = mfd)

  # Create substring for nominal mass calculation
  x <-
    isotope_info[, .(calc_single = paste0('`', orig_name, '`', '*', exact_mass))]

  # Create entire string for calculation
  mass_calc <- x[, lapply(.SD, paste0, collapse = " + ")]

  # Pass string to filter
  mass_values <- mfd[, eval(parse(text = mass_calc))]

  return(mass_values)
}
