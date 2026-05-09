# Calculate Nominal Mass of a Molecule ####
#' @title Calculate Nominal Mass of a Molecule
#' @name calc_nm
#' @family calculations
#' @description Computes the nominal mass (integer mass) for each molecular formula in the provided data.
#'   This function uses isotope masses stored in the dataset `ume::masses`, based on values from NIST,
#'   for accurate calculation of each element's nominal mass contribution.
#'
#' @inheritParams main_docu
#'
#' @details The function calculates the nominal mass of each molecular formula by retrieving the relevant
#'   integer mass values of isotopes from `ume::masses`. This information is processed to create a calculation
#'   string which is then evaluated to obtain the nominal mass for each molecule.
#'
#'   The nominal mass is derived by summing the integer masses of each constituent element in the formula,
#'   where the integer mass for each element is multiplied by the number of atoms of that element in the molecule.
#'
#'   Note: This function depends on `ume::get_isotope_info()` for isotope data retrieval.
#' @import data.table
#' @return A numeric vector of the calculated nominal mass.
#'
#' @examples
#' # Example using a demo dataset to calculate nominal mass
#' calc_nm(mfd = mf_data_demo)

#' @export

calc_nm <- function(mfd, ...) {

  orig_name <- NULL

  # Check if input is a character or character vector of molecular formulas
  if(is.character(mfd)){

    mfd_new <- convert_molecular_formula_to_data_table(mf = mfd)
    return(mfd_new[, nm])
  }

  # Check for empty input
  if (nrow(mfd) == 0) {
    stop("Input data.table 'mfd' is empty. Please provide a non-empty data.table.")
  }

  # Retrieve names of isotopes contained in mfd.
  # ume::get_isotope_info() uses ume::masses for the mass values of each isotope
  isotope_info <- get_isotope_info(mfd = mfd)

  # Create substring for nominal mass calculation
  x <- isotope_info[, .(calc_single = paste0('`', orig_name, '`', '*', nm))]

  # Create entire string for calculation
  nm_calc <- x[, lapply(.SD, paste0, collapse = " + ")]

  # Pass string to filter
  nm_values <- mfd[, eval(parse(text = nm_calc))]

  return(nm_values)
}
