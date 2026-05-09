#' @title Calculate Double Bond Equivalent (DBE)
#'
#' @description Calculates the Double Bond Equivalent (DBE) for a given
#' neutral molecular formula.
#' DBE is a measure of unsaturation, representing the total number of rings
#' and pi bonds in a molecule.
#' The function uses the `ume::masses` data table to determine valence information
#' for each element in the input molecular formula.
#'#'
#' It can be calculated from the molecular formula using atomic valences:
#'
#' \deqn{
#' \mathrm{DBE} = 1 + \frac{1}{2} \sum_i n_i (v_i - 2)
#' }
#'
#' where:
#' \itemize{
#'   \item \eqn{n_i}: number of atoms of element \eqn{i}
#'   \item \eqn{v_i}: valence of element \eqn{i} (e.g., C = 4, H = 1,
#'   N = 3, O = 2, S = 2/4/6 depending on bonding state)
#' }
#'
#' This formula works for any set of elements as long as their valence
#' is known. Be aware that some elements can have more than one valence
#' at normal conditions (e.g. Sulfur can have valences of 2, 4 and 6).
#' The function uses the valence that is represented in `ume:masses$valence`.
#'
#' For a reasonable neutral molecule DBE has an integer value >=0.
#' A higher DBE indicates a more unsaturated structure;
#' a lower DBE indicates a more saturated structure.
#'
#' @name calc_dbe
#' @family calculations
#' @inheritParams main_docu
#' @import data.table
#'
#' @return A numeric vector of the same length as the number of rows in `mfd`,
#' where each entry represents the calculated DBE for the corresponding molecular formula.
#' The result vector is named 'dbe'.
#'
#' @examples
#' # Example with user-defined data
#' calc_dbe("C6H10O6")
#' calc_dbe("C6H10Br2")
#' calc_dbe(c("C3[13C1]H10O4", "C6H10O6"))
#'
#' # Example with demo data from UME package
#' calc_dbe(mfd = mf_data_demo)
#'
#' @details
#' This function computes DBE based on the molecular formula specified in `mfd`.
#' `mfd` can be a data.table or a character string or character vector of molecular formula strings.
#'
#' For each isotope in the formula, DBE is calculated as the sum of (valence - 2)
#' multiplied by the count of that isotope, divided by 2, and then adding 1.
#' Elements with a valence of 2 are excluded from the DBE calculation.
#'
#' The function stops with an informative error if valence information is missing
#' for any element or isotope present in `mfd`.
#' @export

calc_dbe <- function(mfd,
                     masses = ume::masses,
                     verbose = FALSE,
                     ...) {

  full_name <- new_name <- isotope_name <- label <- symbol <- valence <- NULL

  # Check if input is a character or character vector of molecular formulas
  if (is.character(mfd)) {
    mfd <- convert_molecular_formula_to_data_table(mfd)
  }

  # Check for empty input
  if (nrow(mfd) == 0) {
    stop("Input data.table 'mfd' is empty. Please provide a non-empty data.table.")
  }

  isotope_info <- get_isotope_info(mfd)
  setnames(mfd, isotope_info$orig_name, isotope_info$label, skip_absent = TRUE)

  # Identify isotopes/elements with missing valence information
  missing_valence <- unique(isotope_info[is.na(valence), .(label, symbol)])

  if (nrow(missing_valence) > 0) {

    # prefer element symbols in the message, because this is usually what users know
    missing_symbols <- unique(missing_valence$symbol)
    missing_labels  <- unique(missing_valence$label)

    stop(
      paste0(
        "Missing valence information for the following element(s) in 'masses': ",
        paste(missing_symbols, collapse = ", "), ".",
        "\nAffected isotope label(s): ",
        paste(missing_labels, collapse = ", "), "."
      )
    )
  }

  # Keep only elements relevant for DBE calculation
  # (valence 2 does not affect DBE)
  elements_included <- isotope_info[!is.na(valence) & valence != 2, ]

  if (nrow(elements_included) == 0) {
    stop(
      "No elements with valid valence information for DBE calculation were found in 'mfd'."
    )
  } else if (isTRUE(verbose)) {
    .msg(
      "Included elements for DBE calculation:\n '%s'",
      paste(elements_included$label, collapse = "', '")
    )
  }

  # DBE calculation
  x_sum <- 0
  for (i in elements_included[, label]) {
    x <- mfd[, get(i)] * (elements_included[label == i, valence] - 2)
    x_sum <- x_sum + x
  }

  dbe <- as.vector(x_sum / 2 + 1)

  return(dbe)
}
