#' Extract molecular formula from InChI
#'
#' @title Extract molecular formula from InChI string
#'
#' @description
#' Extracts the molecular formula from an InChI string by parsing the first
#' layer of the InChI representation. The function performs a fast string-based
#' extraction without requiring external cheminformatics libraries.
#'
#' @param inchi A character vector containing InChI strings (e.g.,
#'   `"InChI=1S/C2H6O/c1-2-3/h3H,2H2,1H3"`).
#'
#' @return
#' A character vector of molecular formulas in Hill notation, with the same
#' length and order as `inchi`. Invalid or missing inputs return `NA_character_`.
#'
#' @details
#' The function extracts the molecular formula from the first layer of the
#' InChI string (i.e., the part immediately following `"InChI=1S/"` or
#' `"InChI=1/"` and before the next `/` separator).
#'
#' This approach is highly efficient because the molecular formula is explicitly
#' encoded in the InChI and does not require interpretation of molecular
#' structure, in contrast to SMILES-based approaches.
#'
#' Leading and trailing whitespace is ignored. Non-character inputs result in
#' an error.
#'
#' @examples
#' inchi <- c(
#'   "InChI=1S/C2H6O/c1-2-3/h3H,2H2,1H3",
#'   "InChI=1S/H2O/h1H2",
#'   NA_character_
#' )
#'
#' inchi_to_mf(inchi)
#' # [1] "C2H6O" "H2O"   NA
#'
#' @seealso
#' \url{https://www.inchi-trust.org/technical-faq/}
#'
#' @export
inchi_to_mf <- function(inchi) {
  if (is.factor(inchi)) inchi <- as.character(inchi)

  if (!is.character(inchi)) {
    stop("'inchi' must be a character vector.", call. = FALSE)
  }

  out <- rep(NA_character_, length(inchi))

  ok <- !is.na(inchi) & nzchar(trimws(inchi))

  x <- trimws(inchi[ok])

  x <- sub("^InChI=1S?/", "", x)

  formula <- sub("/.*$", "", x)

  formula[!nzchar(formula)] <- NA_character_

  out[ok] <- formula
  out
}

