#' @title Calculate neutral molecular mass
#' @name calc_neutral_mass
#' @family calculations
#'
#' @description
#' Calculates neutral molecular masses for singly charged ions with full numerical
#' precision. No user options are modified.
#'
#' The conversion used is:
#' * negative mode: m = mz + 1.0072763
#' * positive mode: m = mz - 1.0072763
#' * neutral:       m = mz
#'
#' @inheritParams main_docu
#'
#' @param mz Numeric vector of m/z values (> 0).
#' @param pol Character: `"neg"`, `"pos"`, or `"neutral"`.
#'
#' @return Numeric vector of neutral masses.
#'
#' @examples
#' calc_neutral_mass(199.32, pol = "neg")
#'
#' @export

calc_neutral_mass <- function(mz, pol = c("neg", "pos", "neutral"), ...) {

  # Input validation
  if (any(mz <= 0)) {
    stop("The mz values must be >0.")
  }

  if (any(mz > 100000)) {
    warning("The mz values seem unusually high. Please check your input.")
  }

  pol <- match.arg(pol)

  # Neutral mass calculation (singly charged ions)
  m <- switch(pol,
              neg     = mz + 1.0072763,
              pos     = mz - 1.0072763,
              neutral = mz)

  return(m)
}
