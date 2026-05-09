#' @title Calculate mass accuracy
#' @name calc_ma
#' @family calculations
#' @description Calculates relative mass accuracy (ma, in parts per million) as:
#'
#' \eqn{(m_{meas} - m_{calc}) / m_{calc} \times 10^6}

#' where:
#' \itemize{
#'   \item \eqn{m_{meas}} = measured mass
#'   \item \eqn{m_{calc}} = calculated / theoretical (exact) mass
#' }

#' Returned value is rounded to 4 digits.

#' In this context the theoretical mass is represented by the mass
#' of the assigned molecular formula.

#' A small absolute **ppm value** indicates a very precise measurement and increases
#' confidence in correct molecular formula assignment.

#' @inheritParams main_docu
#' @param m Measured mass
#' @param m_cal Calculated (theoretical) mass.
#' @import data.table
#' @keywords misc
#' @return A numeric vector of mass accuracy (rounded to 4 decimals).
#' @examples
#' # Use of single values
#' calc_ma(m = 264.08641, m_cal = 264.08653)
#' # Use in a molecular formula table
#' calc_ma(m = mf_data_demo$m, m_cal = mf_data_demo$m_cal)
#' mf_data_demo[, .(m, m_cal, accuracy_in_ppm = calc_ma(m, m_cal))]
#' @export

calc_ma <- function(m, m_cal, ...) {
  # Check if both m and m_cal are numeric and greater than 0
  if (!is.numeric(m) || !is.numeric(m_cal)) {
    stop("Both m and m_cal must be numeric.")
  }
  if (any(m <= 0) || any(m_cal <= 0)) {
    stop("Both m and m_cal must be greater than 0.")
  }

  ma_ppm <- round((m - m_cal) / (m_cal) * 1000000, 4)

  return(ma_ppm)
}

#' @title Calculate absolute mass accuracy range (ma)
#' @name calc_ma_abs
#' @description This function calculates the absolute mass accuracy range for a neutral mass (m) at a given a mass accuracy (ma_dev).
#' @inheritParams main_docu
#' @param m Measured mass
#' @param ma_dev Mass accuracy in +/- parts per million (ppm)
#' @keywords misc
#' @import data.table
#' @return Returns a list with two values: m_min, m_max
#' @examples calc_ma_abs(m = 327.0134, ma_dev = 0.5)
#' @export

calc_ma_abs <- function(m, ma_dev, ...) {

  # Check if both m and ma_dev are numeric and greater than 0
  if (!is.numeric(m) || !is.numeric(ma_dev)) {
    stop("Both m and ma_dev must be numeric.")
  }
  if (any(m <= 0) || any(ma_dev <= 0)) {
    stop("Both m and ma_dev must be greater than 0.")
  }
  if (any(ma_dev > 100)) {
    message("ma_dev is the relative mass accuracy given in ppm. Typically the values would be <10 ppm in mass spectrometry.")
  }

  m_min <- m - (m * ma_dev / 1000000) # lower limit of mass error
  m_max <- m + (m * ma_dev / 1000000) # upper limit of mass error

  return(list(m_min = m_min, m_max = m_max))
}
