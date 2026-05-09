## Calculate number of molecular formula assignments per molecular mass ####
#' @title Calculate number of molecular formulas that were assigned to a molecular mass.
#' @name calc_number_occurrence
#' @family calculations
#' @description Calculates the number of molecular formula (mf) assignments
#' for each individual peak (peak_id) in a given mass spectrum (ms_id).
#'
#' @inheritParams main_docu
# @inheritParams order_columns
#' @keywords misc internal
#' @return data.table; an additional column "n_occurrence" is added to the original table mfd

calc_number_occurrence <- function(mfd,
                          ...)
{

  check_mfd(mfd)
  setDT(mfd)

# renew occurrence: number of occurrences of each formula in the entire dataset
  isotope_info <- get_isotope_info(mfd = mfd)
  n_occurrence <- mfd[, n_occurrence := .N, by = c(isotope_info[,isotope])]

  return(n_occurrence)
}

