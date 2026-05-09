
# Complete formula assignment (wrapper) ####
#
#' @title Complete formula assignment (wrapper function)
#' @name ume_assign_formulas
#' @family Formula assignment
#' @family Formula subsetting
#' @description Assigns molecular formulas to neutral molecular masses and calculates all parameters required for data evaluation, such as
#' a posteriori filtering of molecular formulas, plotting, and statistics.
#' The function uses a pre-build molecular formula library.

#' @inheritParams main_docu
#' @inheritDotParams calc_ma_abs
#' @inheritDotParams calc_neutral_mass
#' @inheritDotParams assign_formulas
#' @inheritDotParams eval_isotopes
#' @inheritDotParams calc_eval_params
#' @inheritDotParams add_known_mf
#' @inheritDotParams calc_norm_int
#' @family ume wrapper
#' @keywords misc
#' @import data.table
#' @return A data.table having molecular formula assignments for each mass.
#' @examples
#' ume_assign_formulas(pl = peaklist_demo, formula_library = lib_demo, pol = "neg", ma_dev = 0.2)
#' @details All function arguments:
#' args(filter_mf_data)
#' args(filter_int)
#' @export

ume_assign_formulas <- function(pl,
                                formula_library,
                                verbose = FALSE,
                                ...) {

# Assign formulas
  mfd <- assign_formulas(pl = pl, formula_library = formula_library, ...)

# Evaluate isotopes and calculate parameters, etc.
  mfd <-  eval_isotopes(mfd = mfd, ...) |>
    calc_eval_params(...) |>
    add_known_mf(...) |>
    calc_norm_int(...)

  return(mfd)
}

# Formula subsetting / filtering -------------------------------

#' @title Complete Formula subsetting / filtering (wrapper)
#' @name ume_filter_formulas
#' @family Formula subsetting
#' @family ume wrapper
#' @description A wrapper function to filter molecular formulas according to a evaluation parameters.
#' @inheritParams main_docu
#' @inheritDotParams filter_mf_data
#' @inheritDotParams subset_known_mf
#' @inheritDotParams calc_norm_int
#' @inheritDotParams filter_int
#' @inheritDotParams remove_blanks
#'
#' @keywords filter_int
#' @import data.table
#' @return A data.table having molecular formula assignments for each mass.
#' ume_filter_formulas(mfd = mf_data_demo, dbe_o_max = 15, norm_int_min = 2)
#' @export

ume_filter_formulas <- function(mfd,
                                verbose = FALSE,
                                ...) {

# Check if mfd is a UME molecular formula data table
  mfd <- check_mfd(mfd)

# Filter by intensity
  mfd_filt <- filter_mf_data(mfd = mfd, ...)

# Filter by molecular formula categories
  mfd_filt <- subset_known_mf(mfd_filt)

# Normalize data by intensity
  mfd_filt <- calc_norm_int(mfd = mfd_filt, ...)

# Filter by intensity
  # Be aware that this filter step might not change the dataset because the filter steps before
  # already excluded formulas outside the norm_int threshold
  mfd_filt <- filter_int(mfd = mfd_filt, ...) # This filter includes a normalization after filtering

# Normalize data by intensity
  mfd_filt <- calc_norm_int(mfd = mfd_filt, ...)

  return(mfd_filt)
}

