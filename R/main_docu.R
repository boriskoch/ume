#' @title Common parameters for ume package functions
#' @name main_docu
#' @description
#' Central place to document arguments (e.g., `msg`, `pl`, `formula_library`) that are
#' inherited by multiple functions via `@inheritParams main_docu`. This is not a user-facing
#' function and is only provided for documentation reuse.
#'
#' @details
#' Use `@inheritParams main_docu` in other functions to pull in these definitions.
#' This topic is marked internal so it does not clutter the index.
#'
#' @section License:
#' This package is released under the MIT License.
#' See the LICENSE file for details.
#'
#' @param formula_library Molecular formula library: a predefined data.table used for
#' assigning molecular formulas to a peak list and for mass calibration. The library
#' requires a fixed format, including mass values for matching. Predefined libraries
#' are available in the R package \emph{ume.formulas} and further described in
#' Leefmann et al. (2019). A standard library for marine dissolved organic matter is
#' \code{ume.formulas::lib_02}. New libraries can be built using
#' \code{ume::create_ume_formula_library()}.
#'
#' @param grp Character vector. Names of columns (e.g., sample or file identifiers)
#' used to aggregate results.
#'
#' @param i_magnitude String. Name of the column that contains peak intensity information
#' (default: \code{"i_magnitude"}).
#'
#' @param known_mf data.table with known molecular formulas (\code{ume::known_mf}).
#'
#' @param masses A data.table. Defaults to \code{ume::masses} (based on NIST data)
#' containing isotope information for elements, including nominal and exact mass,
#' relative abundance, and Hill system order.
#'
#' @param mf Character vector of molecular formula(s)
#' (e.g., \code{c("C10H23NO4", "C10H24N4O2S")}).
#'
#' @param mfd data.table with molecular formula data as derived from
#' \code{ume::assign_formulas}. Column names of elements/isotopes must match names in
#' the \code{isotope} column of \code{ume::masses}; values are integers representing
#' counts per formula.
#'
#' @param msg logical. Deprecated synonym for `verbose`.
#' @param verbose logical; if \code{TRUE}, show progress messages.
#'
#' @param mz String. Name of the column that contains mass-to-charge information
#' (default: \code{"mz"}).
#'
#' @param palname Character. Name of the palette. Available palettes:
#'   `"black"`, `"redblue"`, `"ratios"`, `"rainbow"`, `"awi"`,
#'   `"viridis"`, `"inferno"`, `"terrain.colors"`, `"gray"`.
#'
#' @param pl data.table containing peak data. Mandatory columns include neutral
#' molecular mass (\code{mass}), peak magnitude (\code{i_magnitude}), and a peak
#' identifier (\code{peak_id}).
#'
#' @param hover_cols Character vector: variables to include in plotly tooltip.
#'   Defaults to `c("dbe_o", "N")`. Uses `nice_labels_dt` for readability.
#'
#' @param logo Logical. If TRUE, adds a UME caption.
#' @param nice_labels Logical. If true (default) axis/legend labels are generated from ume::nice_labels_dt.
#' @param col_bar Logical. If `TRUE`, adds a color legend (default is `TRUE`).
#' @param plotly Logical. If TRUE, return interactive plotly object.
#' @param int_col Character. The name of the column that contains the intensity values
#' to be used (e.g. for clustering or color coding). Default usually is "norm_int" for normalized intensity values.
#' @param tf Logical. If `TRUE`, applies a transformation to the color scale (default is `FALSE`).
#' @param size_dots Numeric. Size of the dots in the plot (default = 0.5).
#' @param gg_size Base text size for `theme_uplots()`. Default = 12.
#' @param cex.axis Numeric. Size of axis text (default is `1`).
#' @param cex.lab Numeric. Size of axis labels (default is `1.4`).
#' @param z_var Character. Column name for variable used for color-coding. Content of column should be numeric.
#' @param bins Numeric. Number of bins(e.g. for the x-scale in a histogram)
#'
#' @param fun Function used to aggregate `z_var` for identical combinations.
#' Default is `median`.
#'
#' @param ... Additional arguments passed to methods.
#'
#' @keywords internal
NULL
