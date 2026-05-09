
# Calculate Ideg ####
#' @title Calculate Degradation Index (Ideg)
#' @family calculations
#' @description
#' This function calculates the degradation index ('Ideg') following Flerus et al. (2012). High Ideg values indicate 'older' marine DOM
#' (i.e., a higher contribution of peaks that correlate negatively with delta14C), while low values indicate 'younger' DOM
#' (i.e., a higher contribution of peaks that correlate positively with delta14C)./
#'
#' Ideg is computed as the ratio of summed magnitudes for five negative (NEG) molecular formulas to the total summed magnitudes of five positive (POS)
#' and five negative (NEG) molecular formulas:
#'
#'
#' \deqn{Ideg = \frac{\sum{NEG}}{\sum{NEG} + \sum{POS}}}
#'
#'
#' The index ranges from 0 to 1 and is valid only if all required formulas (n = 10) are present. Ideg depends strongly on
#' the type of sample preparation, ionization method, and instrument settings, and should only be interpreted for relative
#' changes within the same dataset.
#'
#' @inheritParams main_docu
#' @param mf_col Character. The name of the column containing molecular formulas. Default is "mf".
#' @param magnitude_col Character. The name of the column containing magnitude values (absolute or relative). Default is "i_magnitude".
#' @import data.table
#' @return A `data.table` with columns:
#' - `grp`: Grouping variable.
#' - `ideg`: Calculated degradation index (rounded to 3 decimals).
#' - `ideg_n`: Number of assigned formulas used in the calculation.
#' @examples
#' # Create a minimal dataset containing all required POS and NEG formulas
#' library(data.table)
#'
#' demo_ideg <- data.table(
#'   file_id = 1,
#'   mf = c(
#'     "C17H20O9", "C19H22O10", "C20H22O10", "C20H24O11", "C21H26O11",   # NEG
#'     "C13H18O7", "C14H20O7", "C15H22O7", "C15H22O8", "C16H24O8"        # POS
#'   ),
#'   i_magnitude = c(
#'     1200, 900, 1500, 700, 800,     # NEG intensities
#'     2000, 1800, 2200, 1600, 1900   # POS intensities
#'   )
#' )
#'
#' calc_ideg(
#'   mfd = demo_ideg,
#'   mf_col = "mf",
#'   magnitude_col = "i_magnitude",
#'   grp = "file_id"
#' )
#' @export

calc_ideg <- function(mfd, mf_col = "mf", magnitude_col = "i_magnitude", grp = "file_id", ...) {

  cls <- ideg_n <- mag <- mag_sum <- mag_sum_neg <- mag_sum_pos <- mass_number <- NULL
  n_neg <- n_pos <- parent_label <- sum_neg <- sum_pos <- tmp <- NULL

  if (!inherits(mfd, "data.table")) mfd <- data.table::as.data.table(mfd)
  if (!mf_col %in% names(mfd)) stop("Column ", mf_col, " not found in 'mfd'.")
  if (!magnitude_col %in% names(mfd)) stop("Column ", magnitude_col, " not found in 'mfd'.")
  if (!grp %in% names(mfd)) stop("Grouping column '", grp, "' not found in 'mfd'.")

  NEG <- c("C17H20O9", "C19H22O10", "C20H22O10", "C20H24O11", "C21H26O11")
  POS <- c("C13H18O7", "C14H20O7", "C15H22O7", "C15H22O8", "C16H24O8")
  all_mf <- c(POS, NEG)

  # 1) collapse LCMS spectra within each analysis: sum intensity per formula
  t <- mfd[get(mf_col) %in% all_mf,
           .(mf = get(mf_col),
             mag = as.numeric(get(magnitude_col))),
           by = grp]

  if (nrow(t) == 0L) {
    out <- unique(mfd[, .(tmp = 1L), by = grp])[, tmp := NULL]
    out[, `:=`(ideg = NA_real_, ideg_n = 0L)]
    return(out[])
  }

  t_sum <- t[, .(mag_sum = sum(mag, na.rm = TRUE)), by = c(grp, "mf")]

  # 2) classify and compute index
  t_sum[, cls := data.table::fifelse(mf %in% NEG, "neg",
                                     data.table::fifelse(mf %in% POS, "pos", NA_character_))]

  sums <- t_sum[, .(
    mag_sum_neg = sum(mag_sum[cls == "neg"], na.rm = TRUE),
    mag_sum_pos = sum(mag_sum[cls == "pos"], na.rm = TRUE),
    n_neg       = sum(cls == "neg"),
    n_pos       = sum(cls == "pos")
  ), by = grp]

  sums[, ideg := round(mag_sum_neg / (mag_sum_neg + mag_sum_pos), 3)]
  sums[(mag_sum_neg + mag_sum_pos) == 0, ideg := NA_real_]
  sums[, ideg_n := as.integer(n_neg + n_pos)]

  sums[, c(grp, "ideg", "ideg_n"), with = FALSE][]
}

# Calculate iterr ####
#' @title Calculate terrestrial indeces Iterr and Iterr2 (after Medeiros et al. 2016)
#'
#' @description Calculate a degradation index 'Iterr' and modified index 'iterr2' after Medeiros et al. (2016).
#' High Iterr values represent higher contribution of terrestrial material (i.e. higher contribution
#' of peaks that correlate positively with delta13C) while low values represent less terrestrial
#' material (i.e. higher contribution of peaks that correlate negatively with delta13C).
#' Iterr / Iterr2 are calculated from a peak magnitude ratio of 50 or 5 POS and NEG formulas, respectively:
#' sum(terr) / (sum(terr) + sum(marine))
#' Therefore Iterr / Iterr2 range between 1 and 0. It should be noted that absolute values strongly depend
#' on factors such as type of solid phase extraction, ionization method, instrument settings etc.
#' Therefore values can only be interpreted as relative changes.
#' It should also be noted that for an appropriate evaluation ALL index formulas must be present.
#' @name calc_iterr
#' @family index calculations
#' @inheritParams main_docu
#' @param mf_col Name of the column containing molecular formulas (string)
#' @param magnitude_col Name of the column containing absolute or
#' relative mass peak magnitudes (string).
#' @import data.table
#' @references
#' Medeiros P.M., Seidel M., Niggemann J., Spencer R.G.M., Hernes P.J.,
#' Yager P.L., Miller W.L., Dittmar T., Hansell D.A. (2016).
#' A novel molecular approach for tracing terrigenous dissolved organic matter
#' into the deep ocean. *Global Biogeochemical Cycles*, **30**, 689-699.
#' \doi{10.1002/2015gb005320}
#'
#' @keywords indeces
#' @return Iterr and iterr2 values
#' @examples
#' library(data.table)
#'
#' # Create a minimal dataset containing all required
#' # POS, NEG, POS2, and NEG2 formulas for demonstration
#'
#' demo_iterr <- data.table(
#'   file_id = 1,
#'   mf = c(
#'     # NEG (Iterr)
#'     'C13H12O5','C15H14O4','C14H12O5','C14H14O5','C13H12O6',
#'     'C16H16O4','C15H14O5','C14H12O6','C15H16O5','C14H14O6',
#'     'C16H14O5','C16H16O5','C15H14O6','C15H16O6','C14H14O7',
#'     'C17H16O5','C16H14O6','C17H18O5','C16H16O6','C15H14O7',
#'     'C17H16O6','C16H14O7','C18H18O6','C17H16O7','C17H18O7',
#'     'C18H16O7','C18H18O7','C17H16O8','C19H18O7','C20H20O7',
#'     'C19H18O8','C20H18O9','C19H16O10','C21H20O9','C20H18O10',
#'     'C22H22O9','C21H20O10','C23H22O10','C24H24O10','C25H26O10',
#'
#'     # POS (Iterr)
#'     'C15H19NO6','C15H21NO6','C17H21NO7','C17H23NO7','C17H22O8',
#'     'C16H21NO8','C17H20N2O7','C17H19NO8','C18H23NO7','C17H21NO8',
#'     'C18H24O8','C16H19NO9','C17H23NO8','C17H22O9','C17H24O9',
#'     'C18H21NO8','C17H19NO9','C18H23NO8','C18H22O9','C17H21NO9',
#'     'C18H24O9','C18H20N2O8','C18H21NO9','C19H24O9','C18H23NO9',
#'     'C18H22O10','C18H24O10','C20H24O9','C19H22O10','C20H26O9',
#'     'C19H24O10','C19H26O10','C20H24O10','C20H26O10','C19H24O11',
#'     'C20H24O11','C20H26O11','C20H26O12','C22H28O11','C21H28O12',
#'
#'     # NEG2 (Iterr2)
#'     'C17H18O7','C18H18O7','C17H16O7','C17H16O8','C15H16O6',
#'
#'     # POS2 (Iterr2)
#'     'C20H24O9','C20H24O10','C19H22O10','C17H21NO8','C20H26O9'
#'   ),
#'
#'   # Assign magnitude values (arbitrary but valid)
#'   i_magnitude = c(
#'     rep(1000, 40),  # NEG
#'     rep(2000, 40),  # POS
#'     rep(1500, 5),   # NEG2
#'     rep(1800, 5)    # POS2
#'   )
#' )
#'
#' calc_iterr(
#'   mfd = demo_iterr,
#'   mf_col = "mf",
#'   magnitude_col = "i_magnitude",
#'   grp = "file_id"
#' )
#' @export

calc_iterr <- function(mfd, mf_col = "mf", magnitude_col = "i_magnitude", grp = "file_id", ...) {

  cls <- ideg_n <- mag <- mag_sum <- mag_sum_neg <- mag_sum_pos <- mass_number <- NULL
  n_neg <- n_pos <- parent_label <- sum_neg <- sum_pos <- tmp <- NULL

  if (!inherits(mfd, "data.table")) mfd <- data.table::as.data.table(mfd)
  if (!mf_col %in% names(mfd)) stop("Column ", mf_col, " not found in 'mfd'.")
  if (!magnitude_col %in% names(mfd)) stop("Column ", magnitude_col, " not found in 'mfd'.")
  if (!grp %in% names(mfd)) stop("Grouping column '", grp, "' not found in 'mfd'.")

  NEG <- c('C13H12O5','C15H14O4','C14H12O5','C14H14O5','C13H12O6','C16H16O4',
           'C15H14O5','C14H12O6','C15H16O5','C14H14O6','C16H14O5','C16H16O5',
           'C15H14O6','C15H16O6','C14H14O7','C17H16O5','C16H14O6','C17H18O5',
           'C16H16O6','C15H14O7','C17H16O6','C16H14O7','C18H18O6','C17H16O7',
           'C17H18O7','C18H16O7','C18H18O7','C17H16O8','C19H18O7','C20H20O7',
           'C19H18O8','C20H18O9','C19H16O10','C21H20O9','C20H18O10','C22H22O9',
           'C21H20O10','C23H22O10','C24H24O10','C25H26O10')

  POS <- c('C15H19NO6','C15H21NO6','C17H21NO7','C17H23NO7','C17H22O8','C16H21NO8',
           'C17H20N2O7','C17H19NO8','C18H23NO7','C17H21NO8','C18H24O8','C16H19NO9',
           'C17H23NO8','C17H22O9','C17H24O9','C18H21NO8','C17H19NO9','C18H23NO8',
           'C18H22O9','C17H21NO9','C18H24O9','C18H20N2O8','C18H21NO9','C19H24O9',
           'C18H23NO9','C18H22O10','C18H24O10','C20H24O9','C19H22O10','C20H26O9',
           'C19H24O10','C19H26O10','C20H24O10','C20H26O10','C19H24O11','C20H24O11',
           'C20H26O11','C20H26O12','C22H28O11','C21H28O12')

  NEG2 <- c('C17H18O7','C18H18O7','C17H16O7','C17H16O8','C15H16O6')
  POS2 <- c('C20H24O9','C20H24O10','C19H22O10','C17H21NO8','C20H26O9')

  all_mf <- unique(c(NEG, POS, NEG2, POS2))

  # 1) collapse LCMS spectra within each analysis: sum intensity per formula
  t <- mfd[get(mf_col) %in% all_mf,
           .(mf  = get(mf_col),
             mag = as.numeric(get(magnitude_col))),
           by = grp]

  if (nrow(t) == 0L) {
    out <- unique(mfd[, .(tmp = 1L), by = grp])[, tmp := NULL]
    out[, `:=`(iterr = NA_real_, iterr_n = 0L, iterr2 = NA_real_, iterr2_n = 0L)]
    return(out[])
  }

  t_sum <- t[, .(mag_sum = sum(mag, na.rm = TRUE)), by = c(grp, "mf")]

  # helper to compute one index
  one_index <- function(dt, pos_set, neg_set, idx_name) {
    dt2 <- dt[mf %in% c(pos_set, neg_set)]
    dt2[, cls := data.table::fifelse(mf %in% pos_set, "pos", "neg")]

    res <- dt2[, .(
      sum_pos = sum(mag_sum[cls == "pos"], na.rm = TRUE),
      sum_neg = sum(mag_sum[cls == "neg"], na.rm = TRUE),
      n_pos   = sum(cls == "pos"),
      n_neg   = sum(cls == "neg")
    ), by = grp]

    res[, (idx_name) := round(sum_pos / (sum_pos + sum_neg), 3)]
    res[(sum_pos + sum_neg) == 0, (idx_name) := NA_real_]
    res[, paste0(idx_name, "_n") := as.integer(n_pos + n_neg)]

    res[, c(grp, idx_name, paste0(idx_name, "_n")), with = FALSE]
  }

  dt_iterr  <- one_index(t_sum, POS,  NEG,  "iterr")
  dt_iterr2 <- one_index(t_sum, POS2, NEG2, "iterr2")

  dt_iterr2[dt_iterr, on = grp][]
}



# Calculate the Simpson diversity index ####

#' @title Calculate the Simpson Diversity Index
#'
#' @description
#' The Simpson diversity index is calculated to measure the probability that two randomly selected individuals
#' (e.g., molecular formulas) belong to the same category. It quantifies the dominance or evenness within a dataset.
#'
#' The Simpson index is defined as:
#' \deqn{D = \sum (p_i^2)}
#' where:
#' - \eqn{p_i} is the relative abundance of the \eqn{i}-th molecular formula.
#'
#' The index ranges between 0 and 1:
#' - A value near 0 indicates high diversity (even distribution of abundances).
#' - A value of 1 indicates no diversity (one molecular formula dominates).
#'
#' @param mf Character vector. A list of unique molecular formulas.
#' @param magnitude Numeric vector. A list of respective abundances (intensities) for each molecular formula.
#' Must be non-negative and have the same length as `mf`.
#'
#' @return A single numeric value representing the Simpson diversity index. Returns 0 if `magnitude` is all zeros.
#' @examples
#' calc_simpson_index(
#'   mf = c("C10H20O5", "C12H18O3", "C18H30O6"),
#'   magnitude = c(1982375, 2424, 312410)
#' )
#' @export

calc_simpson_index <- function(mf, magnitude) {
  # Input validation
  if (!is.character(mf) || length(mf) == 0) stop("'mf' must be a non-empty character vector.")
  if (!is.numeric(magnitude) || length(magnitude) != length(mf))
    stop("'magnitude' must be a numeric vector of the same length as 'mf'.")
  if (any(magnitude < 0)) stop("'magnitude' must contain non-negative values.")

  # Total abundance and relative abundances
  total_abundance <- sum(magnitude)
  if (total_abundance == 0) return(0) # Simpson index is zero when total abundance is zero

  relative_abundances <- magnitude / total_abundance

  # Calculate Simpson index
  simpson_index <- sum(relative_abundances^2)

  return(simpson_index)
}


# Calculate Shannon diversity index ####

#' @title Calculate the Shannon Diversity Index
#'
#' @description
#' The Shannon diversity index is calculated to quantify the diversity of molecular formulas based on
#' their relative abundances. This index considers both the richness (number of unique formulas)
#' and the evenness (distribution of abundances). Higher values indicate greater diversity.
#'
#' The Shannon index is defined as:
#' \deqn{H = -\sum (p_i \cdot \ln(p_i))}
#' where:
#' - \eqn{p_i} is the relative abundance of the \eqn{i}-th molecular formula.
#'
#' Zero-abundance formulas are excluded from the calculation.
#'
#' @param mf Character vector. A list of unique molecular formulas.
#' @param magnitude Numeric vector. A list of respective abundances (intensities) for each molecular formula.
#' Must be non-negative and have the same length as `mf`.
#'
#' @return A single numeric value representing the Shannon diversity index. Returns 0 if `magnitude` is all zeros.
#' @examples
#' calc_shannon_index(
#'   mf = c("C10H20O5", "C12H18O3", "C18H30O6"),
#'   magnitude = c(1982375, 2424, 312410)
#' )
#' @export

calc_shannon_index <- function(mf, magnitude) {
  # Input validation
  if (!is.character(mf) || length(mf) == 0) stop("'mf' must be a non-empty character vector.")
  if (!is.numeric(magnitude) || length(magnitude) != length(mf))
    stop("'magnitude' must be a numeric vector of the same length as 'mf'.")
  if (any(magnitude < 0)) stop("'magnitude' must contain non-negative values.")

  # Total abundance and relative abundances
  total_abundance <- sum(magnitude)
  if (total_abundance == 0) return(0) # Shannon index is zero when total abundance is zero

  relative_abundances <- magnitude / total_abundance
  non_zero_relative_abundances <- relative_abundances[relative_abundances > 0]

  # Calculate Shannon index
  shannon_index <- -sum(non_zero_relative_abundances * log(non_zero_relative_abundances))

  return(shannon_index)
}


# Calculate Pielou eveness ####

#' @title Calculate Pielou's Evenness
#'
#' @description
#' This function calculates Pielou's evenness index, a measure of the distribution of abundances across
#' molecular formulas. Evenness ranges from 0 (one molecular formula dominates) to 1 (all formulas are equally abundant).
#'
#' Evenness is derived using the Shannon index:
#' \deqn{E = \frac{H}{\log(S)}}
#' where:
#' - \eqn{H} is the Shannon diversity index.
#' - \eqn{S} is the number of unique molecular formulas.
#'
#' If there is only one molecular formula, evenness is defined as 1.
#'
#' @param mf Character vector. A list of unique molecular formulas.
#' @param magnitude Numeric vector. A list of respective intensities (abundances) for each molecular formula.
#' Must be non-negative and have the same length as `mf`.
#'
#' @return A single numeric value representing Pielou's evenness.
#' @examples
#' calc_pielou_evenness(
#'   mf = c("C10H20O5", "C12H18O3", "C18H30O6"),
#'   magnitude = c(1982375, 2424, 312410)
#' )
#' @export

calc_pielou_evenness <- function(mf, magnitude) {
  # Input validation
  if (!is.character(mf) || length(mf) == 0) stop("'mf' must be a non-empty character vector.")
  if (!is.numeric(magnitude) || length(magnitude) != length(mf))
    stop("'magnitude' must be a numeric vector of the same length as 'mf'.")
  if (any(magnitude < 0)) stop("'magnitude' must contain non-negative values.")

  # Calculate total abundance and relative abundances
  total_abundance <- sum(magnitude)
  if (total_abundance == 0) return(0) # Evenness is undefined if total abundance is zero

  relative_abundances <- magnitude / total_abundance

  # Calculate Shannon index
  shannon_index <- -sum(relative_abundances * log(relative_abundances), na.rm = TRUE)

  # Count of unique molecular formulas
  mf_count <- length(unique(mf))

  # Handle cases with fewer than 2 unique formulas
  if (mf_count <= 1) return(1)

  # Calculate maximum possible Shannon index and Pielou's evenness
  max_possible_shannon <- log(mf_count)
  evenness <- shannon_index / max_possible_shannon

  return(evenness)
}


