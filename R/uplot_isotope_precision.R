#' @title Precision of Isotope Abundance
#' @name uplot_isotope_precision
#' @family uplots
#' @family isotopes
#' @description
#' Isotope precision describes how reliably the instrument reproduces the
#' expected intensity of the naturally occurring \eqn{^{13}\mathrm{C}} isotope
#' peak relative to its corresponding monoisotopic \eqn{^{12}\mathrm{C}} peak.
#'
#' @details
#' The measured \eqn{^{13}\mathrm{C}} signal provides an
#' \emph{intrinsic} validation of molecular formula assignments.
#'
#' For a molecule containing \eqn{n} carbon atoms with a natural abundance
#' of 1.07% for
#' \eqn{^{13}\mathrm{C}}, the theoretical relative intensity
#' of the isotope peak \eqn{^{13}\mathrm{C}_{1}}\eqn{^{12}\mathrm{C}_{n-1}} is:
#'
#' \deqn{
#' I_{theo} = n \times 0.0107
#' }
#'
#' The measured intensity \eqn{I_{meas}} provides an independent estimate of
#' the number of carbon atoms:
#'
#' \deqn{
#' n_{calc} = \frac{I_{meas}}{0.0107}
#' }
#'
#' From this, the deviation in carbon number can be defined:
#'
#' \deqn{
#' C_{dev} = n_{assigned} - n_{calc}
#' }
#'
#' A value of \eqn{C_{dev} = 0} indicates perfect agreement between the
#' formula assignment and the isotope-based estimate. Negative values indicate
#' that the measured isotope abundance is lower than expected.
#'
#' Isotope precision is assessed by evaluating the distribution of
#' \eqn{C_{dev}} across peaks with sufficient signal quality.
#' \eqn{C_{dev}} becomes small and stable at higher signal-to-noise ratios
#' (\eqn{S/N}). Therefore, isotopic peak ratios for intense mass signals
#' provide an internal metric for validating molecular formula assignments.

#' The function visualizes the deviation between measured and
#' theoretical \eqn{^{13}\mathrm{C}} isotope ratios.
#' Supports optional data reduction (binning) to enhance interactive
#' rendering speed in `Plotly`.
#'
#' @inheritParams main_docu
#' @param z_var Column used for color mapping (default: "nsp_tot")
#' @param int_col Intensity column (default: "norm_int")
#' @param bins Number of bins used when data_reduction = TRUE
#' @param data_reduction Logical. If TRUE, bins the data and uses bin medians
#'   (recommended for very large datasets; speeds up rendering massively).
#' @param plotly Logical. Return a plotly object instead of ggplot.
#'
#' @return A `ggplot` or `plotly` object.
#'
#' @import data.table ggplot2
#' @importFrom plotly ggplotly
#'
#' @export
uplot_isotope_precision <- function(mfd,
                                    z_var = "nsp_tot",
                                    int_col = "norm_int",
                                    size_dots = 1.5,
                                    bins = 100,
                                    data_reduction = FALSE,
                                    tf = FALSE,
                                    logo = TRUE,
                                    plotly = FALSE,
                                    cex.axis = 1,
                                    cex.lab = 1.4) {

  bin_int <- bin_dev <- NULL

  # ----------- INPUT CHECKS ---------------------------------------------------
  stopifnot(is.data.table(mfd))

  if (!z_var %in% names(mfd)) stop("Column ", z_var, " not found in mfd.")
  if (!int_col %in% names(mfd)) stop("Column ", int_col, " not found in mfd.")
  if (!"dev_n_c" %in% names(mfd)) stop("mfd must contain column 'dev_n_c'.")
  if (!"int13c"  %in% names(mfd)) stop("mfd must contain column 'int13c'.")

  # restrict to usable range
  df <- copy(mfd[int13c > 0 & dev_n_c < 500 & dev_n_c > -500,
                 .(dev_n_c, z = get(z_var), intensity = get(int_col))])

  # ----------- OPTIONAL DATA REDUCTION (BINNING) ------------------------------
  if (isTRUE(data_reduction)) {

    # define bins
    df[, bin_int := cut(intensity, bins, labels = FALSE)]
    df[, bin_dev := cut(dev_n_c, bins, labels = FALSE)]

    # compute medians per bin
    df <- df[, .(
      dev_n_c = as.numeric(median(dev_n_c, na.rm = TRUE)),
      intensity = as.numeric(median(intensity, na.rm = TRUE)),
      z = as.numeric(median(z, na.rm = TRUE))
    ), by = .(bin_int, bin_dev)]

    # remove empty bins
    df <- df[!is.na(dev_n_c) & !is.na(intensity) & !is.na(z)]
  }

  # ----------- ENSURE NO DUPLICATE COLUMN NAMES -------------------------------
  if (anyDuplicated(names(df))) {
    df <- df[, .SD, .SDcols = !duplicated(names(df))]
  }

  # ----------- COLOR SCALE ----------------------------------------------------
  # Use viridis or identity scale depending on z_var size
  color_scale <- if (tf) {
    ggplot2::scale_color_viridis_c(trans = "log")
  } else {
    ggplot2::scale_color_viridis_c()
  }

  # ----------- GGPLOT ---------------------------------------------------------
  p <- ggplot(df, aes(x = intensity, y = dev_n_c, color = z)) +
    geom_point(size = size_dots, alpha = 0.8) +
    color_scale +
    labs(
      x = "Magnitude of parent ion (%)",
      y = "Carbon number deviation (Cdev)",
      title = "Isotope Ratio Precision",
      color = z_var
    ) +
    theme_minimal(base_size = cex.axis * 10) +
    theme(
      axis.title = element_text(size = cex.lab * 10),
      legend.position = "right"
    )

  # baseline + median deviation line
  p <- p +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
    geom_hline(yintercept = median(df$dev_n_c, na.rm = TRUE),
               color = "grey")

  # optional logo (caption is plotly-compatible)
  if (logo) {
    p <- p + labs(caption = "UltraMassExplorer")
  }

  # ----------- RETURN ---------------------------------------------------------
  if (plotly) {
    return(plotly::ggplotly(p))
  } else {
    return(p)
  }
}
