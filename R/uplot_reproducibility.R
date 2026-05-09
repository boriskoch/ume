#' @title Check Reproducibility of Sample Analyses
#' @name uplot_reproducibility
#' @family uplots
#'
#' @description
#' Computes reproducibility of sample analyses based on the relative intensity
#' column (`norm_int`). For each molecular formula (`mf`), the function calculates:
#'
#' - number of occurrences (`N`)
#' - median relative intensity (`ri`)
#' - relative standard deviation (RSD = sd/median × 100)
#'
#' It also bins `ri` into integer bins and calculates the median RSD per bin.
#'
#' The function returns:
#' * processed tables
#' * two **ggplot2** objects:
#'   - intensity vs RSD scatter plot
#'   - binned median RSD plot
#'
#' @param df A data.table or data.frame containing at least columns `mf`
#'          and the intensity column defined in `ri`.
#' @param ri Character string: name of the intensity column. Default: `"norm_int"`.
#'
#' @return A list containing:
#' \describe{
#'   \item{`tmp`}{Summary table by molecular formula}
#'   \item{`tmp2`}{Binned median RSD table}
#'   \item{`plot_rsd`}{Scatter plot of RI vs RSD (ggplot2)}
#'   \item{`plot_bins`}{Median RSD per bin (ggplot2)}
#' }
#'
#' @import data.table
#' @import ggplot2
#'
#' @examples
#' out <- uplot_reproducibility(mf_data_demo, ri = "norm_int")
#' out$plot_rsd
#' out$plot_bins
#'
#' @export
uplot_reproducibility <- function(df, ri = "norm_int") {

  rsd <- mrsd <- bin <- NULL

  # --------------------------------------------------------------------
  # Safety & validation
  # --------------------------------------------------------------------
  if (!"mf" %in% names(df))
    stop("Column 'mf' not found in df.")

  if (!ri %in% names(df))
    stop("Relative intensity column '", ri, "' not found in df.")

  df <- as.data.table(df)

  # --------------------------------------------------------------------
  # Compute summary statistics per molecular formula
  # --------------------------------------------------------------------
  tmp <- df[, .(
    N  = .N,
    ri = median(get(ri), na.rm = TRUE),
    rsd = (sd(get(ri), na.rm = TRUE) / median(get(ri), na.rm = TRUE)) * 100
  ), by = mf]

  tmp <- tmp[is.finite(rsd)]

  # --------------------------------------------------------------------
  # Bin ri and compute median RSD per bin
  # --------------------------------------------------------------------
  tmp2 <- tmp[, .(
    mrsd = median(rsd, na.rm = TRUE)
  ), by = .(bin = trunc(ri))]

  tmp2 <- tmp2[is.finite(mrsd)]

  # --------------------------------------------------------------------
  # ggplot #1 — Scatter plot of RI vs RSD
  # --------------------------------------------------------------------
  plot_rsd <- ggplot(tmp, aes(x = ri, y = rsd)) +
    geom_point(alpha = 0.6, size = 1.5) +
    labs(
      title = paste0("Reproducibility (n = ", nrow(tmp), " molecular formulas)"),
      x = "Relative intensity (median)",
      y = "Relative Std. Dev. (%)"
    ) +
    theme_minimal(base_size = 14)

  # --------------------------------------------------------------------
  # ggplot #2 — Median RSD per bin
  # --------------------------------------------------------------------
  med_rsd <- median(tmp2$mrsd, na.rm = TRUE)

  plot_bins <- ggplot(tmp2, aes(x = bin, y = mrsd)) +
    geom_point(size = 2) +
    geom_line(alpha = 0.6) +
    geom_hline(yintercept = med_rsd, linetype = "dashed", color = "red") +
    labs(
      title = paste0("Median RSD per RI bin (Median = ", round(med_rsd, 1), "%)"),
      x = "RI bin (integer truncation)",
      y = "Median RSD (%)"
    ) +
    theme_minimal(base_size = 14)

  # --------------------------------------------------------------------
  # Output
  # --------------------------------------------------------------------
  return(list(
    tmp       = tmp,
    tmp2      = tmp2,
    plot_rsd  = plot_rsd,
    plot_bins = plot_bins
  ))
}
