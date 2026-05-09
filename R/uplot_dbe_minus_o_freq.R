#' Frequency Plot of DBE - O
#'
#' @title Frequency Plot of DBE - O atoms
#' @description
#' Bar plot showing the frequency distribution of double bond equivalents (dbe)
#' minus the number of oxygen atoms in a molecular formula (`dbe_o`).
#' The unified UME plotting system is applied (theme, labels, logo, hover text, plotly).
#'
#' The formula assignment strategy follows chemically motivated constraints
#' and group-wise decision criteria based on DBE and oxygen content to
#' distinguish reliable from equivocal molecular formulas.
#'
#' @inheritParams main_docu
#' @inheritDotParams uplot_wrapper
#'
#' @references
#' Herzsprung, P., Hertkorn, N., von Tümpling, W., Harir, M., Friese, K.,
#' & Schmitt-Kopplin, P. (2014).
#' Understanding molecular formula assignment of Fourier transform ion
#' cyclotron resonance mass spectrometry data of natural organic matter
#' from a chemical point of view.
#' \emph{Analytical and Bioanalytical Chemistry}, 406(30), 7977–7987.
#' \doi{10.1007/s00216-014-8249-y}

#'
#' @return ggplot or plotly object
#' @family uplots
#'
#' @examples
#' uplot_dbe_minus_o_freq(mf_data_demo)
#' uplot_dbe_minus_o_freq(mf_data_demo, interactive = TRUE, ume_logo = FALSE, title_show = FALSE)
#'
#' @export


uplot_dbe_minus_o_freq <- function(
    mfd,
    ...
) {

  .aes_map <- list(
    x = "dbe_o",
    y = "N"
  )

  .uplots_require_columns(
    mfd,
    required = .aes_map$x,
    fun_name = "uplot_dbe_minus_o_freq()"
  )

  # exakte Häufigkeiten für diskrete dbe_o-Werte
  mfd_plot <- mfd[, .N, by = .(dbe_o)][order(dbe_o)]

  p <- ggplot2::ggplot(
    mfd_plot,
    ggplot2::aes(x = dbe_o, y = N)
  ) +
    ggplot2::geom_col(
      width = 0.8,
      color = "black",
      fill  = "grey70"
    )

  p <- uplot_wrapper(
    p,
    map_labels = .aes_map,
    x_npc_logo = 1.1,
    ...
  )

  p
}
