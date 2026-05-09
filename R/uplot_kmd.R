#' Kendrick Mass Defect (KMD) vs. Nominal Mass Plot
#'
#' @title Kendrick Mass Defect (KMD) vs. Nominal Mass Plot
#' @name uplot_kmd
#' @family uplots
#'
#' @description
#' This function generates a scatter plot of Kendrick Mass Defect (KMD) versus
#' nominal mass (`nm`), with color-coding based on a specified variable
#' (`z_var`). Optionally, the plot can be returned as an interactive Plotly
#' object.
#'
#' @inheritParams main_docu
#' @inheritDotParams uplot_wrapper
#'
#' @return A ggplot or plotly object.
#' @family uplots
#'
#' @references
#' Kendrick E. (1963). A mass scale based on CH\eqn{_2} = 14.0000 for high
#' resolution mass spectrometry of organic compounds.
#' *Analytical Chemistry*, **35**, 2146–2154.
#'
#' Hughey C.A., Hendrickson C.L., Rodgers R.P., Marshall A.G., Qian K.N. (2001).
#' Kendrick mass defect spectrum: A compact visual analysis for ultrahigh-resolution
#' broadband mass spectra. *Analytical Chemistry*, **73**, 4676–4681.
#' \doi{10.1021/ac010560w}
#'
#' @examples
#' uplot_kmd(mf_data_demo, z_var = "norm_int")
#'
#' @export

uplot_kmd <- function(
    mfd,
    z_var = "norm_int",
    fun = median,
    palname = "redblue",
    tf = FALSE,
    size_dots = 1,
    ...
) {

  zz <- NULL

  # -------------------- Declarative mapping --------------------
  .aes_map <- list(
    x      = "nm",
    y      = "kmd",
    colour = z_var
  )

  # -------------------- Validation -----------------------------
  .uplots_require_columns(
    mfd,
    required = unique(unlist(.aes_map)),
    fun_name = "uplot_kmd()"
  )

  if (!is.function(fun)) {
    stop("'fun' must be a function (e.g. median, mean, max).", call. = FALSE)
  }

  # -------------------- Aggregation ----------------------------
  mfd_plot <- mfd[,.(zz = as.numeric(fun(get(z_var), na.rm = TRUE)), N = .N),
                  by = .(nm, kmd)
  ]

  if (isTRUE(tf)) {
    if (any(mfd_plot$zz <= 0))
      stop("Log transform requires positive z values.")
    mfd_plot[, zz := log10(zz)]
  }

  # -------------------- Base plot ------------------------------
  p <- ggplot2::ggplot(
    mfd_plot,
    ggplot2::aes(x = nm, y = kmd, colour = zz)
  ) +
    ggplot2::geom_point()

  # -------------------- Delegate semantics ---------------------
  p <- uplot_wrapper(
    p,
    title      = "Kendrick plot",
    map_labels = .aes_map,
    size_dots  = size_dots,
    fun_label = deparse(substitute(fun)),
    ...
  )

  return(p)
}
