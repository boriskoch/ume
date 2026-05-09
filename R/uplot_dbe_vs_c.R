#' @title Plot DBE vs Carbon Atoms
#' @name uplot_dbe_vs_c
#' @family uplots
#'
#' @description
#' Creates a scatter plot of DBE (double bond equivalents) vs. number of carbon
#' atoms. Points are color-coded by a selected variable (`z_var`). The plot
#' follows the same stylistic conventions as the other uplot_* functions,
#' including the unified theme and optional UME caption.
#'
#' This approach follows the DBE/C concept introduced for identifying
#' aromatic sub-structures in a molecular formula.
#'
#' @inheritParams main_docu
#' @inheritDotParams uplot_wrapper
#'
#' @references
#' Hockaday, W. C., Grannas, A. M., Kim, S., & Hatcher, P. G. (2006).
#' Direct molecular evidence for the degradation and mobility of black carbon in soils
#' from ultrahigh-resolution mass spectral analysis of dissolved organic matter from a
#' fire-impacted forest soil.
#' \emph{Organic Geochemistry}, 37(4), 501–510.
#' \doi{10.1016/j.orggeochem.2005.11.003}
#'
#' @return A ggplot2 object or a plotly object (if `plotly = TRUE`).
#'
#' @examples
#' uplot_dbe_vs_c(mf_data_demo, z_var = "norm_int")
#'
#' @export

uplot_dbe_vs_c <- function(
    mfd,
    z_var = "norm_int",
    fun = median,
    palname = "redblue",
    tf = FALSE,
    size_dots = 1.5,
    ...
) {

  zz <- NULL

  mfd <- data.table::as.data.table(mfd)

  # -------------------- Declarative mapping --------------------
  .aes_map <- list(
    x      = "12C",
    y      = "dbe",
    colour = z_var
  )

  # -------------------- Validation -----------------------------
  .uplots_require_columns(
    mfd,
    required = unique(unlist(.aes_map)),
    fun_name = "uplot_dbe_vs_c()"
  )

  if (!is.function(fun)) {
    stop("'fun' must be a function (e.g. median, mean, max).", call. = FALSE)
  }

  # -------------------- Prepare data ---------------------------
  mfd_plot <- mfd[, .(zz = as.numeric(fun(get(z_var), na.rm = TRUE)), N=.N),
                  by = .(`12C`, dbe)]

  # Apply optional log-transform
  if (isTRUE(tf)) {
    if (any(mfd_plot$zz <= 0))
      stop("Log transform requires positive values.")
    mfd_plot[, zz := log10(zz)]
  }

  # -------------------- Base plot ------------------------------
  p <- ggplot2::ggplot(
    mfd_plot,
    ggplot2::aes(x = `12C`, y = dbe, colour = zz)
  ) +
    ggplot2::geom_point()

  # -------------------- Delegate semantics ---------------------
  p <- uplot_wrapper(
    p,
    map_labels = .aes_map,
    size_dots  = size_dots,
    fun_label = deparse(substitute(fun)),
    ...
  )

  return(p)
}
