#' Plot DBE vs Oxygen Atoms (cf. Herzsprung et al. 2014) with Option for Interactive Plot
#'
#' This function generates a scatter plot of Double Bond Equivalent (DBE) versus the number of oxygen atoms (`o`).
#' It allows for optional customization of colors based on a specified variable (`z_var`) and offers the
#' option to convert the plot to an interactive plotly object.
#'
#' @inheritParams main_docu
#' @inheritDotParams uplot_wrapper
#'
#' @return A ggplot or plotly object.
#' @family uplots
#'
# @examples
# uplot_dbe_vs_o(mfd = mf_data_demo, z_var = "norm_int", plotly = TRUE)

uplot_dbe_vs_o <- function(mfd,
                           z_var = "norm_int",
                           fun = median,
                           palname = "redblue",
                           tf = FALSE,
                           size_dots = 1.5,
                             ...
)
{
  zz <- NULL

  mfd <- data.table::as.data.table(mfd)

  # -------------------- Declarative mapping --------------------
  .aes_map <- list(
    x      = "16O",
    y      = "dbe",
    colour = z_var
  )

  # -------------------- Validation -----------------------------
  .uplots_require_columns(
    mfd,
    required = unique(unlist(.aes_map)),
    fun_name = "uplot_dbe_vs_o()"
  )

  if (!is.function(fun)) {
    stop("'fun' must be a function (e.g. median, mean, max).", call. = FALSE)
  }

  # --- Prepare data -----------------------------------------------------------
  # Prepare data for plotting
  mfd_plot <- mfd[, .(zz = as.numeric(fun(get(z_var), na.rm = TRUE)), N=.N),
                  by = .(`16O`, dbe)]

  # Apply optional log-transform
  if (isTRUE(tf)) {
    if (any(mfd_plot$zz <= 0))
      stop("Log transform requires positive values.")
    mfd_plot[, zz := log10(zz)]
  }

  # -------------------- Base plot ------------------------------
  p <- ggplot2::ggplot(
    mfd_plot,
    ggplot2::aes(x = `16O`, y = dbe, colour = zz)
  ) +
    ggplot2::geom_point()

  # -------------------- Delegate semantics ---------------------
  p <- uplot_wrapper(
    p,
    title      = "DBE vs 16O atoms",
    map_labels = .aes_map,
    size_dots  = size_dots,
    fun_label = deparse(substitute(fun)),
    ...
  )

  return(p)
}
