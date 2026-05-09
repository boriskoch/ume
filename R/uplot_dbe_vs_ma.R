#' Plot DBE vs ppm with Option for Interactive Plot
#'
#' This function generates a scatter plot of DBE (Double Bond Equivalent) versus parts per million (ppm) from the provided data.
#' It also provides the option to customize the appearance and to return an interactive `plotly` plot.

#' @inheritParams main_docu
#' @inheritDotParams uplot_wrapper
#'
#' @return A ggplot or plotly object.
#' @family uplots
#'
#' @examples
#' uplot_dbe_vs_ma(mfd = mf_data_demo, size_dots = 1)
#'
#' @export

uplot_dbe_vs_ma <- function(mfd,
                            z_var = "norm_int",
                            fun = median,
                            palname = "redblue",
                            tf = FALSE,
                            size_dots = 1.5,
                             ...
){

  zz <- NULL
  mfd <- data.table::as.data.table(mfd)

  # -------------------- Declarative mapping --------------------
  .aes_map <- list(
    x      = "ppm",
    y      = "dbe",
    colour = z_var
  )

  # -------------------- Validation -----------------------------
  .uplots_require_columns(
    mfd,
    required = unique(unlist(.aes_map)),
    fun_name = "uplot_dbe_vs_ma()"
  )

  if (!is.function(fun)) {
    stop("'fun' must be a function (e.g. median, mean, max).", call. = FALSE)
  }


  # -------------------- Aggregation ----------------------------
  mfd_plot <- mfd[,.(zz = as.numeric(fun(get(z_var), na.rm = TRUE)), N = .N),
                  by = .(ppm, dbe)
  ]

  if (isTRUE(tf)) {
    if (any(mfd_plot$zz <= 0))
      stop("Log transform requires positive z values.")
    mfd_plot[, zz := log10(zz)]
  }

  # -------------------- Base plot ------------------------------
  p <- ggplot2::ggplot(
    mfd_plot,
    ggplot2::aes(x = ppm, y = dbe, colour = zz)
  ) +
    ggplot2::geom_point()

  # -------------------- Delegate semantics ---------------------
  p <- uplot_wrapper(
    p,
    title      = "DBE vs mass accuracy",
    map_labels = .aes_map,
    size_dots  = size_dots,
    fun_label = deparse(substitute(fun)),
    ...
  )

  return(p)
}
