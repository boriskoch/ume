#' Carbon vs Mass (CvM) Diagram
#'
#' @title Carbon vs Mass (CvM) Diagram
#' @description
#' Generates a scatter plot of nominal molecular mass (`nm`) versus carbon count (`12C`),
#' coloured by the median a supplied variable (`z_var`), following Reemtsma (2010).
#'
#' @inheritParams main_docu
#' @inheritDotParams uplot_wrapper
#'
#' @references
#' Reemtsma, T. (2010).
#' The carbon versus mass diagram to visualize and exploit FTICR-MS data of
#' natural organic matter.
#' \emph{Journal of Mass Spectrometry}, 45(4), 382–390.
#' \doi{10.1002/jms.1722}
#'
#' @return A ggplot or plotly object.
#' @family uplots
#'
#' @examples
#' uplot_cvm(mfd = mf_data_demo, z_var = "co_tot", ume_logo = FALSE)
#' uplot_cvm(mfd = mf_data_demo, z_var = "norm_int", palname = "viridis")
#'
#' \dontrun{
#' uplot_cvm(mfd = mf_data_demo, z_var = "co_tot", interactive = TRUE)
#' uplot_cvm(mf_data_demo, base_size = 11, palname = "awi", tf = TRUE,
#'   title_show = FALSE, col_bar = FALSE)
#' }
#'
#'
#'
#' @export

uplot_cvm <- function(
    mfd,
    z_var = "co_tot",
    fun = median,
    palname = "redblue",
    tf = FALSE,
    size_dots = 1.5,
    ...
) {

  zz <- NULL

  # -------------------- Declarative mapping --------------------
  .aes_map <- list(
    x      = "nm",
    y      = "12C",
    colour = z_var
  )

  # -------------------- Validation -----------------------------
  .uplots_require_columns(
    mfd,
    required = unique(unlist(.aes_map)),
    fun_name = "uplot_cvm()"
  )

  if (!is.function(fun)) {
    stop("'fun' must be a function (e.g. median, mean, max).", call. = FALSE)
  }

  # -------------------- Aggregation ----------------------------
  mfd_plot <- mfd[,.(zz = as.numeric(fun(get(z_var), na.rm = TRUE)), N = .N),
    by = .(nm, `12C`)
  ]

  if (isTRUE(tf)) {
    if (any(mfd_plot$zz <= 0))
      stop("Log transform requires positive z values.")
    mfd_plot[, zz := log10(zz)]
  }

  # -------------------- Base plot ------------------------------
  p <- ggplot2::ggplot(
    mfd_plot,
    ggplot2::aes(x = nm, y = `12C`, colour = zz)
  ) +
    ggplot2::geom_point()

  # -------------------- Delegate semantics ---------------------
  p <- uplot_wrapper(
    p,
    title      = "Carbon vs. mass (CvM) Diagram",
    map_labels = .aes_map,
    size_dots  = size_dots,
    fun_label = deparse(substitute(fun)),
    ...
  )

  return(p)
}
