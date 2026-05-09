#' Plot Van Krevelen Diagram
#'
#' @title uplot_vk
#' @description Creates a Van Krevelen diagram (H/C vs O/C).
#'
#' @inheritParams main_docu
#' @inheritDotParams f_colorz
#'
#' @param projection If TRUE, median z-values per (oc, hc) are used.
#' @param median_vK Add median VK point.
#' @param col_median Color of the marker for the median O/C and H/C value (Default = "white")
#' @param ai Add aromaticity index threshold lines.
#'
#' @family uplots
#' @return ggplot or plotly object
#' @export

uplot_vk <- function(mfd,
                     z_var = "norm_int",
                     projection = TRUE,
                     palname = "viridis",
                     median_vK = TRUE,
                     col_median = "white",
                     ai = TRUE,
                     size_dots = 3,
                     col_bar = TRUE,
                     tf = FALSE,
                     ...) {

  # --- Checks -------------------------------------------------------------------
  if (!z_var %in% names(mfd))
    stop("Column '", z_var, "' not found in mfd.")

  if (!all(c("oc", "hc") %in% names(mfd)))
    stop("mfd must contain columns 'oc' and 'hc'.")

  if (nrow(mfd) == 0)
    stop("mfd contains no rows.")

  # --- Data preparation ----------------------------------------------------------
  mfd_vk <- mfd[!is.na(get(z_var)), .(oc, hc, z = get(z_var))]

  if (tf) {
    if (any(mfd_vk$z <= 0))
      stop("Log transform requires z > 0.")
    mfd_vk[, z := log10(z)]
  }

  if (projection) {
    mfd_vk <- mfd_vk[, .(z = median(z)), by = .(oc, hc)]
  }

  # --- Build palette ------------------------------------------------------------
  pal_vec <- f_colorz(z = seq(0, 1, length.out = 256),
                      col_num = 256, ...)

  if (!is.character(pal_vec) || length(pal_vec) < 2)
    stop("Palette generation failed: pal_vec must be a vector of hex colors.")

  # --- Base ggplot (ohne Theme, ohne Labelmapping) ------------------------------
  # p <- ggplot(mfd_vk, aes(x = oc, y = hc, color = z)) +
  #   geom_point(size = size_dots) +
  #   scale_color_gradientn(
  #     colours = pal_vec,
  #     name = z_var,
  #     guide = if (col_bar) "colourbar" else "none"
  #   )
  p <- ggplot(mfd_vk, aes(x = oc, y = hc, color = z)) +
    geom_point(size = size_dots) +
    scale_color_gradientn(
      colours = pal_vec,
      guide = if (col_bar) "colourbar" else "none"
    )


  # --- Median marker ------------------------------------------------------------
  if (median_vK) {
    p <- p + geom_point(
      data = data.frame(
        oc = median(mfd_vk$oc),
        hc = median(mfd_vk$hc)
      ),
      aes(oc, hc),
      inherit.aes = FALSE,
      color = col_median,
      size = 5,
      shape = 13
    )
  }

  # --- Aromaticity index lines ---------------------------------------------------
  if (ai) {
    p <- p +
      annotate("segment",
               x = 0, y = 1.125, xend = 1, yend = 0.2,
               colour = "grey50", linewidth = 0.4) +
      annotate("segment",
               x = 0, y = 0.75, xend = 1, yend = 0.1,
               colour = "grey20", linewidth = 0.7)
  }


  # --- Define automatic label mapping -------------------------------------------
  map_labels <- list(
    x      = "oc",
    y      = "hc",
    colour = z_var
  )

  # --- Delegation an Wrapper ----------------------------------------------------

  uplot_wrapper(
    p,
    title      = "Van Krevelen Diagram",
    map_labels = map_labels,
    ...
  )

}
