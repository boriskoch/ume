#' @title Plot Mass Accuracy vs m/z
#' @name uplot_ma_vs_mz
#' @family uplots
#'
#' @description
#' Generates a UME-style scatter plot showing mass accuracy (`ppm`)
#' versus mass-to-charge ratio (`m/z`).
#'
#' Summary statistics (median, 2.5% and 97.5% quantiles) are displayed
#' as horizontal reference lines and an annotation panel.
#'
#' The plot is returned as a **ggplot2 object** by default, with optional
#' **plotly** conversion for interactivity.
#'
#' @inheritParams main_docu
#'
#' @param ma_col Character. Column containing mass accuracy (ppm).
#'
#' @return A ggplot or plotly object.
#'
#' @examples
#' uplot_ma_vs_mz(mf_data_demo, ma_col = "ppm")
#'
#' @import data.table
#' @import ggplot2
#' @importFrom plotly ggplotly
#'
#' @export
uplot_ma_vs_mz <- function(mfd,
                           ma_col = "ppm",
                           logo = FALSE,
                           plotly = FALSE,
                           ...) {

  #---------------------------
  # 1. Basic column validation
  #---------------------------
  if (!"mz" %in% names(mfd))
    stop("Column 'mz' is missing in mfd.")

  if (!ma_col %in% names(mfd))
    stop("Column '", ma_col, "' not found in mfd.")

  df <- as.data.table(mfd)[, .(mz, ppm = get(ma_col))]

  # Remove NA
  df <- df[is.finite(mz) & is.finite(ppm)]

  #---------------------------
  # 2. Summary statistics
  #---------------------------
  med_ppm  <- median(df$ppm)
  q025_ppm <- quantile(df$ppm, 0.025)
  q975_ppm <- quantile(df$ppm, 0.975)

  #---------------------------
  # 3. ggplot construction
  #---------------------------
  p <- ggplot(df, aes(x = mz, y = ppm)) +
    geom_point(alpha = 0.7, size = 1.8, colour = "#0067a5") +
    geom_hline(yintercept = med_ppm,  linetype = "solid",  colour = "red",  linewidth = 0.6) +
    geom_hline(yintercept = q025_ppm, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
    geom_hline(yintercept = q975_ppm, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
    labs(
      title = "Mass Accuracy vs m/z",
      x = "m/z",
      y = "Mass accuracy (ppm)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.title = element_text(size = 14),
      axis.text  = element_text(size = 12)
    )

  # Add statistics panel (top-left)
  info_txt <- sprintf(
    "Median: %.2f ppm\n2.5%%: %.2f ppm\n97.5%%: %.2f ppm",
    med_ppm, q025_ppm, q975_ppm
  )

  p <- p +
    annotate(
      "text",
      x = min(df$mz, na.rm = TRUE),
      y = max(df$ppm, na.rm = TRUE),
      label = info_txt,
      hjust = 0, vjust = 1,
      size = 4,
      color = "black"
    )

  # Optional logo
  if (logo) {
    p <- p + labs(caption = "UltraMassExplorer")
  }

  #---------------------------
  # 4. Optional: convert to plotly
  #---------------------------
  if (plotly) {
    return(plotly::ggplotly(p))
  }

  return(p)
}
