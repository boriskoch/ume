#' @title Mass Accuracy Frequency Histogram
#' @name uplot_freq_vs_ppm
#' @family uplots
#'
#' @description
#' Creates a histogram showing the frequency distribution of mass accuracy
#' values (`ppm`).
#' Displays median and quantile statistics in the title and optionally adds
#' a UME caption (logo).
#' The plot uses the unified UME theme (`theme_uplots()`), ensuring visual
#' consistency across all `uplot_*` functions.
#'
#' @details
#' This plot is useful for visual inspection of mass accuracy performance.
#' The required additional columns (`14N`, `32S`, `31P`, `dbe_o`) ensure that the
#' dataset is a complete UME molecular formula table and can be compared
#' to other quality-control plots.
#'
#' @inheritParams main_docu
#'
#' @param df A `data.table` or `data.frame` containing columns:
#'   - `ppm` — mass accuracy in ppm
#'   - `14N`, `32S`, `31P`, `dbe_o` — required for consistency with UME QC tools
#'
#' @param col Character. Histogram bar color. Default `"grey"`.
#' @param width Numeric. Histogram bin width (not used when `bins = 100`).
#'
#' @return A **ggplot2 histogram**, or a **plotly object** if `plotly = TRUE`.
#'
#' @examples
#' uplot_freq_vs_ppm(mf_data_demo)
#'
#' @export
uplot_freq_vs_ppm <- function(df,
                              col = "grey",
                              width = 0.01,
                              gg_size = 12,
                              logo = TRUE,
                              plotly = FALSE) {

  required_cols <- c("ppm", "14N", "32S", "31P", "dbe_o")
  missing <- setdiff(required_cols, names(df))
  if (length(missing)) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }

  # --- Summary statistics -------------------------------------------------------
  med   <- median(df$ppm, na.rm = TRUE)
  q975  <- quantile(df$ppm, 0.975, na.rm = TRUE)
  q025  <- quantile(df$ppm, 0.025, na.rm = TRUE)

  title_text <- sprintf(
    "Mass accuracy histogram\n(Median: %.2f ppm; 97.5%% quantile: %.2f ppm; 2.5%% quantile: %.2f ppm)",
    med, q975, q025
  )

  # --- Build ggplot -------------------------------------------------------------
  p <- ggplot(df, aes(x = ppm)) +
    geom_histogram(
      fill = col,
      color = "black",
      bins = 100,
      linewidth = 0.2
    ) +
    labs(
      title = title_text,
      x = "Mass accuracy (ppm)",
      y = "Count",
      caption = if (logo) "UltraMassExplorer" else NULL
    ) +
    theme_uplots(base_size = gg_size) +
    theme(
      plot.title = element_text(size = gg_size + 2, face = "bold"),
      axis.title = element_text(size = gg_size + 2),
      plot.caption = element_text(
        size = gg_size - 2,
        color = "gray40",
        hjust = 1,
        face = "italic"
      )
    )

  # --- Plotly output ------------------------------------------------------------
  if (isTRUE(plotly)) {
    return(plotly::ggplotly(p))
  }

  return(p)
}
