#' Plot Average Relative Intensity per Sample
#'
#' @title Average Relative Intensity per Sample
#' @name uplot_ri_vs_sample
#' @family uplots
#'
#' @description
#' Creates a bar plot showing the **median relative intensity** (default: `norm_int`)
#' for each sample (grouped by `file_id`).
#' The overall dataset-wide median and standard deviation are shown in the title.
#'
#' @inheritParams main_docu
#'
#' @param df A data.table containing at least:
#'   - a column with relative intensity values (`int_col`)
#'   - a sample or file identifier (`grp`)
#' @param int_col Character. Column name containing relative intensity values.
#' @param grp Character. Column name specifying sample / file grouping.
#' @param col Character. Fill color for bars.
#' @param width Numeric. Width of bars (default `0.3`).
#'
#' @import data.table
#' @import ggplot2
#'
#' @return
#' A **ggplot2 object** containing a bar plot of per-sample median relative intensity.
#'
#' @examples
#' uplot_ri_vs_sample(mf_data_demo, int_col = "norm_int", grp = "file")
#'
#' @export
uplot_ri_vs_sample <- function(df,
                               int_col = "norm_int",
                               grp = "file_id",
                               col = "grey",
                               logo = TRUE,
                               width = 0.3,
                               gg_size = 12) {

  median_int <- NULL

  # --- Checks ------------------------------------------------------------------
  if (!is.data.table(df)) df <- as.data.table(df)

  if (!all(c(int_col, grp) %in% names(df))) {
    stop("The dataframe must contain columns '", int_col,
         "' (relative intensity) and '", grp, "' (group identifier).")
  }

  if (nrow(df) == 0) {
    stop("Input dataframe contains no rows.")
  }

  # --- Summary statistics -------------------------------------------------------
  tmp_mean <- df[, median(get(int_col), na.rm = TRUE)]
  tmp_sd   <- df[, sd(get(int_col), na.rm = TRUE)]

  # Format title
  title_text <- paste0(
    "Average relative intensity per sample\n",
    "Median = ", round(tmp_mean, 3),
    "  +/-  ", round(tmp_sd, 3)
  )

  # --- Prepare grouped plotting data -------------------------------------------
  df_plot <- df[, .(median_int = median(get(int_col), na.rm = TRUE)), by = grp]

  # --- Build plot --------------------------------------------------------------
  p <- ggplot(df_plot, aes(x = factor(.data[[grp]]), y = median_int)) +
    geom_bar(stat = "identity", fill = col, color = "black", width = width) +
    labs(
      title = title_text,
      x = "Samples",
      y = "Median relative intensity"
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = gg_size),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title = element_text(size = gg_size + 2),
      plot.margin = margin(t = 10, r = 20, b = 10, l = 10)
    )

  # --- Add logo ---------------------------------------------------------------
  if (logo) {
    p <- p + labs(caption = "UltraMassExplorer")
  }

  return(p)
}
