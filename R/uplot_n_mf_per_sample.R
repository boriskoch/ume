#' Number of Molecular Formulas per Sample / File
#'
#' @title Number of Molecular Formulas per Sample Plot
#' @name uplot_n_mf_per_sample
#' @family uplots
#'
#' @description
#' Creates a bar plot showing how many molecular formulas were assigned per
#' sample (`file_id`). The plot title contains the mean and standard deviation
#' of assigned molecular formulas across samples. Optionally, the plot can be
#' converted to an interactive Plotly plot or display the UltraMassExplorer logo.
#'
#' @inheritParams main_docu
#' @param df A data.table containing at least a `file_id` column.
#' @param col Character. Fill color for the bars (default `"grey"`).
#' @param width Numeric. Width of bars (default `0.3`).
#'
#' @import ggplot2
#' @import data.table
#' @importFrom plotly ggplotly
#'
#' @return A `ggplot` object, or a `plotly` object if `plotly = TRUE`.
#'
#' @examples
#' uplot_n_mf_per_sample(mf_data_demo)
#'
#' @export
uplot_n_mf_per_sample <- function(df,
                                  col = "grey",
                                  logo = TRUE,
                                  width = 0.3,
                                  gg_size = 12,
                                  plotly = FALSE) {

  # Validate input
  if (!"file_id" %in% names(df)) {
    stop("The dataframe must contain a 'file_id' column.")
  }
  if (nrow(df) == 0) stop("Input dataframe is empty.")

  # Compute number of assigned formulas per sample
  df_counts <- df[, .N, by = file_id]

  # Compute summary statistics
  mean_n <- mean(df_counts$N)
  sd_n   <- sd(df_counts$N)

  # Format title
  title_txt <- sprintf(
    "Average formulas per sample: %.2f +/- %.2f (%.2f%%)",
    mean_n, sd_n, (sd_n / mean_n) * 100
  )

  # Build the ggplot
  p <- ggplot(df_counts, aes(x = factor(file_id), y = N)) +
    geom_bar(stat = "identity", fill = col, color = "black", width = width) +
    labs(
      title = title_txt,
      x = "Samples",
      y = "No. of assigned formulas"
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = gg_size),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title = element_text(size = gg_size + 2)
    )

  # Optional UME caption (safer than annotation_custom for CRAN)
  if (logo) {
    p <- p + labs(caption = "UltraMassExplorer")
  }

  # Convert to Plotly if requested
  if (isTRUE(plotly)) {
    p <- plotly::ggplotly(p)
  }

  return(p)
}
