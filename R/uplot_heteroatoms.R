#' @title Heteroatom Combination vs Mass Accuracy
#' @name uplot_heteroatoms
#' @family uplots
#'
#' @description
#' Produces a boxplot visualizing the distribution of mass accuracy (`ppm`)
#' for different heteroatom combinations (`nsp_type`) defined by the number
#' of nitrogen (N), sulfur (S), and phosphorus (P) atoms in each formula.
#'
#' The plot can be returned as either a ggplot object or as an interactive
#' plotly object (`plotly = TRUE`). An optional “UltraMassExplorer”
#' watermark can be added.
#'
#' @inheritParams main_docu
#'
#' @param df A `data.table` containing at least:
#'   - `nsp_type`: character or factor indicating heteroatom combinations
#'   - `ppm`: numeric mass accuracy values
#' @param col Character. Box color. Default `"grey"`.
#'
#' @return A ggplot or plotly interactive boxplot.
#'
#' @examples
#' uplot_heteroatoms(mf_data_demo)
#'
#' @import data.table
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @importFrom grid textGrob gpar
#' @export
uplot_heteroatoms <- function(df,
                              col = "grey",
                              gg_size = 12,
                              logo = TRUE,
                              plotly = FALSE) {

  # -------- Column validation -------------------------------------------------
  required <- c("nsp_type", "ppm")
  missing <- setdiff(required, names(df))
  if (length(missing))
    stop("Missing required columns: ", paste(missing, collapse = ", "))

  if (nrow(df) == 0)
    stop("Input data.table is empty.")

  # -------- Convert to data.table ---------------------------------------------
  df <- data.table::as.data.table(df)

  # -------- Base ggplot -------------------------------------------------------
  p <- ggplot(df, aes(x = nsp_type, y = ppm)) +
    geom_boxplot(fill = col, color = "black") +
    labs(
      title = "Mass Accuracy Distribution by Heteroatom Combination",
      x = "Heteroatom combinations (N/S/P)",
      y = "Mass accuracy (ppm)"
    ) +
    theme_minimal(base_size = gg_size) +
    theme(
      plot.title = element_text(size = gg_size + 2),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  # -------- Add UME logo ------------------------------------------------------
  if (logo) {
    p <- p + annotation_custom(
      grid::textGrob(
        label = "UltraMassExplorer",
        gp = grid::gpar(fontsize = gg_size - 2, fontface = "italic", col = "gray40")
      ),
      xmin = -Inf, xmax = -Inf,
      ymin = -Inf, ymax = -Inf
    )
  }

  # -------- Convert to plotly if requested -----------------------------------
  if (isTRUE(plotly)) {
    return(plotly::ggplotly(p))
  }

  return(p)
}
