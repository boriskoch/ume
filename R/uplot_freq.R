#' @title Frequency Plot of a Selected Variable
#' @name uplot_freq
#'
#' @description
#' Creates a frequency plot (bar plot) for a selected variable in a molecular
#' formula dataset. Values are grouped and counted, then visualized as bars.
#' A unified UME plot theme is applied for consistent styling across all
#' uplot_* functions.
#'
#' @inheritParams main_docu
#'
#' @param var Character. Name of the variable for which the frequency
#'        distribution should be plotted (e.g. `"14N"`).
#' @param col Bar fill color.
#' @param space Not used (kept for backward compatibility).
#' @param width Bar width.
#'
#' @return
#' A ggplot object, or a plotly object when `plotly = TRUE`.
#'
#' @import data.table
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @export
uplot_freq <- function(
    mfd,
    var = "14N",
    col = "grey",
    space = 0.5,
    width = 0.3,
    logo = TRUE,
    gg_size = 12,
    plotly = FALSE,
    ...
) {

  fac <- N <- NULL
  mfd <- data.table::as.data.table(mfd)

  # Validate input column
  if (!var %in% names(mfd)) {
    stop("Column '", var, "' not found in dataset.")
  }

  # Frequency table
  freq_data <- mfd[, .N, by = .(fac = get(var))]
  data.table::setorder(freq_data, fac)

  # Construct ggplot
  p <- ggplot(freq_data, aes(x = fac, y = N)) +
    geom_bar(
      stat = "identity",
      width = width,
      fill = col,
      color = "black"
    ) +
    labs(
      title = paste("Frequency of", var),
      x = var,
      y = "Count",
      caption = if (logo) "UltraMassExplorer" else NULL
    ) +
    theme_uplots(base_size = gg_size)

  # Convert to plotly if requested
  if (isTRUE(plotly)) {
    return(plotly::ggplotly(p))
  }

  return(p)
}
