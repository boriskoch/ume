#' Plot Median of Mass Accuracy per Sample (ppm)
#'
#' This function generates a bar plot showing the median of mass accuracy (ppm) for each sample.
#' It also provides the option to convert the plot into an interactive `plotly` object.
#'
#' @inheritParams main_docu
#' @param df A data frame containing the data. The columns `ppm` (ppm values) and `file_id`
#'           (sample identifiers) should be present in the data.
#'
#' @return A `ggplot` object or a `plotly` object depending on the `plotly` argument.
# @importFrom ggplot2 scale_color_identity scale_color_viridis_d geom_hline ggplot geom_bar geom_histogram coord_cartesian geom_point scale_color_gradientn labs theme_minimal theme annotation_custom
#' @import ggplot2 data.table
#' @importFrom plotly ggplotly
#'
# @examples
# uplot_ppm_avg(df = my_data, plotly = TRUE)

uplot_ppm_avg <- function(df,
                            cex.axis = 12,  # Size of axis text
                            cex.lab = 15, # Size of axis label
                            plotly = FALSE,  # Option to return plotly object
                            ...
)
{
  # Check if required columns are present
  if (!all(c("ppm", "file_id") %in% names(df))) {
    stop("Error: 'ppm' or 'file_id' column not found in the data frame.")
  }

  fac <- median_ppm <- NULL

  # Calculate median ppm for each sample
  tmp <- df[, .(median_ppm = median(ppm)), by = .(fac = file_id)]
  setkey(tmp, fac)

  # Create the ggplot object
  p <- ggplot(tmp, aes(x = fac, y = median_ppm)) +
    geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +
    labs(x = "Sample", y = "Median Accuracy (ppm)", title = "Median of ppm") +
    theme_minimal() +
    theme(
      axis.text = element_text(size = cex.axis),
      axis.title = element_text(size = cex.lab),
      plot.title = element_text(size = cex.lab + 1),
      legend.position = "none"
    )

  # Convert to plotly if requested
  if (plotly) {
    return(plotly::ggplotly(p))
  } else {
    return(p)
  }
}
