#' Plot Cluster Analysis and Multi-Dimensional Scaling
#'
#' @title uplot_cluster
#' @name uplot_cluster
#' @family uplots
#' @description
#' This function plots the results of a cluster analysis and a multi-dimensional scaling (MDS) plot based on the input data.
#' It first creates a hierarchical cluster dendrogram using the Bray-Curtis dissimilarity index, followed by an MDS plot for dimensionality reduction.
#' The function outputs both plots side by side.
#'
#' @inheritParams main_docu
#' @import data.table vegan
#' @importFrom plotly plot_ly layout
#' @return
#' A named list with two elements:
#'
#' \describe{
#'   \item{\code{dendrogram}}{
#'     A \code{recordedplot} object containing the hierarchical clustering
#'     dendrogram generated from the Bray–Curtis dissimilarity matrix.
#'   }
#'
#'   \item{\code{mds}}{
#'     A \code{plotly} object representing the two-dimensional
#'     Multi-Dimensional Scaling (MDS) scatter plot.
#'     This can be rendered interactively in HTML or converted to
#'     a static ggplot object if needed.
#'   }
#' }
#'
#' The function always returns a list with these two components.
#'
#' @examples
#' # Example with demo data
#'   out <- uplot_cluster(mfd = mf_data_demo, grp = "file", int_col = "norm_int")
#'   out$dendrogram
#'   out$mds
#'
#' @note This function requires the `vegan` package for the Bray-Curtis
#' dissimilarity and MDS calculations.
#' @export

uplot_cluster <- function(mfd, grp = "file_id", int_col = "norm_int", ...) {

  # Check if the necessary columns are present in the data
  if (!grp %in% names(mfd)) stop("Data table must contain a column named '", grp, "'.")
  if (!"mf" %in% names(mfd)) stop("Data table must contain a column named 'mf'.")
  if (!int_col %in% names(mfd)) stop("Data table must contain a column named '", int_col, "'.")

  # Check if there are at least two unique values in the grouping column (grp)
  if (length(unique(mfd[[grp]])) <= 2) {
    stop("Statistical evaluation requires more than 2 unique values in the grouping column (grp).")
  }

  # Pivot the data for cluster analysis

  df_pivot <- dcast(mfd, get(grp) ~ mf, value.var = int_col, fun.aggregate = mean, fill= 0)

  max_char <- max(nchar(as.character(df_pivot[, grp]))) # Determine the length of axis label

  # Convert to a dataframe and set rownames
  df_pivot <- data.frame(df_pivot)
  rownames(df_pivot) <- df_pivot[, 1]
  df_pivot[, 1] <- NULL  # Remove the first column (group names)

  # Calculate Bray-Curtis dissimilarity matrix
  dist_matrix <- vegan::vegdist(df_pivot, method = "bray") * 100 # Scale distances to 100 instead of 1
  hclust_result <- hclust(dist_matrix, method = "average")

  # Plot the hierarchical clustering dendrogram
  dendrogram_plot <- plot(hclust_result,
                          main = "",
                          xlab = "Samples", sub = "", cex = 0.8)
  dendrogram_plot <- grDevices::recordPlot()

  # Perform Multi-dimensional Scaling (MDS)

  invisible(
    capture.output(
  mds_result <- vegan::metaMDS(dist_matrix, k = 2), file = NULL))

  # MDS Plot using Plotly
  mds_plot <- plotly::plot_ly(
    type = "scatter",
    mode = "markers",
    x = mds_result$points[, 1],  # First MDS axis
    y = mds_result$points[, 2],  # Second MDS axis
    text = rownames(df_pivot),  # Display row names as text labels
    marker = list(color = mds_result$points[, 1], colorscale = "Viridis", size = 10)
  ) |>
    plotly::layout(
      #title = "Multi-dimensional Scaling (MDS)",
      xaxis = list(title = "MDS Axis 1"),
      yaxis = list(title = "MDS Axis 2")
    )

  # Return both plots as a list
  return(list(dendrogram = dendrogram_plot, mds = mds_plot))
}

