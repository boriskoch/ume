#' Principal Component Analysis (PCA) Plotting
#'
#' @title Plot PCA Results
#' @name uplot_pca
#' @family uplots
#'
#' @description
#' Performs Principal Component Analysis (PCA) on molecular formula intensity
#' data and visualizes the results as a PCA score plot and a Van Krevelen plot
#' colored by PC1 loadings.
#'
#' @details
#' The PCA is performed on a wide matrix with one row per group defined by
#' `grp` and one column per molecular formula (`mf`). Intensities are aggregated
#' using the mean if multiple values occur for the same combination of `grp`
#' and `mf`.
#'
#' Columns with zero variance are removed before PCA because they cannot be
#' scaled. The argument `grp` defines the observational unit for the PCA, for
#' example `"file_id"`, `"sample_id"`, or `"ms_id"`.
#'
#' @inheritParams main_docu
#' @param grp Character. Name of the column used to define rows/samples in the
#'   PCA matrix.
#' @param int_col Character. Name of the intensity column used for PCA
#'   (default = `"norm_int"`).
#' @param palname Character. Name of the color palette passed to `uplot_vk()`
#'   (default = `"viridis"`).
#' @param col_bar Logical. If `TRUE`, show the color bar in the Van Krevelen
#'   plot.
#' @param ... Additional arguments passed to `uplot_vk()`.
#'
#' @return
#' A list containing:
#' \describe{
#'   \item{pca}{The PCA model object returned by [stats::prcomp()].}
#'   \item{t_score}{A `data.table` with PCA scores for each group.}
#'   \item{fig_vk}{A Van Krevelen plot colored by PC1 loadings.}
#'   \item{fig_pca}{A PCA score plot of PC1 versus PC2.}
#'   \item{mfd}{The input molecular formula data augmented with PC1/PC2 scores
#'   and PC1/PC2 loadings.}
#' }
#'
#' @examples
#' res <- uplot_pca(
#'   mfd = mf_data_demo,
#'   grp = "file_id",
#'   int_col = "norm_int"
#' )
#'
#' res$fig_pca
#' res$fig_vk
#'
#' @note
#' The function uses [stats::prcomp()] for PCA and [uplot_vk()] for the
#' Van Krevelen plot.
#'
#' @seealso [uplot_vk()]
#'
#' @import data.table
#' @importFrom stats prcomp var
#' @importFrom plotly plot_ly layout
#' @export

uplot_pca <- function(mfd,
                      grp,
                      int_col = "norm_int",
                      palname = "viridis",
                      col_bar = TRUE,
                      ...) {

  mf <- PC1 <- PC2 <- pc1_load <- pc2_load <- ..score_cols <- NULL

  # --- Input checks ---------------------------------------------------------
  if (!data.table::is.data.table(mfd)) {
    mfd <- data.table::as.data.table(mfd)
  } else {
    mfd <- data.table::copy(mfd)
  }

  if (!is.character(grp) || length(grp) != 1L || is.na(grp) || !nzchar(grp)) {
    stop("'grp' must be a single column name.", call. = FALSE)
  }

  if (!is.character(int_col) || length(int_col) != 1L || is.na(int_col) || !nzchar(int_col)) {
    stop("'int_col' must be a single column name.", call. = FALSE)
  }

  needed_cols <- c("mf", grp, int_col)
  missing_cols <- setdiff(needed_cols, names(mfd))

  if (length(missing_cols) > 0L) {
    stop(
      "Missing required column(s): ",
      paste0("'", missing_cols, "'", collapse = ", "),
      call. = FALSE
    )
  }

  # --- Pivot table: groups × molecular formulas -----------------------------
  df_pivot <- data.table::dcast(
    mfd,
    formula = stats::as.formula(paste0(grp, " ~ mf")),
    value.var = int_col,
    fun.aggregate = mean,
    fill = 0
  )

  if (nrow(df_pivot) < 2L) {
    stop("PCA requires at least two groups defined by 'grp'.", call. = FALSE)
  }

  group_values <- df_pivot[[grp]]

  df_pivot <- as.data.frame(df_pivot)
  rownames(df_pivot) <- as.character(group_values)
  df_pivot[[grp]] <- NULL

  if (ncol(df_pivot) < 2L) {
    stop("PCA requires at least two molecular formula columns.", call. = FALSE)
  }

  # --- Remove zero-variance columns -----------------------------------------
  zero_var <- vapply(
    df_pivot,
    function(x) {
      stats::var(x, na.rm = TRUE) == 0
    },
    logical(1)
  )

  if (any(zero_var)) {
    df_pivot <- df_pivot[, !zero_var, drop = FALSE]
  }

  if (ncol(df_pivot) < 2L) {
    stop(
      "PCA requires at least two molecular formula columns with non-zero variance.",
      call. = FALSE
    )
  }

  # --- PCA ------------------------------------------------------------------
  pca <- stats::prcomp(df_pivot, scale. = TRUE)

  t_score <- data.table::as.data.table(pca$x)
  t_score[, (grp) := rownames(pca$x)]
  data.table::setcolorder(t_score, c(grp, setdiff(names(t_score), grp)))

  # --- Axis labels ----------------------------------------------------------
  pc_var <- pca$sdev^2 / sum(pca$sdev^2) * 100

  xlab <- sprintf("PC1 (%.3g%%)", pc_var[1])
  ylab <- sprintf("PC2 (%.3g%%)", pc_var[2])

  # --- Plotly PCA score plot ------------------------------------------------
  x_min <- min(t_score$PC1, na.rm = TRUE)
  x_max <- max(t_score$PC1, na.rm = TRUE)

  x_pad <- diff(range(t_score$PC1, na.rm = TRUE)) * 0.15
  if (!is.finite(x_pad) || x_pad == 0) x_pad <- 1

  fig_pca <- plotly::plot_ly(
    data = t_score,
    x = ~PC1,
    y = ~PC2,
    type = "scatter",
    mode = "markers+text",
    text = t_score[[grp]],
    textposition = "right",
    textfont = list(color = "black"),
    marker = list(size = 8)
  ) |>
    plotly::layout(
      xaxis = list(title = xlab, range = c(x_min - x_pad, x_max + x_pad)),
      yaxis = list(title = ylab),
      title = ""
    )

  # --- PCA loadings ---------------------------------------------------------
  loadings <- data.table::as.data.table(pca$rotation)
  loadings[, mf := rownames(pca$rotation)]

  # --- Merge PC loadings onto molecular formula table -----------------------
  mfd2 <- merge(
    mfd,
    loadings[, .(mf, pc1_load = PC1, pc2_load = PC2)],
    by = "mf",
    all.x = TRUE
  )

  mfd2 <- mfd2[!is.na(pc1_load)]

  # --- Merge PCA scores back to molecular formula table ---------------------
  score_cols <- c(grp, "PC1", "PC2")

  # data.table joins require identical column types
  if (!identical(class(t_score[[grp]]), class(mfd2[[grp]]))) {
    t_score[, (grp) := as.character(get(grp))]
    mfd2[, (grp) := as.character(get(grp))]
  }

  mfd2 <- t_score[, ..score_cols][mfd2, on = grp]

  # --- Van Krevelen plot projected by PC1 loadings --------------------------
  fig_vk <- uplot_vk(
    mfd = mfd2,
    z_var = "pc1_load",
    palname = palname,
    col_bar = col_bar,
    main = "Van Krevelen Diagram (colored by PC1 loadings)",
    ...
  )

  # --- Return results -------------------------------------------------------
  list(
    pca = pca,
    t_score = t_score,
    fig_vk = fig_vk,
    fig_pca = fig_pca,
    mfd = mfd2
  )
}
