#' @title Histogram of Mass Accuracy
#' @name uplot_freq_ma
#'
#' @description
#' Creates a histogram of mass accuracy values (ppm).
#' Includes summary statistics (median, 2.5% and 97.5% quantiles).
#' Follows general uplot behavior:
#'   - returns a ggplot2 object by default
#'   - converts to plotly *only if* plotly = TRUE
#'   - uses caption-style UME logo
#'
#' @inheritParams main_docu
#' @inheritDotParams uplot_wrapper
#' @param ma_col String. Name of the column having mass accuracy values.
#'
#' @return ggplot or plotly object
#' @family uplots
#'
#' @export

uplot_freq_ma <- function(
    mfd,
    ma_col = "ppm",   # <-- default
    bins   = NULL,
    ...
) {

  mfd <- data.table::as.data.table(mfd)

  # -------------------- Declarative mapping --------------------
  .aes_map <- list(
    x = ma_col
  )

  # -------------------- Validation -----------------------------
  .uplots_require_columns(
    mfd,
    required = ma_col,
    fun_name = "uplot_freq_ma()"
  )

  xvals <- mfd[[ma_col]]

  if (!is.numeric(xvals)) {
    stop(sprintf("Column '%s' must be numeric for histogram.", ma_col),
         call. = FALSE)
  }

  # -------------------- Bin width ------------------------------
  rng <- range(xvals, na.rm = TRUE)
  span <- diff(rng)

  if (is.null(bins)) {
    binwidth <- .uplots_binwidth_fd(xvals)
  } else {
    binwidth <- span / bins
  }

  # Fallbacks for degenerate cases (e.g. all values == 0)
  if (!is.finite(binwidth) || binwidth <= 0) {

    # If all values are identical, pick a small, sensible width
    if (is.finite(span) && span == 0) {
      # width ~ 1% of magnitude, but never smaller than 1e-3
      ref <- max(abs(rng[1]), 1)
      binwidth <- max(ref * 0.01, 1e-3)
    } else {
      # last-resort fallback
      binwidth <- max(abs(stats::median(xvals, na.rm = TRUE)) * 0.01, 1e-3)
    }
  }

  # Optional: ensure the single-value bar is visible (esp. when all == 0)
  x_limits <- NULL
  if (is.finite(span) && span == 0) {
    x0 <- rng[1]
    x_limits <- c(x0 - 2 * binwidth, x0 + 2 * binwidth)
  }

  # -------------------- Base plot ------------------------------
  p <- ggplot2::ggplot(
    mfd,
    ggplot2::aes(x = .data[[ma_col]])
  ) +
    ggplot2::geom_histogram(
      binwidth = binwidth,
      boundary = 0,          # nice for ppm around 0
      closed   = "left",
      color = "black",
      fill  = "grey70"
    )

  if (!is.null(x_limits)) {
    p <- p + ggplot2::coord_cartesian(xlim = x_limits)
  }


  # -------------------- Summary statistics ---------------------
  med  <- round(stats::median(xvals, na.rm = TRUE), 2)
  q975 <- round(stats::quantile(xvals, 0.975, na.rm = TRUE), 2)
  q025 <- round(stats::quantile(xvals, 0.025, na.rm = TRUE), 2)

  title_text <- paste0(
    "Histogram of mass accuracy (", ma_col, ")\n",
    "(Median: ", med, "; ",
    "97.5%: ", q975, "; ",
    "2.5%: ", q025, ")"
  )

  # -------------------- Delegate to wrapper --------------------
  uplot_wrapper(
    p,
    title      = title_text,
    map_labels = .aes_map,
    ...
  )
}
