#' @title Plot Mass Spectrum
#' @name uplot_ms
#' @family uplots
#' @description
#' Plots a mass spectrum, showing peak magnitude versus mass-to-charge ratio (m/z).
#'
#' Optionally reduces the dataset by selecting the most abundant peaks per spectrum.
#'
#' @inheritParams main_docu
#' @param pl A `data.table` containing at least columns for mass-to-charge ratio
#'   and peak magnitude (e.g. a peak list or molecular formula data).
#' @param mass Character. Name of the column containing mass-to-charge or mass
#'   information (default = `"mz"`).
#' @param peak_magnitude Character. Name of the column containing peak magnitude
#'   (default = `"i_magnitude"`).
#' @param label Character. Name of the column identifying individual spectra
#'   (default = `"file_id"`).
#' @param data_reduction Numeric between 0 and 1. Fraction of the most abundant
#'   peaks to retain per spectrum. Default = 1 (no reduction).
#'   If set to 0, a minimum of 0.01 is used to ensure some data is displayed.
#'
#' @return A `ggplot` object or a `plotly` object if `plotly = TRUE`.
#'
#' @examples
#' uplot_ms(pl = peaklist_demo, data_reduction = 0.1, plotly = TRUE)
#' uplot_ms(pl = peaklist_demo, data_reduction = 1, plotly = FALSE)
#'
#' @import data.table
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @export

uplot_ms <- function(pl,
                     mass = "mz",
                     peak_magnitude = "i_magnitude",
                     label = "file_id",
                     logo = FALSE,
                     plotly = TRUE,
                     data_reduction = 1,
                     ...) {

  if (!data.table::is.data.table(pl)) {
    pl <- data.table::as.data.table(pl)
  } else {
    pl <- data.table::copy(pl)
  }

  .aes_map <- list(
    x      = mass,
    y      = peak_magnitude,
    colour = label
  )

  .uplots_require_columns(
    pl,
    required = unique(unlist(.aes_map)),
    fun_name = "uplot_ms()"
  )

  if (!is.numeric(data_reduction) || length(data_reduction) != 1L ||
      is.na(data_reduction) || data_reduction < 0 || data_reduction > 1) {
    stop("`data_reduction` must be a numeric value between 0 and 1.", call. = FALSE)
  }

  if (data_reduction == 0) {
    data_reduction <- 0.01
  }

  if (data_reduction < 1) {
    rank_col <- "..uplot_rank"
    n_col <- "..uplot_n"

    pl[, (n_col) := .N, by = label]

    pl[, (rank_col) := data.table::frank(
      -get(peak_magnitude),
      ties.method = "first"
    ), by = label]

    pl <- pl[get(rank_col) <= ceiling(data_reduction * get(n_col))]
    pl[, c(rank_col, n_col) := NULL]
  }

  pl[, (label) := as.factor(get(label))]

  p <- ggplot2::ggplot(
    pl,
    ggplot2::aes(
      x = .data[[mass]],
      y = .data[[peak_magnitude]],
      color = .data[[label]]
    )
  ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        xend = .data[[mass]],
        y = 0,
        yend = .data[[peak_magnitude]]
      ),
      linewidth = 0.5
    ) +
    ggplot2::labs(
      x = "m/z",
      y = "Peak magnitude"
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      legend.title = ggplot2::element_blank()
    )

  uplot_wrapper(
    p,
    title        = "",
    map_labels   = .aes_map,
    colour_scale = "discrete",
    logo         = logo,
    plotly       = plotly,
    ...
  )
}
