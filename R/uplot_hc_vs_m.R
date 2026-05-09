#' @title H/C vs Molecular Mass Plot
#' @name uplot_hc_vs_m
#' @family uplots
#'
#' @description
#' Creates a scatter plot of the hydrogen-to-carbon ratio (H/C) versus molecular
#' mass (`nm`). Points are color-coded according to a selected intensity or
#' property column (`int_col`). This visualization follows the conceptual design
#' in Schmitt-Kopplin et al. (2010).
#'
#' The function can optionally add a branding label ("UltraMassExplorer") and can
#' optionally return an interactive Plotly version of the plot.
#'
#' @inheritParams main_docu
#' @inheritDotParams f_colorz
#'
#'
#'
#' @param df A `data.table` containing columns:
#'   - `nm`: molecular mass
#'   - `hc`: hydrogen-to-carbon ratio
#'   - `int_col`: the column used for color-coding
#' @param int_col Character, column used for color-coding. Default `"norm_int"`.
#' @param palname Character, palette name passed to `f_colorz()`.
#'
#' @return A `ggplot2` scatter plot, or a `plotly` object if `plotly = TRUE`.
#'
#' @examples
#' uplot_hc_vs_m(mf_data_demo, int_col = "norm_int")
#'
#' @import data.table
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @importFrom grid textGrob gpar
#' @export
uplot_hc_vs_m <- function(df,
                          int_col = "norm_int",
                          palname = "redblue",
                          size_dots = 1.2,
                          gg_size = 12,
                          logo = TRUE,
                          plotly = FALSE,
                          ...) {

  zz <- color <- NULL

  # ---- Column validation -----------------------------------------------------
  required <- c("nm", "hc", int_col)
  missing <- setdiff(required, names(df))
  if (length(missing))
    stop("Missing required columns: ", paste(missing, collapse = ", "))

  # Prepare data
  df_hc <- df[, .(nm, hc, zz = get(int_col))]
  df_hc[, color := f_colorz(zz, ...)]

  # ---- ggplot ---------------------------------------------------------------
  p <- ggplot(df_hc, aes(x = nm, y = hc, color = color)) +
    geom_point(size = size_dots, alpha = 0.85) +
    scale_color_identity() +
    labs(
      x = "Molecular Mass (Da)",
      y = "H/C ratio",
      title = "H/C vs Molecular Mass"
    ) +
    theme_minimal(base_size = gg_size) +
    theme(
      legend.position = "right",
      plot.title = element_text(size = gg_size + 2)
    )

  # ---- Add UME logo (unified across UME plots) ------------------------------
  if (logo) {
    p <- p + annotation_custom(
      grid::textGrob(
        label = "UltraMassExplorer",
        hjust = 1, vjust = 0,
        gp = grid::gpar(
          fontsize = gg_size - 2,
          fontface = "italic",
          col = "gray40"
        )
      ),
      xmin = Inf, xmax = Inf,
      ymin = -Inf, ymax = -Inf
    )
  }

  # ---- Optional interactive plot -------------------------------------------
  if (isTRUE(plotly)) {
    return(plotly::ggplotly(p))
  }

  return(p)
}

