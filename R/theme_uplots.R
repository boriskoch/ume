#' Unified UME Theme for All uplot_* Functions
#'
#' @title theme_uplots
#' @description
#' Applies a clean UME-style theme used across all uplot_* visualisations.
#'
#' @param base_size Numeric base font size.
#' @param base_family Font family.
#'
#' @return A ggplot2 theme object.
#' @export
theme_uplots <- function(base_size = 12, base_family = "") {

  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(

      # Backgrounds
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.background  = ggplot2::element_rect(fill = "white", color = NA),

      # Grid removal
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),

      # Axis styling
      axis.line  = ggplot2::element_line(color = "black", linewidth = 0.6),
      axis.ticks = ggplot2::element_line(color = "black", linewidth = 0.6),

      # Text
      axis.text  = ggplot2::element_text(size = base_size),
      axis.title = ggplot2::element_text(size = base_size + 3),

      legend.title = ggplot2::element_text(size = base_size),
      legend.text  = ggplot2::element_text(size = base_size),

      plot.title = ggplot2::element_text(size = base_size + 4
                                         #, face = "bold"
                                         ),
      # Margins
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )
}
