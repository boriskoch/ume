#' @title Internal: Apply UME layout styling to plotly figures
#' @name uplot_layout
#'
#' @description
#' Internal helper function used by UME plotting functions to add
#' consistent layout styling and an optional UME logo annotation to
#' Plotly figures.
#'
#' This function is **not exported**. End users should not call it.
#'
#' @param fig A plotly object.
#' @param margin Logical. If TRUE, applies extended outer margins.
#' @param ... Reserved for future extensions.
#'
#' @return A modified plotly object with UME styling applied.
#'
#' @importFrom plotly add_annotations layout
#' @keywords internal
uplot_layout <- function(fig, margin = TRUE, ...) {

  fig <- fig |>
    plotly::add_annotations(
      text = "<i>UltraMassExplorer</i>",
      font = list(color = "#00abe8"),
      x = 1.05, y = 0.05,
      xref = "paper", yref = "paper",
      align = "left",
      bgcolor = "lightgrey",
      opacity = 0.5,
      textangle = 270,
      showarrow = FALSE
    ) |>
    plotly::layout(
      images = list(
        list(
          # image can be added later via dataURI()
          xref = "paper", yref = "paper",
          x = 1.01, y = 0.0,
          sizex = 0.1, sizey = 0.1,
          opacity = 0.5,
          layer = "above"
        )
      )
    )

  if (margin) {
    fig <- fig |>
      plotly::layout(
        margin = list(
          l = 100,
          r = 100,
          b = 75,
          t = 75,
          pad = 4
        )
      )
  }

  return(fig)
}
