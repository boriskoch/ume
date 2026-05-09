#' @title Plot LC-MS Spectrum (or fallback MS if no RT available)
#' @name uplot_lcms
#' @family uplots
#'
#' @description
#' Creates a 3D LC–MS plot (RT x m/z x intensity) **when retention time is available**.
#' If no retention-time column exists (e.g., with DI-FTMS demo data), the function
#' gracefully falls back to `uplot_ms()` and issues an informative message.
#'
#' @inheritParams main_docu
#'
#' @param mass Column containing m/z values (default `"mz"`).
#' @param peak_magnitude Column containing intensity (default `"i_magnitude"`).
#' @param retention_time Column with retention time (default `"ret_time_min"`).
#' @param label Sample/group labeling column (default `"file_id"`).
#'
#' @return A plotly 3D visualization (LC-MS) or a 2D MS spectrum fallback.
#'
#' @import data.table
#' @importFrom plotly plot_ly add_trace layout
#' @export
uplot_lcms <- function(pl,
                       mass = "mz",
                       peak_magnitude = "i_magnitude",
                       retention_time = "ret_time_min",
                       label = "file_id",
                       logo = FALSE,
                       ...) {

  pl <- as.data.table(pl)

  #------------------------------------------------------------
  # 1. Check retention time availability
  #------------------------------------------------------------
  if (!retention_time %in% names(pl)) {
    message("uplot_lcms(): No retention time column found -> using uplot_ms() instead.")
    return(uplot_ms(pl, mass = mass, peak_magnitude = peak_magnitude,
                    label = label, logo = logo, plotly = TRUE))
  }

  #------------------------------------------------------------
  # 2. Standardize column names for internal use
  #------------------------------------------------------------
  setnames(pl,
           old = c(mass, peak_magnitude, retention_time, label),
           new = c("mz", "i_magnitude", "RT", "file_id"),
           skip_absent = TRUE)

  # Data must contain RT values
  if (all(is.na(pl$RT))) {
    message("uplot_lcms(): retention time column exists but contains no values -> using uplot_ms().")
    return(uplot_ms(pl, mass = "mz", peak_magnitude = "i_magnitude",
                    label = "file_id", logo = logo, plotly = TRUE))
  }

  #------------------------------------------------------------
  # 3. Build LC-MS 3D plot
  #------------------------------------------------------------
  split_data <- split(pl, pl$RT)

  fig <- plotly::plot_ly()

  for (i in seq_along(split_data)) {
    df_i <- split_data[[i]]

    fig <- fig |>
      plotly::add_trace(
        data  = df_i,
        x     = ~RT,
        y     = ~mz,
        z     = ~i_magnitude,
        type  = "scatter3d",
        mode  = "lines",
        line  = list(width = 2),
        showlegend = FALSE,
        hovertemplate = paste(
          "RT: %{x} min<br>",
          "m/z: %{y}<br>",
          "Intensity: %{z}<br>"
        )
      )
  }

  #------------------------------------------------------------
  # 4. Layout formatting
  #------------------------------------------------------------
  fig <- fig |>
    plotly::layout(
      title = "3D LC-MS Spectrum",
      scene = list(
        xaxis = list(title = "Retention time (min)"),
        yaxis = list(title = "m/z"),
        zaxis = list(title = "Intensity")
      )
    )

  # Optional logo
  if (logo) {
    fig <- fig |>
      plotly::add_annotations(
        text = "<i>UltraMassExplorer</i>",
        x = 1, y = -0.12,
        xref = "paper", yref = "paper",
        showarrow = FALSE
      )
  }

  return(fig)
}
