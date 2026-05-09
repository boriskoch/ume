# ======================================================================
# f_colorz()
# ======================================================================

#' Create Customized Color Scales
#'
#' @description
#' Creates color scales for numeric values using predefined color palettes.
#' The function supports optional log-transformation of the input values,
#' handles constant vectors gracefully, and maps each numeric value to a
#' color in the selected palette.
#'
#' @inheritParams main_docu
#' @param z Numeric vector. Values whose colors should be computed.
#' @param col_num Integer. Number of colors in the palette (default: `100`).
#'
#' @return A character vector of colors of the same length as `z`.
#'
#' @keywords color palette
#' @keywords internal

f_colorz <- function(z, tf = FALSE, palname = "viridis", col_num = 100,
                     verbose = FALSE, ...) {

  # --- checks -----------------------------------------------------------
  if (!is.numeric(z))
    stop("z value for color is not numeric.", call. = FALSE)

  # --- log transformation -----------------------------------------------
  if (isTRUE(tf)) {
    if (min(z) <= 0)
      stop("Log transformation requires values > 0.", call. = FALSE)
    z <- log(z)
    if (verbose) .msg("Log transformation for color scale applied.")
  }

  # --- obtain palette function ------------------------------------------
  builder <- .palette_builders[[palname]]
  if (is.null(builder))
    stop(sprintf("Unknown palette '%s'.", palname), call. = FALSE)

  colpal <- builder(col_num)

  # --- constant vector: use first color ---------------------------------
  if (diff(range(z)) == 0)
    return(rep(colpal[1], length(z)))

  # --- map to color indices (base R only) -------------------------------
  rng <- range(z)
  idx <- as.integer((z - rng[1]) / (rng[2] - rng[1]) * (col_num - 1)) + 1

  if (verbose) .msg("Color palette: ", palname)

  colpal[idx]
}



# ======================================================================
# f_colpal_selection()
# ======================================================================

#' Retrieve a Palette and a Representative Default Color
#'
#' @description
#' Helper function returning a small palette preview (40 colors) plus
#' a representative "selected color" for legends and UI elements.
#'
#' @param palname Character. The palette name (same options as in `f_colorz()`).
#'
#' @return A list with:
#' * `cpal` — vector of 40 palette colors
#' * `paltype` — type of palette ("limited" or "square")
#' * `colsel` — representative color (middle of the palette)
#'
#' @keywords internal
f_colpal_selection <- function(palname = "awi") {

  builder <- .palette_builders[[palname]]
  if (is.null(builder))
    stop(sprintf("Unknown palette '%s'.", palname), call. = FALSE)

  # generate a 40-color preview
  cpal <- builder(40)
  paltype <- if (palname == "default") "square" else "limited"

  # use center color as representative
  colsel <- cpal[ceiling(length(cpal) / 2)]

  list(cpal = cpal, paltype = paltype, colsel = colsel)
}
