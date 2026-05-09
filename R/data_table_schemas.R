
# .ume_schema_peaklist----------

#' @title Data table schemas used in ume
#' @description Internal definitions of expected column structures for key ume table types.
#' @keywords internal

.ume_schema_peaklist <- list(
  cols = c("file_id", "mz", "i_magnitude"),
  types = list(
    "integer",          # file_id must be integer
    "numeric",          # mz must be numeric
    c("numeric", "integer")  # i_magnitude can be numeric OR integer
  )
)

.ume_schema_formula_table <- list(
  cols = c("file_id", "peak_id", "mz", "i_magnitude", "m", "m_cal", "mf"),
  types = list(
    "integer",
    "integer",
    "numeric",
    c("numeric", "integer"),
    "numeric",
    "numeric",
    "character")
)

.ume_schema_library <- list(
  cols = c("mf", "mass", "12C", "1H", "16O"),
  types = c("character", "numeric", "integer", "integer", "integer")
)



# CENTRAL PALETTE REGISTRY ####
# This internal object defines all palettes in ONE place.
# Both f_colorz() and f_colpal_selection() use this.


# .palette_builders ----------------------

#' @title CENTRAL PALETTE REGISTRY
#' @description defines all palettes in ONE place.
#' @import viridis
#' @keywords internal

# Registry of palette creators (single source of truth)
.palette_builders <- list(
  black = function(n) rep("black", n),
  redblue = function(n) {
    steps <- c("blue","royalblue","skyblue","grey","yellow","darkorange","red")
    color.palette(steps, c(20,15,15,15,15,20), space = "rgb")(n)
  },
  ratios = function(n) {
    steps <- c("blue","royalblue","skyblue","black","yellow","darkorange","red")
    color.palette(steps, c(20,20,5,5,20,20), space = "rgb")(n)
  },
  rainbow = function(n) rainbow(n, start = 0.7, end = 0.1),
  awi = function(n) colorRampPalette(c("#4b4b4c","#003e6e","#00abe8","#bcbdbf"))(n),
  viridis = function(n) viridis::viridis(n),
  inferno = function(n) viridis::inferno(n),
  "terrain.colors" = function(n) terrain.colors(n),
  gray = function(n) gray(seq(0,1,length.out=n))
)
