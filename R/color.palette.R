#' Create a Custom Interpolated Color Palette
#'
#' Constructs a continuous color palette from a sequence of base colors.
#' Intermediate colors are interpolated between each pair of adjacent colors,
#' optionally using a custom number of interpolation steps.
#'
#' This helper is primarily used for UME visualizations (e.g., color bars in
#' density plots), but it can be used independently for any plotting task.
#'
#' @inheritParams main_docu
#'
#' @param steps
#'   A character vector of base colors (e.g., hex codes or color names).
#'   These colors define the breakpoints in the palette.
#'
#' @param n.steps.between
#'   An optional integer vector specifying how many interpolated colors should
#'   be added **between** each pair of entries in `steps`.
#'   Must have length `length(steps) - 1`.
#'   If `NULL` (default), no intermediate colors are added beyond the endpoints.
#'
#' @return
#' A function of class `"colorRampPalette"` that generates interpolated color
#' vectors when called with a single integer argument `n`.
#'
#' For example, `pal <- color.palette(c("blue", "white", "red")); pal(100)`
#' returns a vector of 100 smoothly interpolated colors.
#'
#' @examples
#' # Generate a simple blue-white-red palette
#' pal <- color.palette(c("blue", "white", "red"))
#' pal(10)
#'
#' # Add additional steps between colors
#' pal2 <- color.palette(c("blue", "white", "red"), n.steps.between = c(5, 10))
#' pal2(20)
#'
#' @export

color.palette <- function(steps, n.steps.between=NULL, ...){

  if(is.null(n.steps.between)){
    n.steps.between <- rep(0, (length(steps)-1))
    }
  if(length(n.steps.between) != length(steps)-1){
    stop("Must have one less n.steps.between value than steps")
    }

  fill.steps <- cumsum(rep(1, length(steps))+c(0,n.steps.between))
  RGB <- matrix(NA, nrow=3, ncol=fill.steps[length(fill.steps)])
  RGB[,fill.steps] <- col2rgb(steps)

  for(i in which(n.steps.between>0)){
    col.start=RGB[,fill.steps[i]]
    col.end=RGB[,fill.steps[i+1]]
    for(j in seq(3)){
      vals <- seq(col.start[j], col.end[j], length.out=n.steps.between[i]+2)[2:(2+n.steps.between[i]-1)]
      RGB[j,(fill.steps[i]+1):(fill.steps[i+1]-1)] <- vals
    }
  }

  new.steps <- rgb(RGB[1,], RGB[2,], RGB[3,], maxColorValue = 255)
  pal <- colorRampPalette(new.steps, ...)
  return(pal)
}
