#' jet palette
#' 
#' \code{jetPal} is a palette of colors similar that found in Matlab. 
#' This is a nice general palette for data exploration - similar to a rainbow palette 
#' but does not cycle back to the lower color level 
#' (dark blue --> cyan --> yellow --> dark red).
#' 
#' @param n Number of colors to generate
#' 
#' @examples
#' image(volcano, col=jetPal(50))
#' @export
jetPal <-  colorRampPalette(
  c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")
)