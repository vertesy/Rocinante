######################################################################
# Rocinante - LESS USED FUNCTIONS
######################################################################
# source('~/GitHub/Packages/Rocinante/R/Rocinante.less.used.R')
# rm(list = ls(all.names = TRUE)); try(dev.off(), silent = T)




#' val2col
#'
#' This function converts a vector of values("yourdata") to a vector of color levels.
#' One must define the number of colors. The limits of the color scale("zlim") or
#' the break points for the color changes("breaks") can also be defined.
#' When breaks and zlim are defined, breaks overrides zlim.
#' Source: http://menugget.blogspot.nl/2011/09/converting-values-to-color-levels.html
#' @param yourdata The data, to what the colors will be scaled to.
#' @param zlim Limits.
#' @param col Color of the plot.
#' @param breaks Number of bins.
#' @param rename The returned color vector will be named with its previous values
#' @export
#' @examples val2col (yourdata = rpois(200, 20), zlim = c(0,5),col = rev(heat.colors(100)), breaks = 101  )


### CONTAINS A QUICK FIX FOR THE NUMBER OF COLOR LEVELS. See #59 on GitHub ###
val2col <- function(yourdata, # This function converts a vector of values("yourdata") to a vector of color levels. One must define the number of colors. The limits of the color scale("zlim") or the break points for the color changes("breaks") can also be defined. When breaks and zlim are defined, breaks overrides zlim.
                    zlim,
                    col = rev(heat.colors(max(12, 3 * length(unique(yourdata))))),
                    breaks,
                    rename = FALSE) {
  if (!missing(breaks)) {
    if (length(breaks) != (length(col) + 1)) {
      stop("must have one more break than color")
    }
  }
  if (missing(breaks) & !missing(zlim)) {
    breaks <- seq(zlim[1], zlim[2], length.out = (length(col) + 1))
  }
  if (missing(breaks) & missing(zlim)) {
    zlim <- range(yourdata, na.rm = TRUE)
    zlim[2] <- zlim[2] + c(zlim[2] - zlim[1]) * (0.001)
    zlim[1] <- zlim[1] - c(zlim[2] - zlim[1]) * (0.001)
    breaks <- seq(zlim[1], zlim[2], length.out = (length(col) + 1))
  }
  colorlevels <- col[((as.vector(yourdata) - breaks[1]) /
                        (range(breaks)[2] - range(breaks)[1])) * (length(breaks) - 1) + 1]
  if (length(names(yourdata))) {
    names(colorlevels) = yourdata
  }

  if (rename) {
    names(colorlevels) = yourdata
  } # works on vectors only"
  colorlevels
}


colSums.barplot <- function(df, col = "seagreen2", na_rm = TRUE, ...) { barplot(colSums(df, na.rm = na_rm), col = col, ...) } # Draw a barplot from ColSums of a matrix.

hist.XbyY <- function(dfw2col = NULL, toSplit = 1:100, splitby = rnorm(100), breaks_ = 20 ) { # Split a one variable by another. Calculates equal bins in splitby, and returns a list of the corresponding values in toSplit.
  # http://stackoverflow.com/questions/8853735/get-index-of-the-histogram-bin-in-r
  if (NCOL(dfw2col) == 2) { toSplit = dfw2col[ , 1]; splitby = dfw2col[ , 2]; print(11) }
  xx = hist(splitby, breaks = breaks_, plot = TRUE)
  IDX = findInterval(x = splitby, vec = xx$breaks)
  ls = split(toSplit, IDX)
  iprint("Range of data:", range(xx$breaks))
  names(ls) = xx$breaks[-1]
  return(ls)
}#  ll = hist.XbyY(); wbarplot(unlapply(ll, length))



# # deprecated :
# NrAndPc <- function(logical_vec = idx_localised, total = TRUE, NArm = TRUE) { # Summary stat. text formatting for logical vectors (%, length)
#   x = paste0(pc_TRUE(logical_vec), " or ", sum(logical_vec, na.rm = NArm))
#   if (total) paste0(x, " of ", length(logical_vec))
# }
