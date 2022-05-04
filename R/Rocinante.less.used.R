######################################################################
# Rocinante - LESS USED FUNCTIONS
######################################################################
# source('~/GitHub/Packages/Rocinante/R/Rocinante.less.used.R')
# rm(list = ls(all.names = TRUE)); try(dev.off(), silent = T)




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
