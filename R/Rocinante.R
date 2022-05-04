######################################################################
# Rocinante - A collection of custom R functions. Helper functions complementing CodeAndRoll2.
######################################################################
# source('~/GitHub/Packages/Rocinante/R/Rocinante.R')
# rm(list = ls(all.names = TRUE)); try(dev.off(), silent = T)

# Functions ------------------------
# require('CodeAndRoll2')
# require('MarkdownReports')
# source('~/Github/TheCorvinas/R/DatabaseLinke.r')


# try(source('~/GitHub/Packages/gruffi/R/AddGOGeneList.manual.R'), silent = T)
# try(source('~/GitHub/Packages/gruffi/R/IntersectWithExpressed.R'), silent = T)

# Search query links ------------------------------------------------------------------------
try(library(DatabaseLinke.R, include.only = c('qHGNC','link_google', 'link_bing', 'openURLs.1by1')) , silent = T)



# Setup ------------------------
# pdf.options(title = paste0('Copyright Abel Vertesy ', Sys.Date())) # Setup to your own name
debuggingState(on = FALSE)
# "gtools", "readr", "gdata", "colorRamps", "grDevices", "plyr"
print("Depends on CodeAndRoll2, MarkdownReports, gtools, readr, gdata, clipr. Some functions depend on other libraries.")


# Params ------------------------
wA4 = 8.27 # A4 inches
hA4 = 11.69


# Alisases ----------------
sort.natural = gtools::mixedsort
p0 = paste0
l = length
toclip = clipr::write_clip
fromclip = clipr::read_clip

stry <- function(...) {try(..., silent = T)} # Silent try


load.gruffi <-  'devtools::load_all(path = "~/GitHub/Packages/gruffi/")'
load.UVI.tools = 'devtools::load_all(path = "~/GitHub/Packages/UVI.tools/")'


# ------------------------
sourcePartial <- function(fn,startTag = '#1', endTag = '#/1') { # Source parts of another script. Source: https://stackoverflow.com/questions/26245554/execute-a-set-of-lines-from-another-r-file
  lines <- scan(fn, what = character(), sep = "\n", quiet = TRUE)
  st <- grep(startTag,lines)
  en <- grep(endTag,lines)
  tc <- textConnection(lines[(st + 1):(en - 1)])
  source(tc)
  close(tc)
}

# ------------------------
# ------------------------
### Distance and correlation calculations --------------
eucl.dist.pairwise <- function(df2col) { # Calculate pairwise euclidean distance
  dist_ = abs(df2col[,1] - df2col[,2]) / sqrt(2)
  if (!is.null(rownames(df2col)))   names(dist_) = rownames(df2col)
  dist_
}

sign.dist.pairwise <- function(df2col) { # Calculate absolute value of the pairwise euclidean distance
  dist_ = abs(df2col[,1] - df2col[,2]) / sqrt(2)
  if (!is.null(rownames(df2col)))   names(dist_) = rownames(df2col)
  dist_
}

# Auto correlation functions
rowACF <- function(x, na_pass = na.pass, plot = FALSE, ...) { apply(x, 1, acf, na.action = na_pass,  plot = plot, ...)} # RETURNS A LIST. Calculates the autocorrelation of each row of a numeric matrix / data frame.
colACF <- function(x, na_pass = na.pass, plot = FALSE, ...) { apply(x, 2, acf, na.action = na_pass,  plot = plot, ...)} # RETURNS A LIST. Calculates the autocorrelation of each row of a numeric matrix / data frame.

acf.exactLag <- function(x, lag = 1, na_pass = na.pass, plot = FALSE, ... ) { # Autocorrelation with exact lag
  x = acf(x, na.action = na_pass,  plot = plot, ...)
  x[['acf']][(lag + 1)]
}

rowACF.exactLag <- function(x, na_pass = na.pass, lag = 1, plot = FALSE, ...) { # RETURNS A Vector for the "lag" based autocorrelation. Calculates the autocorrelation of each row of a numeric matrix / data frame.
  signif(apply(x, 1, acf.exactLag, lag = lag, plot = plot, ...), digits = 2)
}

colACF.exactLag <- function(x, na_pass = na.pass, lag = 1, plot = FALSE, ...) { # RETURNS A Vector for the "lag" based autocorrelation. Calculates the autocorrelation of each row of a numeric matrix / data frame.
  signif(apply(x, 2, acf.exactLag, lag = lag, plot = plot, ...), digits = 2)
}

# Clipboard interaction -------------------------------------------------------------------------------------------------
# https://github.com/vertesy/DataInCode
# try(source("~/Github/TheCorvinas/R/DataInCode/DataInCode.R"), silent = FALSE)

clip2clip.vector <- function() { # Copy from clipboard (e.g. excel) to a R-formatted vector to the  clipboard
  x = dput(clipr::read_clip() )
  clipr::write_clip(
    utils::capture.output(x)
  )
  print(x)
}


clip2clip.commaSepString <- function() { # Read a comma separated string (e.g. list of gene names) and properly format it for R.
  x = unlist(strsplit(clipr::read_clip(), split = ','))
  clipr::write_clip(
    utils::capture.output(x)
  )
  print(x)
}

write_clip.replace.dot.with.comma <- function(var = df.markers, decimal_mark = ',') { # Clipboard export for da wonderful countries where "," is the decimal...
  write_clip(format(var, decimal.mark = decimal_mark) )
}
# write_clip.replace.dot.with.comma(df_markers)


# Else -------------------------------------------------------------------------------------------------
view.head <- function(matrix, enn = 10) { matrix[1:min(NROW(matrix), enn), 1:min(NCOL(matrix), enn)] } # view the head of an object by console.
view.head2 <- function(matrix, enn = 10) { View(head(matrix, n = min(NROW(matrix), NCOL(matrix), enn))) } # view the head of an object by View().

iidentical.names <- function(v1, v2) { # Test if names of two objects for being exactly equal
  nv1 = names(v1)
  nv2 = names(v2)
  len.eq = (length(nv1) == length(nv2))
  if (!len.eq) iprint("Lenghts differ by:", (length(nv1) - length(nv2)) )
  Check = identical(nv1, nv2)
  if (!Check) {
    diff = setdiff(nv1, nv2)
    ld = length(diff)
    iprint(ld, "elements differ: ", head(diff))
  }
  Check
}

iidentical <- function(v1, v2) { # Test if two objects for being exactly equal
  len.eq = (length(v1) == length(v2))
  if (!len.eq) iprint("Lenghts differ by:", (length(v1) - length(v2)) )
  Check = identical(v1,v2)
  if (!Check) {
    diff = setdiff(v1, v2)
    ld = length(diff)
    iprint(ld, "elements differ: ", head(diff))
  }
  Check
}

iidentical.all <- function(li) all(sapply(li, identical, li[[1]])) # Test if two objects for being exactly equal.

#' IfExistsAndTrue
#'
#' Internal function. Checks if a variable is defined, and its value is TRUE.
#' @param name Name of the varaible
#' @export
#' @examples IfExistsAndTrue()

IfExistsAndTrue <- function(name = "pi" ) { # Internal function. Checks if a variable is defined, and its value is TRUE.
  x = FALSE
  if (exists(name)) {
    if (isTRUE(get(name)))  {x = TRUE} else {x = FALSE; iprint(name, " exists, but != TRUE; ", get(name))}
  }
  return(x)
}

memory.biggest.objects <- function(n = 5, saveplot = F) { # Show distribution of the largest objects and return their names. # https://stackoverflow.com/questions/17218404/should-i-get-a-habit-of-removing-unused-variables-in-r
  try.dev.off()
  gc()
  ls.mem <- ls( envir = .GlobalEnv)
  ls.obj <- lapply(ls.mem, get)
  Sizes.of.objects.in.mem <- unlapply(ls.obj, object.size)
  names(Sizes.of.objects.in.mem) <- ls.mem
  topX = sort(Sizes.of.objects.in.mem,decreasing = TRUE)[1:n]

  Memorty.usage.stat = c(topX, 'Other' = sum(sort(Sizes.of.objects.in.mem,decreasing = TRUE)[-(1:n)]))
  pie(Memorty.usage.stat, cex = .5, sub = make.names(date()), col = grDevices::terrain.colors(l(Memorty.usage.stat)))
  # try(ggExpress::qpie(Memorty.usage.stat, w = 7,  ), silent = T)
  # Use wpie if you have MarkdownReports, from https://github.com/vertesy/MarkdownReports
  dput(names(topX))
  iprint("rm(list = c( 'objectA',  'objectB'))")
  # inline_vec.char(names(topX))
  # Use inline_vec.char if you have DataInCode, from https://github.com/vertesy/DataInCode
}
# memory.biggest.objects()





# Biology ------------------------------------------------------------

GC_content <- function(string, len = nchar(string), pattern = c("G","C")) { # GC-content of a string (frequency of G and C letters among all letters).
  char.list <- stringr::str_split_fixed(string, pattern = "", n = nchar(string))
  tbl = table(factor(unlist(char.list), levels = c("A", "T", "G", "C")))
  sum(tbl[  pattern ]) / sum(tbl)
}


# Generic ------------------------
printEveryN <- function(i, N = 1000) { if ((i %% N) == 0 ) iprint(i) } # Report at every e.g. 1000


'%!in%' <- function(x,y)!('%in%'(x,y))

stopif2 <- function(condition, ...) { if (condition) {iprint(...); stop()} } # Stop script if the condition is met. You can parse anything (e.g. variables) in the message


say <- function(...) { # Use system voice to notify (after a long task is done)
  sys <- Sys.info()["sysname"]
  if (sys == "Darwin") system("say -v Samantha Ready!")
  if (sys == "Linux") system("echo -e '\a'; sleep 0.5s; echo -e '\a'; sleep 0.5s; echo -e '\a'; sleep 0.5s; echo -e '\a'; sleep 0.5s; echo -e '\a'; sleep 0.5s; echo -e '\a'")  # For UNIX servers.
}
sayy <- function(...) {system("say -v Samantha 'Ready to roll!'")} # Use system voice to notify (after a long task is done)

oo <- function() { # Open current working directory.
  system("open .")
}

# detach_package <-
unload <- function(pkg, character.only = FALSE) { # Unload a package. Source: https://stackoverflow.com/questions/6979917/how-to-unload-a-package-without-restarting-r
  if (!character.only)
  {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while (search_item %in% search())
  {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}


## Plotting and Graphics -----------------------------------------------------------------------------------------------------

legend.col <- function(col, lev) { # Legend color. # Source: https://aurelienmadouasse.wordpress.com/2012/01/13/legend-for-a-continuous-color-scale-in-r/
  opar <- par
  n <- length(col)
  bx <- par("usr")
  box.cx <- c(bx[2] + (bx[2] - bx[1]) / 1000,
              bx[2] + (bx[2] - bx[1]) / 1000 + (bx[2] - bx[1]) / 50)
  box.cy <- c(bx[3], bx[3])
  box.sy <- (bx[4] - bx[3]) / n
  xx <- rep(box.cx, each = 2)

  par(xpd = TRUE)
  for (i in 1:n) {
    yy <- c(box.cy[1] + (box.sy * (i - 1)),
            box.cy[1] + (box.sy * (i)),
            box.cy[1] + (box.sy * (i)),
            box.cy[1] + (box.sy * (i - 1)))
    polygon(xx, yy, col = col[i], border = col[i])
  }
  par(new = TRUE)
  plot(0, 0, type = "n",
       ylim = c(min(lev), max(lev)),
       yaxt = "n", ylab = "",
       xaxt = "n", xlab = "",
       frame.plot = FALSE)
  axis(side = 4, las = 2, tick = FALSE, line = .25)
  par <- opar
  par(xpd = FALSE)
  # print("You might need to set par('mar' = c( 5.1, 4.1, 4.1, 2.1)) to higher values.")
}


### Colors -----------------------------------------------------------------------------------------------------
richColors <- function(n = 3) { gplots::rich.colors(n) } # Alias for rich.colors in gplots


Color_Check <- function(..., incrBottMarginBy = 0, savefile = FALSE ) { # Display the colors encoded by the numbers / color-ID-s you pass on to this function
  if (incrBottMarginBy) { .ParMarDefault <- par("mar");   par(mar = c(par("mar")[1] + incrBottMarginBy, par("mar")[2:4]) ) }  # Tune the margin
  Numbers = c(...)
  if (length(names(Numbers)) == length(Numbers)) {labelz = names(Numbers)} else {labelz = Numbers}
  barplot(rep(10, length(Numbers)), col = Numbers, names.arg = labelz, las = 2 )
  if (incrBottMarginBy) { par("mar" = .ParMarDefault )}

  fname = substitute(...)
  if (savefile) { dev.copy2pdf(file = ww.FnP_parser(fname, "ColorCheck.pdf")) }
}

HeatMapCol_BGR <- grDevices::colorRampPalette(c("blue", "cyan", "yellow", "red"), bias = 1)
# HeatMapCol_BWR <- grDevices::colorRampPalette(c("blue", "white", "red"), bias = 1)
HeatMapCol_RedBlackGreen <- grDevices::colorRampPalette(c("red", "black", "green"), bias = 1)


colSums.barplot <- function(df, col = "seagreen2", na_rm = TRUE, ...) { barplot(colSums(df, na.rm = na_rm), col = col, ...) } # Draw a barplot from ColSums of a matrix.

lm_equation_formatter <- function(lm) { # Renders the lm() function's output into a human readable text. (e.g. for subtitles)
  eq = signif(lm$coefficients);
  kollapse("Intercept: ", eq[1], " Slope: ", eq[2]);
}

lm_equation_formatter2 <- function(lm) { # Renders the lm() function's output into a human readable text. (e.g. for subtitles)
  eq = signif(lm$coefficients, digits = 3);
  kollapse("y = ", eq[2], "* x + ", eq[1]);
}

lm_equation_formatter3 <- function(lm, y.var.name = "y", x.var.name = "x") { # Renders the lm() function's output into a human readable text. (e.g. for subtitles)
  eq = signif(lm$coefficients, digits = 3);
  plusSign = if (sign(eq[1] == 1)) "" else "-"
  kollapse(y.var.name, " = ", eq[2], "*",x.var.name," ",plusSign,"", eq[1]);
}


### Functions for pairs() plots  -----------------------------------------------------------------------------------------------------
panel.cor.pearson <- function(x, y, digits = 2, prefix = "", cex.cor = 2, method = "pearson") { # A function to display correlation values for pairs() function. Default is pearson correlation, that can be set to  "kendall" or "spearman".
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, method = method, use = "complete.obs"))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste(prefix, txt, sep = "")
  if (missing(cex.cor)) cex <- 0.8/strwidth(txt)

  test <- cor.test(x, y)
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " "))

  text(0.5, 0.5, txt, cex = cex * r)
  text(.8, .8, Signif, cex = cex,  col = 2)
}

panel.cor.spearman <- function(x, y, digits = 2, prefix = "", cex.cor = 2, method = "spearman") { # A function to display correlation values for pairs() function. Default is pearson correlation, that can be set to  "kendall" or "spearman".
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, method = method, use = "complete.obs"))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste(prefix, txt, sep = "")
  if (missing(cex.cor)) cex <- 0.8/strwidth(txt)

  test <- cor.test(x, y)
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " "))

  text(0.5, 0.5, txt, cex = cex * r)
  text(.8, .8, Signif, cex = cex, col = 2)
}


quantile_breaks <- function(xs, n = 10, na.Rm = FALSE) { # Quantile breakpoints in any data vector http://slowkow.com/notes/heatmap-tutorial/
  breaks <- quantile(xs, probs = seq(0, 1, length.out = n), na.rm = na.Rm)
  breaks[!duplicated(breaks)]
}




## Clustering heatmap tools -----------------------------------------------------------------------------------------------------

hclust.getOrder.row <- function(pheatmapObject) pheatmapObject$tree_row$labels[pheatmapObject$tree_row$order] # Extract ROW order from a pheatmap object.
hclust.getOrder.col <- function(pheatmapObject) pheatmapObject$tree_col$labels[pheatmapObject$tree_col$order] # Extract COLUMN order from a pheatmap object.

hclust.getClusterID.row <- function(pheatmapObject, k = 3) cutree(pheatmapObject$tree_row, k = k) # Extract cluster ID's for ROWS of a pheatmap object.
hclust.getClusterID.col <- function(pheatmapObject, k = 3) cutree(pheatmapObject$tree_col, k = k) # Extract cluster ID's for COLUMNS of a pheatmap object.

hclust.ClusterSeparatingLines.row <- function(pheatmapObject, k = 3) which(!duplicated(cutree(pheatmapObject$tree_row, k = k)[pheatmapObject$tree_row$order])[-1]) # Calculate the position of ROW separating lines between clusters in a pheatmap object.
hclust.ClusterSeparatingLines.col <- function(pheatmapObject, k = 3) which(!duplicated(cutree(pheatmapObject$tree_col, k = k)[pheatmapObject$tree_col$order])[-1]) # Calculate the position of COLUMN separating lines between clusters in a pheatmap object.

Gap.Postions.calc.pheatmap <- function(annot.vec.of.categories) { # calculate gap positions for pheatmap, based a sorted annotation vector of categories
  NAZ = sum(is.na(annot.vec.of.categories))
  if (NAZ) iprint("There are", NAZ, "NA values in your vector. They should be last and they are omitted.")
  consecutive.lengthes = rle( na.omit.strip(annot.vec.of.categories))$lengths
  cumsum(consecutive.lengthes) # return abs.positions
}

matlabColors.pheatmap <- function(matrixx, nr = 50) {colorRamps::matlab.like(length(quantile_breaks(matrixx, n = nr)) - 1)} # Create a Matlab-like color gradient using "colorRamps".

"FOR VECTOR. it works"
annot_col.create.pheatmap.vec <- function(data, annot_vec, annot_names = "Annot") { # For VECTORS. Auxiliary function for pheatmap. Prepares the 2 variables needed for "annotation_col" and "annotation_colors" in pheatmap
  stopifnot( length(annot_vec) == dim(data)[2] )
  namez = as.character(if (is.null(annot_names)) substitute(annot_vec) else annot_names)

  df = data.frame(x = annot_vec); df[, 1] = as.character(df[, 1])
  names(df) = namez # colnames but more flexible
  rownames(df) = colnames(data)
  assign(x = "annot", value = df, envir = .GlobalEnv)

  tt = table(annot_vec); nz = names(tt)
  if (is.numeric(annot_vec)) {
    coll = val2col(annot_vec[!duplicated(annot_vec)]); names(coll) = nz
  } else {
    coll = gplots::rich.colors(length(tt)); names(coll) = nz
  }
  col_list = list(annot_vec = coll)
  names(col_list) = namez
  assign(x = "annot_col", value = col_list, envir = .GlobalEnv)

  print("annot [data frame] and annot_col [list] variables are created. Use: pheatmap(..., annotation_col = annot, annotation_colors = annot_col)")
}


annot_col.create.pheatmap.df <- function(data, annot_df_per_column, annot_names = NULL) { # For data frames. Auxiliary function for pheatmap. Prepares the 2 variables needed for "annotation_col" and "annotation_colors" in pheatmap
  stopif( dim(annot_df_per_column)[1] != dim(data)[2] , message = "The number of rows in the annotation data != to the # columns in your data frame")

  df = as.data.frame(annot_df_per_column)
  if (any(rownames(df) != colnames(data))) { print("The rownames of annot_df_per_column are not the same as the colnames of data:")
    print(cbind("rownames(df)" = rownames(df) , "colnames(data)" = colnames(data))) }
  namez = as.character(if (is.null(annot_names)) colnames(annot_df_per_column) else annot_names)

  colnames(df) = namez
  rownames(df) = colnames(data)
  assign(x = "annot", value = df, envir = .GlobalEnv)

  col_list = list.fromNames(namez)
  for (i in 1:NCOL(df) ) {
    annot_column_i = df[, i]
    tt = table(annot_column_i); nz = names(tt)
    coll = if (is.numeric(annot_column_i)) { val2col(unique(annot_column_i));
    } else { gplots::rich.colors(length(tt)) }
    names(coll) = sort(nz)
    col_list[[i]] = coll
  } #for each column
  assign(x = "annot_col", value = col_list, envir = .GlobalEnv)

  print("annot [data frame] and annot_col [list] variables are created. Use: pheatmap(..., annotation_col = annot, annotation_colors = annot_col)")
}

annot_col.fix.numeric <- function(ListOfColnames, df.annot = annot, annot_col = annot_col) { # fix class and color annotation in pheatmap annotation data frame's and lists.
  for (i in 1:length(ListOfColnames) ) {
    j = ListOfColnames[i]
    annot[[j]] = as.numeric(annot[[j]])
    annot_col[[j]] = NULL # remove fixed colors -> auto determine by pheatmap
  } #for
  assign(x = "annot_col", value = annot_col, envir = .GlobalEnv)
  iprint("Columns in annot are as.numeric(), list elements in annot_col are removed")
}


annot_row.create.pheatmap.df <- function(data, annot_df_per_row, annot_names = NULL) { # For data frames. Auxiliary function for pheatmap. Prepares the 2 variables needed for "annotation_col" and "annotation_colors" in pheatmap
  stopif( dim(annot_df_per_row)[1] != dim(data)[1] , message = "The number of rows in the annotation data != to the # columns in your data frame")

  df = as.data.frame(annot_df_per_row)
  if (any(rownames(df) != rownames(data))) { print("The rownames of annot_df_per_row are not the same as the rownames of data:")
    print(cbind("rownames(df)" = rownames(df) , "rownames(data)" = rownames(data))) }
  namez = as.character(if (is.null(annot_names)) colnames(annot_df_per_row) else annot_names)

  colnames(df) = namez
  rownames(df) = rownames(data)
  assign(x = "annot_rows", value = df, envir = .GlobalEnv)

  col_list = list.fromNames(namez)
  for (i in 1:NCOL(df) ) {
    annot_column_i = df[, i]
    tt = table(annot_column_i); nz = names(tt)
    coll = if (is.numeric(annot_column_i)) { val2col(unique(annot_column_i));
    } else { gplots::rich.colors(length(tt)) }
    names(coll) = sort(nz)
    col_list[[i]] = coll
  } #for each column
  assign(x = "annot_rows.col", value = col_list, envir = .GlobalEnv)

  print("annot_rows [data frame] and annot_rows.col [list] variables are created. Use: pheatmap(..., annotation_row = annot_rows, annotation_colors = annot_rows.col)")
}




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



# TMP ------------------------------------------------------------------------------------------------


sourceGitHub <- function(script = "Cell.cycle.scoring.R"
                         , repo = "Seurat.Pipeline"
                         , folder = "elements"
                         , user = "vertesy"
                         , rawpath = "https://raw.githubusercontent.com"
                         , suffix = "master"
                         , token = NULL, ...) { # Source from GitHub. Example https://raw.githubusercontent.com/vertesy/Seurat.Pipeline/main/elements/Cell.cycle.scoring.R
  path.part = FixPath(kpps(user, repo, suffix, folder, script))
  fullpath = RemoveFinalSlash(kpps(rawpath, path.part))
  if (!is.null(token)) fullpath = paste0(fullpath, token)
  print(fullpath)
  source(fullpath)
}



backup <- function(obj) { # make a backup of an object into global env. Scheme: obj > obj.bac
  varname <- as.character(substitute(obj))
  bac.varname <- ppp(varname, "bac")
  if (exists(bac.varname)) {
    print(" Backup already exists.")
  } else {
    iprint(varname, "is backep up into:", bac.varname)
    assign(x = bac.varname, value = obj, envir= as.environment(1) )
  }
}
# backup(combined.obj)



list.dirs.depth.n <- function(dir = '.' , depth = 2) { # list dirs recursive up to a certain level in R https://stackoverflow.com/questions/48297440/list-files-recursive-up-to-a-certain-level-in-r
  iprint("Scanning directories. Depth:", depth)
  res <- list.dirs(dir, recursive = FALSE)
  if (depth > 1) {
    add <- list.dirs.depth.n(res, depth - 1)
    c(res, add)
  } else {
    res
  }
}
# list.dirs.depth.n(depth = 3)



### Copy
# -------------------------------------------------------------------------------------------------------------




ssh2osX <- function(shellpath=clipr::read_clip()) { # '/groups/knoblich/users/burkard/Abel.Vertesy/R12357/R12357_merged_20211108093511/README.html'
  newpath <- gsub(x = shellpath, pattern = '/groups/', replacement = 'smb://storage.imp.ac.at/groups/')
  clipr::write_clip(newpath)
}

osX2ssh <- function(shellpath=clipr::read_clip()) { # '/groups/knoblich/users/burkard/Abel.Vertesy/R12357/R12357_merged_20211108093511/README.html'
  newpath <- gsub(x = shellpath, replacement = '/groups/',  pattern= 'smb://storage.imp.ac.at/groups/')
  clipr::write_clip(newpath)
}





# TMP TMP TMP TMP -------------------------------------------------------------------------------------------------------------


q32vA4_grid_plot <- function(plot_list, plotname = F, suffix = NULL, plot =F
                             , nrow = 3, ncol = 2, extension = c('pdf', 'png')[2]
                             , h = hA4 * scale, w = wA4 * scale, scale = 1
                             , ...) { # Save 4 umaps on an A4 page.
  print("Plot panels on 3-by-2 vertical A4 page.")
  stopifnot(length(plot_list)<7)

  if (plotname==F) plotname =  sppp(substitute(plot_list), suffix)
  fname = kpp(plotname, extension)
  p1 = cowplot::plot_grid(plotlist = plot_list, nrow = nrow, ncol = ncol, labels = LETTERS[1:length(plot_list)], ...  )
  cowplot::save_plot(plot = p1, filename = fname, base_height = h, base_width = w)
  ww.FnP_parser(fname)
}



# NEW fun ------------------------------


# _________________________________________________________________________________________________
#' STRINGdb.reformat.ann.table.per.gene  > Databaselnker
#'
#' @param path_of_tsv input file
#' @param column column name
#' @param sep value separation
#' @export
#' @examples

STRINGdb.reformat.ann.table.per.gene <- function(path_of_tsv = '/Users/abel.vertesy/Downloads/enrichment.DISEASES.tsv'
                                                 , column = 'matching proteins in your network (labels)'
                                                 , sep = ',') {

  annotation_tsv <- CodeAndRoll2::read.simple.tsv(path_of_tsv)
  stopifnot(column %in% colnames(annotation_tsv))
  (tbl_split <- tidyr::separate_rows(data = annotation_tsv, column, sep = sep ))
  CodeAndRoll2::write.simple.tsv(tbl_split, ManualName = paste(path_of_tsv, "per.gene.tsv", sep = ".") )
  return(tbl_split)
}




# _________________________________________________________________________________________________
#' link_SNPedia_clip2clip
#'
#' @param rdIDs  Should be row-by-row list of  rsID's from an Excel column
#' @param searchQueryPrefix snpedia search query link base
#' @param as.MarkDownLink  return as Excel link, Def: TRUE
#' @param as.MarkDownLink  return as Markdown link, Def: FALSE
#' @export
#' @examples link_SNPedia_clip2clip(rdIDs = clipr::read_clip_tbl( header=F)

link_SNPedia_clip2clip <- function(rdIDs = clipr::read_clip_tbl( header=F)
                                   , searchQueryPrefix = 'https://www.snpedia.com/index.php/'
                                   , as.ExcelLink = T
                                   , as.MarkDownLink = F
) {

  v.rdIDs <- tibble::deframe(rdIDs)
  links <- paste0(searchQueryPrefix, v.rdIDs)
  tbl_link <- as.tibble(links)
  print(head(tbl_link))
  colnames(tbl_link) <- NULL
  if (as.ExcelLink) {
    tbl_link <- FormatAsExcelLink(site_name = v.rdIDs, site_url = links)
    print("Now paste into to Execl, or google sheets")
  } else  if (as.MarkDownLink) {
    tbl_link <- paste0('[', v.rdIDs , '](', links , ')')
    print("Now  paste into to typora (then text edit, then Execl, then google docs)")
  }

  clipr::write_clip(tbl_link)

}
# link_SNPedia_clip2clip()



# _________________________________________________________________________________________________
#' link_Franklin_clip2clip > Databaselnker
#'
#' @param coordinates Coordinates in input format 5:35162876	C/T  OR 16:7164219	T/G
#' @param searchQueryPrefix Genoox Franklin search query link base
#' @param as.ExcelLink  return as Excel link, Def: TRUE
#' @export
#' @examples link_Franklin_clip2clip(coordinates = clipr::read_clip_tbl( header=F) )

link_Franklin_clip2clip <- function(coordinates = clipr::read_clip_tbl( header=F)
                                    , searchQueryPrefix = 'https://franklin.genoox.com/clinical-db/variant/snp/'
                                    , as.ExcelLink = T
) {
  stopifnot(ncol(coordinates) == 2)
  Coord <-
    if (idim(coordinates)[2]==2) {
      coordinates <- paste(coordinates[,1], coordinates[,2], sep = ":")
    } else  tibble::deframe(coordinates)

  Coord.Formattes <- paste0('chr', gsub(x = Coord, pattern = ':', replacement = '-'))
  Coord.Formattes <- gsub(x = Coord.Formattes, pattern = '/', replacement = '-')

  links <- paste0(searchQueryPrefix, Coord.Formattes)
  tbl_link <- as.tibble(links)
  print(head(tbl_link))
  colnames(tbl_link) <- NULL
  if (as.ExcelLink) {
    tbl_link <- FormatAsExcelLink(site_name = tibble::deframe(Coord.Formattes), site_url = links)
    print("Now paste into to Execl, or google sheets")
  }
  clipr::write_clip(tbl_link)

}
# link_Franklin_clip2clip()


# _________________________________________________________________________________________________
#' link_VarSome_clip2clip
#'
#' @param rdIDs  Should be row-by-row list of  rsID's from an Excel column
#' @param searchQueryPrefix Varsome search query link base
#' @param as.MarkDownLink  return as Excel link, Def: TRUE
#' @param as.MarkDownLink  return as Markdown link, Def: FALSE
#' @export
#' @examples link_VarSome_clip2clip(rdIDs = clipr::read_clip_tbl( header=F) # "https://varsome.com/variant/hg38/rs12970134?annotation-mode=germline"

link_VarSome_clip2clip <- function(rdIDs = clipr::read_clip_tbl( header=F)
                                   , searchQueryPrefix = 'https://varsome.com/variant/'
                                   , hg = "hg19"
                                   , suffix = "?annotation-mode=germline"
                                   , as.ExcelLink = T
                                   , as.MarkDownLink = F
) {

  "https://varsome.com/variant/hg38/rs12970134?annotation-mode=germline"
  prefix_total = paste0(searchQueryPrefix, hg, "/")

  v.rdIDs <- tibble::deframe(rdIDs)
  links <- paste0(prefix_total, v.rdIDs, suffix)
  tbl_link <- as.tibble(links)
  print(head(tbl_link))
  colnames(tbl_link) <- NULL
  if (as.ExcelLink) {
    tbl_link <- FormatAsExcelLink(site_name = paste('VS', v.rdIDs)
                                  , site_url = links)
    print("Now paste into to Execl, or google sheets")
  } else  if (as.MarkDownLink) {
    tbl_link <- paste0('[', v.rdIDs , '](', links , ')')
    print("Now  paste into to typora (then text edit, then Execl, then google docs)")
  }

  clipr::write_clip(tbl_link)

}
# link_VarSome_clip2clip()


# _________________________________________________________________________________________________

#
# qHGNC <- function(vector_of_gene_symbols # Parse HGNC links to your list of gene symbols.
#                   , writeOut = FALSE, Open = TRUE, HGNC_symbol_search = "http://www.genenames.org/cgi-bin/gene_search?search=") {
#   links = paste0(HGNC_symbol_search, vector_of_gene_symbols)
#   if (writeOut) {
#     bash_commands = paste0("open ", links)
#     ReadWriter::write.simple.append("", ManualName = BashScriptLocation)
#     ReadWriter::write.simple.append(bash_commands, ManualName = BashScriptLocation)
#   } else if (Open) { openURLs.1by1(links) } else { return(links) }
# }

