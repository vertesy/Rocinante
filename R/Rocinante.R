# ____________________________________________________________________
# Rocinante - A collection of custom R functions. Helper functions complementing CodeAndRoll2.
# ____________________________________________________________________
# source('~/GitHub/Packages/Rocinante/R/Rocinante.R')
# file.edit('~/GitHub/Packages/Rocinante/R/Rocinante.less.used.R')
# rm(list = ls(all.names = TRUE)); try(dev.off(), silent = TRUE)
# source('~/.pack.R')

print("Loading Rocinante custom function library.")
# Search query links _______________________________________________________________
try(library(DatabaseLinke.R, include.only = c('qHGNC','link_google', 'link_bing', 'openURLs.1by1')) , silent = TRUE)



# Setup _______________________________________________________________
# pdf.options(title = paste0('Copyright Abel Vertesy ', Sys.Date())) # Setup to your own name
debuggingState(on = FALSE)
print("Depends on CodeAndRoll2, MarkdownReports, gtools, readr, gdata, clipr. Some functions depend on other libraries.")

# Params _______________________________________________________________
wA4 = 8.27 # A4 inches
hA4 = 11.69


get_col <- function(mat, col_idx) as_tibble(mat) |> pull(col_idx)
get_row <- function(mat, row_idx) as_tibble(mat) |> slice(row_idx)
get_subvec <- function(vec, range) vec[range]

# Alisases ____________________________________________________________ ----
sort.natural = gtools::mixedsort
comma = scales::comma

u <- unique
b <- browser
p0 <- paste0
l <- length
toclip <- clipr::write_clip
fromclip <- clipr::read_clip

stry <- function(...) {try(..., silent = TRUE)} # Silent try

warnings.erase <- function() assign("last.warning", NULL, envir = baseenv())

rprofile <-  function(...) file.edit('~/.Rprofile')
rocinanteSource <- function() source('~/GitHub/Packages/Rocinante/R/Rocinante.R')



# Package Loaders ____________________________________________________________ ----

o <- pOpen <- list(
  Rocinante =          function(...) file.edit('~/GitHub/Packages/Rocinante/R/Rocinante.R'),
  Stringendo =         function(...) file.edit('~/GitHub/Packages/Stringendo/R/Stringendo.R'),
  CodeAndRoll2 =       function(...) file.edit('~/GitHub/Packages/CodeAndRoll2/R/CodeAndRoll2.R'),
  ReadWriter =         function(...) file.edit('~/GitHub/Packages/ReadWriter/R/ReadWriter.R'),

  PackageTools =       function(...) file.edit('~/GitHub/Packages/PackageTools/R/PackageTools.R'),
    PackageToolsREPL =       function(...) file.edit('~/GitHub/Packages/PackageTools/R/ReplacementTools.R'),
    PackageToolsDOC =       function(...) file.edit('~/GitHub/Packages/PackageTools/R/DocumentationTools.R'),
    PackageToolsDEP =       function(...) file.edit('~/GitHub/Packages/PackageTools/R/DependencyTools.R'),
    PackageToolsMISC =       function(...) file.edit('~/GitHub/Packages/PackageTools/R/Miscellaneous.R'),
    PackageToolsROXY =       function(...) file.edit('~/GitHub/Packages/PackageTools/R/RoxygenTools.R'),

  MarkdownHelpers =    function(...) file.edit('~/GitHub/Packages/MarkdownHelpers/R/MarkdownHelpers.R'),
  MarkdownReports =    function(...) file.edit('~/GitHub/Packages/MarkdownReports/R/MarkdownReports.R'),
  ggExpress =          function(...) file.edit('~/GitHub/Packages/ggExpress/R/ggExpress.R'),

  SeuratUtils =        function(...) file.edit('~/GitHub/Packages/Seurat.utils/R/Seurat.Utils.R'),
    SeuratUtils_META =        function(...) file.edit('~/GitHub/Packages/Seurat.utils/R/Seurat.Utils.Metadata.R'),
    SeuratUtils_VIZ =        function(...) file.edit('~/GitHub/Packages/Seurat.utils/R/Seurat.Utils.Visualization.R'),
  isoENV =             function(...) file.edit('~/GitHub/Packages/isoENV/R/isoENV.R'),
  isoENV.other =       function(...) file.edit('~/GitHub/Packages/isoENV/R/isoENV.other.R'),

  UVITools =           function(...) file.edit('~/GitHub/Packages/UVI.tools/R/UVI.tools.R'),
    UVIToolsBulk =       function(...) file.edit('~/GitHub/Packages/UVI.tools/R/UVI.tools.Bulk.R'),
  ConnectomeTools =    function(...) file.edit('~/GitHub/Packages/Connectome.tools/R/Connectome.tools.R'),
    ConnectomeToolsAAV = function(...) file.edit('~/GitHub/Packages/Connectome.tools/R/Connectome.tools.AAV.R'),
  NestedMultiplexer =  function(...) file.edit('~/GitHub/Packages/NestedMultiplexer/R/NestedMultiplexer.R'),

  DatabaseLinke.R =          function(...) file.edit('~/GitHub/Packages/DatabaseLinke.R/R/DatabaseLinke.R'),
  # gruffiDev =          function(...) file.edit('~/GitHub/Packages/gruffiDev/R/gruffi.R'),
  gruffi =             function(...) file.edit('~/GitHub/Packages/gruffi/R/gruffi.R')
)

d <- pDocAndLoad <- list(
  Stringendo =       function(..., path = "~/GitHub/Packages/Stringendo") { devtools::document(path); devtools::load_all(path) },
  ReadWriter =       function(..., path = "~/GitHub/Packages/ReadWriter") { devtools::document(path); devtools::load_all(path) },
  CodeAndRoll2 =     function(..., path = "~/GitHub/Packages/CodeAndRoll2") { devtools::document(path); devtools::load_all(path) },
  ReadWriter =       function(..., path = "~/GitHub/Packages/ReadWriter") { devtools::document(path); devtools::load_all(path) },
  PackageTools =     function(..., path = "~/GitHub/Packages/PackageTools") { devtools::document(path); devtools::load_all(path) },

  MarkdownHelpers =  function(..., path = "~/GitHub/Packages/MarkdownHelpers") { devtools::document(path); devtools::load_all(path) },
  MarkdownReports =  function(..., path = "~/GitHub/Packages/MarkdownReports") { devtools::document(path); devtools::load_all(path) },
  ggExpress =        function(..., path = "~/GitHub/Packages/ggExpress") { devtools::document(path); devtools::load_all(path) },

  Seurat.utils =     function(..., path = "~/GitHub/Packages/Seurat.utils") { devtools::document(path); devtools::load_all(path) },
  isoENV =           function(..., path = "~/GitHub/Packages/isoENV") { devtools::document(path); devtools::load_all(path) },

  UVI.tools =         function(..., path = "~/GitHub/Packages/UVI.tools") { devtools::document(path); devtools::load_all(path) },
  Connectome.tools =  function(..., path = "~/GitHub/Packages/Connectome.tools") { devtools::document(path); devtools::load_all(path) },
  NestedMultiplexer = function(..., path = "~/GitHub/Packages/NestedMultiplexer") { devtools::document(path); devtools::load_all(path) },

  DatabaseLinke.R = function(..., path = "~/GitHub/Packages/DatabaseLinke.R") { devtools::document(path); devtools::load_all(path) },
  # gruffiDev = function(..., path = "~/GitHub/Packages/gruffiDev") { devtools::document(path); devtools::load_all(path) },
  gruffi = function(..., path = "~/GitHub/Packages/gruffi") { devtools::document(path); devtools::load_all(path) }
)

r <- pReload <- list(
  Rocinante =          function(...) source('~/GitHub/Packages/Rocinante/R/Rocinante.R'),
  Stringendo =      function(..., path = "~/GitHub/Packages/Stringendo") { devtools::load_all(path) },
  ReadWriter =      function(..., path = "~/GitHub/Packages/ReadWriter") { devtools::load_all(path) },
  CodeAndRoll2 =    function(..., path = "~/GitHub/Packages/CodeAndRoll2") { devtools::load_all(path) },
  ReadWriter =    function(..., path = "~/GitHub/Packages/ReadWriter") { devtools::load_all(path) },
  PackageTools = function(..., path = "~/GitHub/Packages/PackageTools") { devtools::load_all(path) },

  MarkdownHelpers =  function(..., path = "~/GitHub/Packages/MarkdownHelpers") { devtools::load_all(path) },
  MarkdownReports =  function(..., path = "~/GitHub/Packages/MarkdownReports") { devtools::load_all(path) },
  ggExpress =        function(..., path = "~/GitHub/Packages/ggExpress") { devtools::load_all(path) },

  Seurat.utils =   function(..., path = "~/GitHub/Packages/Seurat.utils") { devtools::load_all(path) },
  isoENV =         function(..., path = "~/GitHub/Packages/isoENV") { devtools::load_all(path) },

  UVI.tools =         function(..., path = "~/GitHub/Packages/UVI.tools") { devtools::load_all(path) },
  Connectome.tools =  function(..., path = "~/GitHub/Packages/Connectome.tools") { devtools::load_all(path) },
  NestedMultiplexer = function(..., path = "~/GitHub/Packages/NestedMultiplexer") { devtools::load_all(path) },

  DatabaseLinke.R = function(..., path = "~/GitHub/Packages/DatabaseLinke.R") { devtools::load_all(path) },
  # gruffiDev = function(..., path = "~/GitHub/Packages/gruffiDev") { devtools::document(path); devtools::load_all(path) },
  gruffi = function(..., path = "~/GitHub/Packages/gruffi") { devtools::load_all(path) }
)

# Open the traffic graph for each package on GitHub
repoTrafficGraph <- list(
  Stringendo =       function() { browseURL("https://github.com/vertesy/Stringendo/graphs/traffic") },
  ReadWriter =       function() { browseURL("https://github.com/vertesy/ReadWriter/graphs/traffic") },
  CodeAndRoll2 =     function() { browseURL("https://github.com/vertesy/CodeAndRoll2/graphs/traffic") },
  PackageTools =     function() { browseURL("https://github.com/vertesy/PackageTools/graphs/traffic") },
  MarkdownHelpers =  function() { browseURL("https://github.com/vertesy/MarkdownHelpers/graphs/traffic") },
  MarkdownReports =  function() { browseURL("https://github.com/vertesy/MarkdownReports/graphs/traffic") },
  ggExpress =        function() { browseURL("https://github.com/vertesy/ggExpress/graphs/traffic") },
  Seurat.utils =     function() { browseURL("https://github.com/vertesy/Seurat.utils/graphs/traffic") },
  isoENV =           function() { browseURL("https://github.com/vertesy/isoENV/graphs/traffic") },
  UVI.tools =        function() { browseURL("https://github.com/vertesy/UVI.tools/graphs/traffic") },
  Connectome.tools = function() { browseURL("https://github.com/vertesy/Connectome.tools/graphs/traffic") },
  NestedMultiplexer =function() { browseURL("https://github.com/vertesy/NestedMultiplexer/graphs/traffic") },
  DatabaseLinke.R =  function() { browseURL("https://github.com/vertesy/DatabaseLinke.R/graphs/traffic") }
  # gruffi =           function() { browseURL("https://github.com/vertesy/jngoe/graphs/traffic") }
)
repoTrafficGraph_ALL <- function() { lapply(repoTrafficGraph, function(x) x()) }

# Define a list of functions to open the "Create_the_*_Package.R" file for each package
openCreatePackageFile <- list(
  Stringendo =       function() { file.edit("~/GitHub/Packages/Stringendo/Development/Create_the_Stringendo_Package.R") },
  ReadWriter =       function() { file.edit("~/GitHub/Packages/ReadWriter/Development/Create_the_ReadWriter_Package.R") },
  CodeAndRoll2 =     function() { file.edit("~/GitHub/Packages/CodeAndRoll2/Development/Create_the_CodeAndRoll2_Package.R") },
  PackageTools =     function() { file.edit("~/GitHub/Packages/PackageTools/Development/Create_the_PackageTools_Package.R") },
  MarkdownHelpers =  function() { file.edit("~/GitHub/Packages/MarkdownHelpers/Development/Create_the_MarkdownHelpers_Package.R") },
  MarkdownReports =  function() { file.edit("~/GitHub/Packages/MarkdownReports/Development/Create_the_MarkdownReports_Package.R") },
  ggExpress =        function() { file.edit("~/GitHub/Packages/ggExpress/Development/Create_the_ggExpress_Package.R") },
  Seurat.utils =     function() { file.edit("~/GitHub/Packages/Seurat.utils/Development/Create_the_Seurat.utils_Package.R") },
  isoENV =           function() { file.edit("~/GitHub/Packages/isoENV/Development/Create_the_isoENV_Package.R") },
  UVI.tools =        function() { file.edit("~/GitHub/Packages/UVI.tools/Development/Create_the_UVI.tools_Package.R") },
  Connectome.tools = function() { file.edit("~/GitHub/Packages/Connectome.tools/Development/Create_the_Connectome.tools_Package.R") },
  NestedMultiplexer =function() { file.edit("~/GitHub/Packages/NestedMultiplexer/Development/Create_the_NestedMultiplexer_Package.R") },
  DatabaseLinke.R =  function() { file.edit("~/GitHub/Packages/DatabaseLinke.R/Development/Create_the_DatabaseLinke.R_Package.R") }
)
openALL_CreatePackageFiles <- function() { lapply(openCreatePackageFile, function(f) f()) }


openConfigFiles <- list(
  Stringendo =       function() { file.edit("~/GitHub/Packages/Stringendo/Development/config.R") },
  ReadWriter =       function() { file.edit("~/GitHub/Packages/ReadWriter/Development/config.R") },
  CodeAndRoll2 =     function() { file.edit("~/GitHub/Packages/CodeAndRoll2/Development/config.R") },
  PackageTools =     function() { file.edit("~/GitHub/Packages/PackageTools/Development/config.R") },
  MarkdownHelpers =  function() { file.edit("~/GitHub/Packages/MarkdownHelpers/Development/config.R") },
  MarkdownReports =  function() { file.edit("~/GitHub/Packages/MarkdownReports/Development/config.R") },
  ggExpress =        function() { file.edit("~/GitHub/Packages/ggExpress/Development/config.R") },
  Seurat.utils =     function() { file.edit("~/GitHub/Packages/Seurat.utils/Development/config.R") },
  isoENV =           function() { file.edit("~/GitHub/Packages/isoENV/Development/config.R") },
  UVI.tools =        function() { file.edit("~/GitHub/Packages/UVI.tools/Development/config.R") },
  Connectome.tools = function() { file.edit("~/GitHub/Packages/Connectome.tools/Development/config.R") },
  NestedMultiplexer =function() { file.edit("~/GitHub/Packages/NestedMultiplexer/Development/config.R") },
  # gruffi =           function() { file.edit("~/GitHub/Packages/gruffi/Development/config.R") }
  DatabaseLinke.R =  function() { file.edit("~/GitHub/Packages/DatabaseLinke.R/Development/config.R") }
)
openALL_ConfigFiles <- function() { lapply(openConfigFiles, function(f) f()) }

# Package and script helpers ____________________________________________________________ ----

d.all <- function() x <- lapply(d, function(f) f())
r.all <- function() x <- lapply(r, function(f) f())

helpPak <- function(x) {
  pkg <- deparse(substitute(x))
  browseURL(paste0("https://www.rdocumentation.org/packages/", pkg))
}

ooo <- function(...) osXpath(getwd(), ...)
ccc <- function(...) clipr::write_clip(cbepath(clipr::read_clip()))
oofix <- function(...) clipr::write_clip(gsub(pattern = '\\[1\\] ', replacement = '', x = clipr::read_clip()))

# ____________________________________________________________
osXpath7 <- function(x = ifExistsElse('OutDir', alternative =  "/groups/knoblich/users/abel.vertesy/Analysis/sc6_21.v7/variables.2.regress_nuclear.fraction/"),
                      # "/groups/knoblich/Projects/connectomics/Analysis/sc6_21.v5/",
                    attach = "smb://storage.imp.ac.at/groups/knoblich/Projects/connectomics/Analysis/sc6_21.v7/variables.2.regress_nuclear.fraction",
                    cbe = "/groups/knoblich/Projects/connectomics/Analysis/sc6_21.v7/") {
  # last.folder <- basename(attach)
  message("Attach in finder:\n", attach, "\n")
  message(paste("open", gsub(x, pattern = cbe, replacement = "/Volumes/")))
}


# _______________________________________________________________________________________
# osXpath2 <- function(x = ifExistsElse('OutDir', alternative =  "/groups/knoblich/users/abel.vertesy/Analysis/"),
#                     # "/groups/knoblich/Projects/connectomics/Analysis/sc6_21.v5/",
#                     attach = "smb://storage.imp.ac.at/groups/knoblich/Projects/connectomics/Analysis",
#                     cbe = "/groups/knoblich/Projects/connectomics/") {
#   # last.folder <- basename(attach)
#   message("designed attach: ", attach, " in finder.\n")
#   message(paste("open", gsub(x, pattern = cbe, replacement = "/Volumes/")))
# }

# _______________________________________________________________________________________
osXpath <- function(x = ifExistsElse('OutDir', alternative =  "/groups/knoblich/users/abel.vertesy/Analysis/sc6_21.v8/"),
                      # "/groups/knoblich/Projects/connectomics/Analysis/sc6_21.v5/",
                      attach = "smb://storage.imp.ac.at/groups/knoblich/Projects/connectomics/Analysis/sc6_21.v8",
                      cbe = "/groups/knoblich/Projects/connectomics/Analysis/sc6_21.v8/") {
  # last.folder <- basename(attach)
  message("Attach in finder:\n", attach, "\n")
  message(paste("open", gsub(x, pattern = cbe, replacement = "/Volumes/sc6_21.v8/")))
}
# osXpath()

# ____________________________________________________________
cbepath <- function(x = "/Volumes/Analysis/sc6_21.v5/preMerge.v2.Correct.CBC/Gruffi.Stress.annotation.v4/combined.obj_1_gruffi.complete.full_CON_2024.02.27_14.21.qs",
                     cbe = "/groups/knoblich/Projects/connectomics/") {
  # last.folder <- basename(attach)
  message(gsub(x, pattern = "/Volumes/", replacement = cbe))
}


# _______________________________________________________________________________________
#' @title Get Current Script Name in RStudio
#'
#' @description Retrieves the file name of the current script open in the RStudio source editor.
#' This function is specific to RStudio and will not work in other environments.
#' @param toclipboard Copy to clipboard? Def: TRUE.
#'
#' @return A character string with the file name of the current R script open in RStudio.
#' If not running in RStudio or if no script is open, it returns NULL.
#' @export
#'
#' @examples
#' getCurrentScriptName() # Returns the name of the script currently open in RStudio
getCurrentScriptName <- function(toclipboard = TRUE) {
  # Ensure that rstudioapi is available
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    stop("rstudioapi package is required.")
  }
  # Retrieve the file name of the current script
  file_name <- basename(rstudioapi::getSourceEditorContext()$path)

  if (toclipboard & require(clipr)) try(clipr::write_clip(file_name), silent = TRUE)

  return(file_name)
}

#' @title Get Current Script path in RStudio
#'
#' @description Retrieves the file path of the current script open in the RStudio source editor.
#' This function is specific to RStudio and will not work in other environments.
#' @examples
#' getCurrentScriptPath() # Returns the path of the script currently open in RStudio
#'
#' @export
getCurrentScriptPath <- function() {
  # Ensure that rstudioapi is available
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    stop("rstudioapi package is required.")
  }

  # Retrieve the file path of the current script
  message(paste0("file.edit('", rstudioapi::getSourceEditorContext()$path, "')"))

}



#


# ____________________________________________________________
# Wrapper function to create a list of functions and their corresponding package names
listFunctionsByPackage <- function(packageNames) {
  stopifnot(is.vector(packageNames), all(sapply(packageNames, is.character))) # Validate input

  # Initialize an empty list to store results
  functionsList <- list()

  # Iterate over each package
  for (pkg in packageNames) {
    print(pkg)
    # Get functions from the package
    funcs <- PackageTools::all_funs(pkg)
    # funcs <- all_funs(pkg)

    # Add to the functions list with package name as value
    for (func in funcs) {
      functionsList[[func]] <- paste0(pkg, "::", func)
    }
  }

  return(functionsList)
}


# ____________________________________________________________
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


# ____________________________________________________________
#' @title Source Specific Lines from an R Script
#'
#' @description
#' This function sources (executes) specific lines from an R script. It reads the script into R,
#' extracts the lines within the specified range, and then evaluates (executes) those lines.
#'
#' @param file_path A string specifying the path to the R script. Default: `NULL`.
#' @param lines A numeric vector specifying the lines source, e.g.: `10:200`. Default: `NULL`.
#'
#' @return This function does not return a value. It executes the specified lines of the R script.
#' @examples
#' # Source lines 10 to 20 from the script located at "path/to/your/script.R"
#' sourceLines("path/to/your/script.R", 10, 20)
#'
#' @export
sourceLines <- function(file_path, lines) {
  stopifnot(
    is.character(file_path), length(file_path) == 1, file.exists(file_path),
    is.numeric(lines), lines[1] >= 1, lines[2] >= lines[1]
  )

  # Read the entire script into R as a vector of strings (1 for each line).
  script_all_lines <- readLines(file_path)

  # Ensure end_line does not exceed the number of lines in the script
  if (lines[2] > length(script_all_lines)) {
    lines[2] <- length(script_all_lines)
    message("end_line exceeds the number of lines in the script. Clipping to the last line, ", lines[2])
  }

  # Source the extracted lines using textConnection()
  source(textConnection(script_all_lines[lines]))

}
# pathX <- "~/GitHub/TheCorvinas/R/Test.for.sourceLines.function.R"
# sourceLines(pathX, 1, 15)
# sourceLines(pathX, 15,20)
# sourceLines(pathX, 1,222)


# ____________________________________________________________
# Source parts of another script. Source: https://stackoverflow.com/questions/26245554/execute-a-set-of-lines-from-another-r-file
sourcePartial <- function(fn, startTag = '#1', endTag = '#/1') {
  lines <- scan(fn, what = character(), sep = "\n", quiet = TRUE)
  st <- grep(startTag,lines)
  en <- grep(endTag,lines)
  tc <- textConnection(lines[(st + 1):(en - 1)])
  source(tc)
  close(tc)
}

# ____________________________________________________________
args.2.global <- ass <- function(overwrite = FALSE, ...) {
  args <- list(...)
  namez <- names(args)
  for (i in 1:length(args)) { print("------------------------------------")
    print(namez[i])
    print(args[[i]])

    if(exists(namez[i])) {
      iprint(namez[i], "already exists, overwritten only if specified in arg 1.")
      if (overwrite) assign(x = names(args)[i], value = args[[i]], envir = .GlobalEnv)
    } else {
      assign(x = names(args)[i], value = args[[i]], envir = .GlobalEnv)
    }
  }
  print(names(args))
}


# Memory ____________________________________________________________ ----
#' @title Show biggest object in memory
#'
#' @description Show distribution of the largest objects and return their names.
#' Based on https://stackoverflow.com/questions/17218404/should-i-get-a-habit-of-removing-unused-variables-in-r

memory.biggest.objects <- function(n = 5, plot = T, saveplot = FALSE) {
  try(dev.off(), silent = TRUE)
  gc()
  ls.mem <- ls( envir = .GlobalEnv)
  ls.obj <- lapply(ls.mem, get)
  Sizes.of.objects.in.mem <- CodeAndRoll2::unlapply(ls.obj, object.size)
  names(Sizes.of.objects.in.mem) <- ls.mem
  topX = sort(Sizes.of.objects.in.mem,decreasing = TRUE)[1:n]

  Memorty.usage.stat = c(topX, 'Other' = sum(sort(Sizes.of.objects.in.mem,decreasing = TRUE)[-(1:n)]))

  top.names <- head(names(topX), n = 5)
  # strX <- as.character(capture.output(dput(top.names)))
  strX <- kollapse(top.names, collapseby = "', '")
  # strX <- gsub('[^A-Za-z0-9 ,._/()]', '', strX)
  message("rm(list = c('", strX, "'))\n")

  if(plot) {
    pie(x = Memorty.usage.stat, cex = .5, sub = date(),
        col = grDevices::terrain.colors(length(Memorty.usage.stat)))
    # dput(names(topX))
  }
}
# memory.biggest.objects()




# _________________________________________________________________________________________________

# _________________________________________________________________________________________________


# Generic ____________________________________________________________ ----
# printEveryN <- function(i, N = 1000) { if ((i %% N) == 0 ) iprint(i) } # Report at every e.g. 1000




say <- function(...) { # Use system voice to notify (after a long task is done)
  sys <- Sys.info()["sysname"]
  if (sys == "Darwin") system("say Ready!") # say -v Samantha 'Ready!'
  if (sys == "Linux") system("echo -e '\a'; sleep 0.5s; echo -e '\a'; sleep 0.5s; echo -e '\a'; sleep 0.5s; echo -e '\a'; sleep 0.5s; echo -e '\a'; sleep 0.5s; echo -e '\a'")  # For UNIX servers.
}
sayy <- function(...) {system("say 'Ready to roll!'")} # Use system voice to notify (after a long task is done)

# oo <- function(path = '.') { # Open current working directory, or any directory.
#   system(paste("open", path))
# }
# oo <- function() { print(list.files(getwd())); print("dir"); print(getwd()) }

oo <- function(x=NULL) {
  message('WD\n', getwd())
  if (exists('OutDir')) {
    if( ! getwd() == RemoveFinalSlash(OutDir)) {
      message("OutDir different to WD:\n", RemoveFinalSlash(OutDir))
    }
  } else {
    message("Outdir not defined.")
  }
}


view.head <- function(matrix, enn = 10) { matrix[1:min(NROW(matrix), enn), 1:min(NCOL(matrix), enn)] } # view the head of an object by console.
view.head2 <- function(matrix, enn = 10) { View(head(matrix, n = min(NROW(matrix), NCOL(matrix), enn))) } # view the head of an object by View().


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

backup <- function(obj, overwrite = FALSE) { # make a backup of an object into global env. Scheme: obj > obj.bac
  varname <- as.character(substitute(obj))
  bac.varname <- ppp(varname, "bac")
  if (exists(bac.varname) & !overwrite ) {
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


list_subdirectories_at_depth <- function(path = '.', depth = 2) {
  path <- path.expand(path)
  num_slashes <- stringr::str_count(path, "/")
  target_depth <- depth + num_slashes

  subdirs <- list.dirs(path, recursive = TRUE)
  subdirs[stringr::str_count(subdirs, "/") == target_depth]

}
# list_subdirectories_at_depth(path = '~/Downloads', depth = 2)


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


#' @title Find Function Package
#' @description Determines the package that a given function is defined in.
#'
#' @param functionName The name of the function (character or function object).
#' @param searchInstalled If TRUE, searches all installed packages (default FALSE).
#' @return The name of the package containing the function, or NULL if not found.
#' @export


findFunctionPackage <- function(functionName, searchInstalled = FALSE) {
  # Handle different types of input (character or function)
  if (is.function(functionName)) {
    functionName <- deparse(substitute(functionName))
  }

  stopifnot(
    "functionName must be a character" = is.character(functionName),
    "searchInstalled must be logical" = is.logical(searchInstalled)
  )

  # Search in loaded namespaces
  searchSpaces <- search()
  for (space in searchSpaces) {
    if (exists(functionName, envir = asNamespace(space), inherits = FALSE)) {
      return(substring(space, 9)) # Remove "package:" prefix
    }
  }

  # Optionally search in installed packages
  if (searchInstalled) {
    installedPkgs <- installed.packages()[, "Package"]
    for (pkg in installedPkgs) {
      if (exists(functionName, envir = asNamespace(pkg), inherits = FALSE)) {
        return(pkg)
      }
    }
  }

  return(NULL)
}



# Clipboard interaction ____________________________________________________________ ----
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



# _______________________________________________________________
PCA.percent.var.explained <- function(prcomp.res =  sPCA) { # Determine percent of variation associated with each PC. For Seurat see: scCalcPCAVarExplained().
  PCA.w.summary.added <- summary(prcomp.res)
  PCA.w.summary.added$importance['Proportion of Variance', ]
}



# __________________________________________________________________________________________________
# Distance and correlation calculations _____________________________________________________ ----
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


# ___________________________________________________________________________________________ ------
# Plotting and Graphics ____________________________________________________________ ----

colSums.barplot <- function(df, col = "seagreen2", na_rm = TRUE, ...) { barplot(colSums(df, na.rm = na_rm), col = col, ...) } # Draw a barplot from ColSums of a matrix.

richColors <- function(n = 3) { gplots::rich.colors(n) } # Alias for rich.colors in gplots

qheatmap <- function(df, cluster_rows = FALSE, cluster_cols = FALSE,
                     main = make.names(Stringendo::FixPlotName(substitute(df))), ...) {
  pheatmap::pheatmap(mat = df, cluster_rows = cluster_rows, cluster_cols = cluster_cols, main = main, ...)
}

HeatMapCol_BGR <- grDevices::colorRampPalette(c("blue", "cyan", "yellow", "red"), bias = 1)
# HeatMapCol_BWR <- grDevices::colorRampPalette(c("blue", "white", "red"), bias = 1)
HeatMapCol_RedBlackGreen <- grDevices::colorRampPalette(c("red", "black", "green"), bias = 1)


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



## Functions for pairs() plots ____________________________________________________________ ----
# panelCorPearson <- function(x, y, digits = 2, prefix = "", cex.cor = 2, method = "pearson") { # A function to display correlation values for pairs() function. Default is pearson correlation, that can be set to  "kendall" or "spearman".
#   usr <- par("usr"); on.exit(par(usr))
#   par(usr = c(0, 1, 0, 1))
#   r <- abs(cor(x, y, method = method, use = "complete.obs"))
#   txt <- format(c(r, 0.123456789), digits = digits)[1]
#   txt <- paste(prefix, txt, sep = "")
#   if (missing(cex.cor)) cex <- 0.8/strwidth(txt)
#
#   test <- cor.test(x, y)
#   Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
#                    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
#                    symbols = c("***", "**", "*", ".", " "))
#
#   text(0.5, 0.5, txt, cex = cex * r)
#   text(.8, .8, Signif, cex = cex,  col = 2)
# }

panelCorSpearman <- function(x, y, digits = 2, prefix = "", cex.cor = 2, method = "spearman") { # A function to display correlation values for pairs() function. Default is pearson correlation, that can be set to  "kendall" or "spearman".
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



# ___________________________________________________________________________________________ ------
## Clustering heatmap tools _________________________________________________________________ ------

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


# ________________________________________________________________________
"FOR VECTOR. it works"
#' @title Create Annotation Columns for pheatmap
#'
#' @description
#' Auxiliary function for pheatmap. Prepares the two variables needed for `annotation_col` and
#' `annotation_colors` in pheatmap.
#'
#' @param data A data frame or matrix where columns are to be annotated. Default: None.
#' @param annot_vec A vector of annotations corresponding to the columns of the data. Default: None.
#' @param annot_names A character string representing the name of the annotation column. Default: "Annot".
#'
#' @return No return value. The function assigns `annot` (data frame) and `annot_col` (list) to the global
#' environment for use with `pheatmap`.
#'
#' @importFrom gplots rich.colors
#' @export
#'
annot_col.create.pheatmap.vec <- function(data,
                                          annot_vec,
                                          annot_names = "Annot") {

  # Input argument assertions
  stopifnot(
    is.data.frame(data) || is.matrix(data),
    is.vector(annot_vec),
    length(annot_vec) == ncol(data),
    is.character(annot_names)
  )

  namez = as.character(if (is.null(annot_names)) substitute(annot_vec) else annot_names)

  df = data.frame(x = annot_vec);

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

  message("annot [data frame] and annot_col [list] variables are created.
          Use: pheatmap(..., annotation_col = annot, annotation_colors = annot_col)")
}


# ________________________________________________________________________
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

  print("annot [data frame] and annot_col [list] variables are created.
        Use: pheatmap(..., annotation_col = annot, annotation_colors = annot_col)")
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

  print("annot_rows [data frame] and annot_rows.col [list] variables are created.
        Use: pheatmap(..., annotation_row = annot_rows, annotation_colors = annot_rows.col)")
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



# New FUN ____________________________________________________________ ----


ssh2osX <- function(shellpath=clipr::read_clip()) { # '/groups/knoblich/users/burkard/Abel.Vertesy/R12357/R12357_merged_20211108093511/README.html'
  newpath <- gsub(x = shellpath, pattern = '/groups/', replacement = 'smb://storage.imp.ac.at/groups/')
  clipr::write_clip(newpath)
}

osX2ssh <- function(shellpath=clipr::read_clip()) { # '/groups/knoblich/users/burkard/Abel.Vertesy/R12357/R12357_merged_20211108093511/README.html'
  newpath <- gsub(x = shellpath, replacement = '/groups/',  pattern= 'smb://storage.imp.ac.at/groups/')
  clipr::write_clip(newpath)
}


# _________________________________________________________________________________________________
#' STRINGdb.reformat.ann.table.per.gene  > Databaselnker
#'
#' @param path_of_tsv input file
#' @param column column name
#' @param sep value separation
#' @export
#' @examples

STRINGdb.reformat.ann.table.per.gene <- function(path_of_tsv = '~/Downloads/enrichment.DISEASES.tsv'
                                                 , column = 'matching proteins in your network (labels)'
                                                 , sep = ',') {

  annotation_tsv <- CodeAndRoll2::read.simple.tsv(path_of_tsv)
  stopifnot(column %in% colnames(annotation_tsv))
  (tbl_split <- tidyr::separate_rows(data = annotation_tsv, column, sep = sep ))
  CodeAndRoll2::write.simple.tsv(tbl_split, ManualName = paste(path_of_tsv, "per.gene.tsv", sep = ".") )
  return(tbl_split)
}



# _________________________________________________________________________________________________




#  ____________________________________________________________
rnd4l <- function(set = c(LETTERS, 0:9), n = 4) {
  print(paste0(paste0( sample(x = set, size = n), collapse = ''), '__'))
}



# TMP code and roll -------------------- -----------------------------------------------------------------
# fractions <- function(vec, na_rm = TRUE) vec/ sum(vec, na.rm = na_rm)
# unique.wNames <- function(x) { x[!duplicated(x)] }




# ____________________________________________________________
# https://stackoverflow.com/questions/6216968/r-force-local-scope

# findGlobals2 <- function(x) {
#   codetools::findGlobals(x)
#
# }
#
# checkStrict <- function(f, silent=FALSE) {
#   vars <- codetools::findGlobals(f)
#   found <- !vapply(vars, exists, logical(1), envir=as.environment(2))
#   if (!silent && any(found)) {
#     warning("global variables used: ", paste(names(found)[found], collapse=', '))
#     return(invisible(FALSE))
#   }
#
#   !any(found)
# }
#
