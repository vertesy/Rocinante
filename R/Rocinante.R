# ____________________________________________________________________
# Rocinante - A collection of custom R functions. Helper functions complementing CodeAndRoll2.
# ____________________________________________________________________
# source('~/GitHub/Packages/Rocinante/R/Rocinante.R')
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


# Alisases ____________________________________________________________ ----
sort.natural = gtools::mixedsort
comma = scales::comma

b <- browser
p0 <- paste0
l <- length
toclip <- clipr::write_clip
fromclip <- clipr::read_clip

stry <- function(...) {try(..., silent = TRUE)} # Silent try

warnings.erase <- function() assign("last.warning", NULL, envir = baseenv())

rprofile <-  function() file.edit('~/.Rprofile')
rocinanteSource <- function() source('~/GitHub/Packages/Rocinante/R/Rocinante.R')

# Package Loaders ____________________________________________________________ ----

o <- pOpen <- list(
  Rocinante =          function() file.edit('~/GitHub/Packages/Rocinante/R/Rocinante.R'),
  Stringendo =         function() file.edit('~/GitHub/Packages/Stringendo/R/Stringendo.R'),
  CodeAndRoll2 =       function() file.edit('~/GitHub/Packages/CodeAndRoll2/R/CodeAndRoll2.R'),
  ReadWriter =         function() file.edit('~/GitHub/Packages/ReadWriter/R/ReadWriter.R'),

  PackageTools =       function() file.edit('~/GitHub/Packages/PackageTools/R/PackageTools.R'),
    PackageToolsREPL =       function() file.edit('~/GitHub/Packages/PackageTools/R/ReplacementTools.R'),
    PackageToolsDOC =       function() file.edit('~/GitHub/Packages/PackageTools/R/DocumentationTools.R'),
    PackageToolsDEP =       function() file.edit('~/GitHub/Packages/PackageTools/R/DependencyTools.R'),
    PackageToolsMISC =       function() file.edit('~/GitHub/Packages/PackageTools/R/Miscellaneous.R'),

  MarkdownHelpers =    function() file.edit('~/GitHub/Packages/MarkdownHelpers/R/MarkdownHelpers.R'),
  MarkdownReports =    function() file.edit('~/GitHub/Packages/MarkdownReports/R/MarkdownReports.R'),
  ggExpress =          function() file.edit('~/GitHub/Packages/ggExpress/R/ggExpress.R'),

  SeuratUtils =        function() file.edit('~/GitHub/Packages/Seurat.utils/R/Seurat.Utils.R'),
    SeuratUtils_META =        function() file.edit('~/GitHub/Packages/Seurat.utils/R/Seurat.Utils.Metadata.R'),
    SeuratUtils_VIZ =        function() file.edit('~/GitHub/Packages/Seurat.utils/R/Seurat.Utils.Visualization.R'),
  isoENV =             function() file.edit('~/GitHub/Packages/isoENV/R/isoENV.R'),
  isoENV.other =       function() file.edit('~/GitHub/Packages/isoENV/R/isoENV.other.R'),

  UVITools =           function() file.edit('~/GitHub/Packages/UVI.tools/R/UVI.tools.R'),
    UVIToolsBulk =       function() file.edit('~/GitHub/Packages/UVI.tools/R/UVI.tools.Bulk.R'),
  ConnectomeTools =    function() file.edit('~/GitHub/Packages/Connectome.tools/R/Connectome.tools.R'),
    ConnectomeToolsAAV = function() file.edit('~/GitHub/Packages/Connectome.tools/R/Connectome.tools.AAV.R'),
  NestedMultiplexer =  function() file.edit('~/GitHub/Packages/NestedMultiplexer/R/NestedMultiplexer.R'),

  gruffiDev =          function() file.edit('~/GitHub/Packages/gruffiDev/R/gruffi.R'),
  gruffi =             function() file.edit('~/GitHub/Packages/gruffi/R/gruffi.R')

)

d <- pDocAndLoad <- list(
  Stringendo =       function(path = "~/GitHub/Packages/Stringendo") { devtools::document(path); devtools::load_all(path) },
  ReadWriter =       function(path = "~/GitHub/Packages/ReadWriter") { devtools::document(path); devtools::load_all(path) },
  CodeAndRoll2 =     function(path = "~/GitHub/Packages/CodeAndRoll2") { devtools::document(path); devtools::load_all(path) },
  ReadWriter =       function(path = "~/GitHub/Packages/ReadWriter") { devtools::document(path); devtools::load_all(path) },
  PackageTools =     function(path = "~/GitHub/Packages/PackageTools") { devtools::document(path); devtools::load_all(path) },

  MarkdownHelpers =  function(path = "~/GitHub/Packages/MarkdownHelpers") { devtools::document(path); devtools::load_all(path) },
  MarkdownReports =  function(path = "~/GitHub/Packages/MarkdownReports") { devtools::document(path); devtools::load_all(path) },
  ggExpress =        function(path = "~/GitHub/Packages/ggExpress") { devtools::document(path); devtools::load_all(path) },

  Seurat.utils =     function(path = "~/GitHub/Packages/Seurat.utils") { devtools::document(path); devtools::load_all(path) },
  isoENV =           function(path = "~/GitHub/Packages/isoENV") { devtools::document(path); devtools::load_all(path) },

  UVI.tools =         function(path = "~/GitHub/Packages/UVI.tools") { devtools::document(path); devtools::load_all(path) },
  Connectome.tools =  function(path = "~/GitHub/Packages/Connectome.tools") { devtools::document(path); devtools::load_all(path) },
  NestedMultiplexer = function(path = "~/GitHub/Packages/NestedMultiplexer") { devtools::document(path); devtools::load_all(path) },

  gruffiDev = function(path = "~/GitHub/Packages/gruffiDev") { devtools::document(path); devtools::load_all(path) },
  gruffi = function(path = "~/GitHub/Packages/gruffi") { devtools::document(path); devtools::load_all(path) }
)

r <- pReload <- list(
  Stringendo =      function(path = "~/GitHub/Packages/Stringendo") { devtools::load_all(path) },
  ReadWriter =      function(path = "~/GitHub/Packages/ReadWriter") { devtools::load_all(path) },
  CodeAndRoll2 =    function(path = "~/GitHub/Packages/CodeAndRoll2") { devtools::load_all(path) },
  ReadWriter =    function(path = "~/GitHub/Packages/ReadWriter") { devtools::load_all(path) },
  PackageTools = function(path = "~/GitHub/Packages/PackageTools") { devtools::load_all(path) },

  MarkdownHelpers =  function(path = "~/GitHub/Packages/MarkdownHelpers") { devtools::load_all(path) },
  MarkdownReports =  function(path = "~/GitHub/Packages/MarkdownReports") { devtools::load_all(path) },
  ggExpress =        function(path = "~/GitHub/Packages/ggExpress") { devtools::load_all(path) },

  Seurat.utils =   function(path = "~/GitHub/Packages/Seurat.utils") { devtools::load_all(path) },
  isoENV =         function(path = "~/GitHub/Packages/isoENV") { devtools::load_all(path) },

  UVI.tools =         function(path = "~/GitHub/Packages/UVI.tools") { devtools::load_all(path) },
  Connectome.tools =  function(path = "~/GitHub/Packages/Connectome.tools") { devtools::load_all(path) },
  NestedMultiplexer = function(path = "~/GitHub/Packages/NestedMultiplexer") { devtools::load_all(path) },

  gruffiDev = function(path = "~/GitHub/Packages/gruffiDev") { devtools::document(path); devtools::load_all(path) },
  gruffi = function(path = "~/GitHub/Packages/gruffi") { devtools::load_all(path) }
)

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
osXpath <- function(x = "/groups/knoblich/Projects/connectomics/Analysis/sc6_21.v5/",
                    # attach = "smb://storage.imp.ac.at/groups/knoblich/Projects/connectomics/Analysis",
                    cbe = "/groups/knoblich/Projects/connectomics/") {
  # last.folder <- basename(attach)
  gsub(x, pattern = cbe, replacement = "/Volumes/")
}

# ____________________________________________________________
cbepath <- function(x = "/Volumes/Analysis/sc6_21.v5/preMerge.v2.Correct.CBC/Gruffi.Stress.annotation.v4/combined.obj_1_gruffi.complete.full_CON_2024.02.27_14.21.qs",
                     cbe = "/groups/knoblich/Projects/connectomics/") {
  # last.folder <- basename(attach)
  gsub(x, pattern = "/Volumes/", replacement = cbe)
}


osXpath2 <- function(str = "/groups/knoblich/bioinfo/Projects/connectomics/SNP_demux/classifications/v2") { # Parse path for CBE
  x = gsub(x = str, pattern = "/groups/knoblich/", replacement = '/Volumes/knoblich/')
  print("1. Mount:")
  print("smb://storage.imp.ac.at/groups/knoblich/")
  print("2. Go:")
  print(x)
}



# ____________________________________________________________
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
      functionsList[[func]] <- pkg
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
sourcePartial <- function(fn,startTag = '#1', endTag = '#/1') { # Source parts of another script. Source: https://stackoverflow.com/questions/26245554/execute-a-set-of-lines-from-another-r-file
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
  if(plot) {
    pie(x = Memorty.usage.stat, cex = .5, sub = date(),
        col = grDevices::terrain.colors(length(Memorty.usage.stat)))
    dput(names(topX))
  }

  strX <- as.character(capture.output(dput(head(names(topX), n = 5))))
  strX <- gsub('[^A-Za-z0-9 ,._/()]', '', strX)
  Stringendo::iprint("rm(list = ", strX,")")

}
# memory.biggest.objects()

# _________________________________________________________________________________________________
#' @title Retrieve Memory Information
#'
#' @description
#' Fetches and calculates the used and free memory information for the current system.
#' Supports Windows, Linux, and macOS (Darwin), with specific implementations for each.
#'
#' @details
#' On Windows, it uses `memory.size` and `memory.limit` to calculate memory usage.
#' On Linux, it parses the output of `free -m` command.
#' On macOS, it uses `vm_stat` command and calculates memory based on page size.
#' Note: The function might not work correctly on macOS.
#'
#' @return A named numeric vector with the ceiling values of used and free memory in MB.
#' @examples
#' mem_info <- getMemoryInfo()
#' print(mem_info)
#' @export
getMemoryInfo <- function() {
  os_type <- Sys.info()["sysname"]
  gc()

  memory.biggest.objects(plot = F)

  if (os_type == "Windows") {
    warning("Not tested on Windows", immediate. = TRUE)
    mem_used <- memory.size(max = FALSE) / 1024  # Convert MB to GB
    mem_free <- (memory.limit() - memory.size(max = FALSE)) / 1024  # Convert MB to GB

  } else if (os_type == "Linux") {
    warning("Not tested on Linux", immediate. = TRUE)

    if(exists("onCBE") )  { if (isTRUE(onCBE)) {
      message("on CBE")
      job.details <- getSLURMjobDetails(user_name = "abel.vertesy")
      print("job.details")
      print(job.details)
      print("")

      total_memory <- job.details$mem_in_gb
      mem_used <- sum(sapply(ls(envir = .GlobalEnv), function(x) object.size(get(x))))/1e9
      mem_free <- total_memory - mem_used

    }} else {
      stop()
      # mem_info <- system("free -m", intern = TRUE)
      # mem_lines <- strsplit(mem_info, " +")[[2]]
      # mem_used <- as.numeric(mem_lines[3]) / 1024  # Convert MB to GB
      # mem_free <- as.numeric(mem_lines[4]) / 1024  # Convert MB to GB
    }


  } else if (os_type == "Darwin") {
    mem_info <- system("vm_stat", intern = TRUE)

    page_size_info <- mem_info[grep("page size of", mem_info)]
    page_size <- as.numeric(gsub(".*page size of ([0-9]+) bytes.*", "\\1", page_size_info)) / 1024^3  # Convert bytes to GB

    total_memory <- system2("sysctl", "hw.memsize", stdout = TRUE)
    total_memory <- as.numeric(gsub("hw.memsize: ([0-9]+)", "\\1", total_memory)) / 1024^3  # Convert bytes to GB

    extract_pages <- function(pattern) {
      value <- mem_info[grep(pattern, mem_info)]
      as.numeric(gsub(".*: +([0-9]+).*$", "\\1", value))
    }

    pages_free <- extract_pages("Pages free")
    pages_active <- extract_pages("Pages active")
    pages_inactive <- extract_pages("Pages inactive")
    pages_speculative <- extract_pages("Pages speculative")
    pages_wired <- extract_pages("Pages wired down")

    mem_free <- (pages_free + pages_inactive + pages_speculative) * page_size  # Convert pages to GB
    mem_used <- total_memory - mem_free  # Calculate used memory as total minus free
  } else {
    stop("Unsupported OS")
  }

  # Calculate memory usage of objects in the global environment
  # browser()
  object_sizes <- sapply(ls(envir = .GlobalEnv), function(x) object.size(get(x)))
  object_sizes <- sort(object_sizes, decreasing = TRUE)
  total_size <- sum(object_sizes)
  relative_object_sizes <- object_sizes / total_size

  # Filter objects that use more than 5% of total memory
  significant_objects <- object_sizes[relative_object_sizes > 0.05] / 1e9 # Convert bytes to GB

  if (length(significant_objects) < 1) {
    significant_objects <- c(used.other = sum(relative_object_sizes))
  } else {
    other_usage <- sum(relative_object_sizes[!names(relative_object_sizes) %in% names(significant_objects)])
    significant_objects <- c(significant_objects, used.other = other_usage)
  }

  stopifnot(is.numeric(mem_used), is.numeric(mem_free))
  stopifnot(length(significant_objects) < 21) # 20*0.05 =1
  print(c(mem_used, mem_free,total_size) )

  return(list(memory = signif(c(Used = mem_used, Free = mem_free), digits = 2),
              objects = significant_objects))
}




# _________________________________________________________________________________________________
#' @title Plot Memory Usage
#'
#' @description
#' Plots the used and free memory as a stacked barplot. The memory values are displayed in gigabytes.
#' The plot includes the total memory as a subtitle and the operating system with the current time/date as a caption.
#'
#' @details
#' The function calls `getMemoryInfo` to retrieve memory information and then uses `ggplot2` to plot the data.
#' Memory values are converted to GB and percentages are calculated for plotting.
#'
#' @importFrom ggplot2 ggplot geom_bar geom_text aes labs theme_minimal scale_fill_brewer
#' @examples plotMemoryUsage()
#' @export
plotMemoryUsage <- function() {
  require(ggplot2)
  require(gridExtra)
  require(scales)

  mem_info <- getMemoryInfo()
  mem_df <- data.frame(Type = names(mem_info$memory), Memory = mem_info$memory)
  obj_df <- data.frame(Object = names(mem_info$objects), Usage = mem_info$objects)

  # Calculate total memory and the percentage for each type
  total_memory <- sum(mem_df$Memory)
  mem_df$Percentage <- mem_df$Memory / total_memory * 100

  # Calculate total objects usage and the percentage for each object
  total_objects <- sum(obj_df$Usage)
  obj_df$Percentage <- obj_df$Usage / total_objects * 100

  os_info <- Sys.info()["sysname"]
  current_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  # Plot for overall memory usage
  subt <- paste("Total System Memory:", round(total_memory, 2), "GB",
                "\nUsed:", mem_info$memory["Used"], "GB")

  p1 <- ggplot(mem_df, aes(x = "", y = Memory, fill = Type)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(aes(label = paste0(round(Percentage, 1), "%")),
              position = position_stack(vjust = 0.5), size = 3.5) +
    labs(title = "Memory Usage",
         subtitle = subt,
         y = "System Memory Found (GB)", x = "") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set3")

  # Plot for object memory usage
  subt <- paste("Used Memory in R:", round(total_objects, 2), "GB",
                "\nLargest Obj:", round(mem_info$objects[1], 3), "GB",
                names(mem_info$objects)[1])

  p2 <- ggplot(obj_df, aes(x = "", y = Usage, fill = Object)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(aes(label = paste0(round(Percentage, 1), "%")),
              position = position_stack(vjust = 0.5), size = 3.5) +
    labs(title = "Detailed Object Usage",
         subtitle = subt,
         caption = paste(os_info, current_time),
         y = "Memory by R objects (GB)", x = "") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2")

  # Arrange the two plots side by side
  grid.arrange(p1, p2, ncol = 2)
}
# plotMemoryUsage()





# _________________________________________________________________________________________________
#' @title Retrieve SLURM Job Details
#'
#' @description This function fetches details of the current SLURM job for a specified user.
#' It returns information such as the job's hostname, ID, memory allocation (in GB),
#' number of CPUs, nodes, and runtime.
#'
#' @param user_name A string representing the username for which to retrieve job information.
#'                  The default value is "abel.vertesy".
#' @return A list containing the following elements:
#'         - `hostname`: The cleaned hostname of the node.
#'         - `job_id`: The ID of the job.
#'         - `mem_in_gb`: The amount of memory allocated to the job in gigabytes.
#'         - `cpus`: The number of CPUs allocated to the job.
#'         - `nodes`: The number of nodes allocated to the job.
#'         - `runtime`: The runtime of the job in either "HH:MM" or "H:MM:SS" format.
#' @importFrom stats setNames
#' @examples
#' slurm_details <- get_slurm_job_details(user_name = "your_username")
#' print(slurm_details)
#' @note This function is intended to be used in a SLURM managed HPC environment.
#'       The availability and correctness of the information depend on the SLURM
#'       configuration and the user's permissions.
#'
#' @export
getSLURMjobDetails <- function(user_name = "abel.vertesy") {
  # Define a helper function to run a command and return its output
  run_command <- function(cmd) {
    output <- system(cmd, intern = TRUE)
    if (length(output) == 0) {
      return(NA)
    }
    return(output)
  }

  # Extract the hostname
  hostname_clean <- run_command("hostname | sed 's/\\.cbe\\.vbc\\.ac\\.at//'")

  # Get job information
  job_info <- run_command(paste("squeue -u", user_name, "| grep", hostname_clean))
  if (all(is.na(job_info)) || length(job_info) == 0) {
    return(list(hostname = hostname_clean, job_id = NA, mem = NA, cpus = NA, nodes = NA))
  }

  # Extract job ID
  job_id <-  sub("^\\s*(\\S+).*", "\\1", job_info)
  job_runtime <- sub(".*R\\s+((\\d+:)?\\d{2}:\\d{2}).*", "\\1", job_info)

  # Get job details
  job_details <- run_command(paste("scontrol show job", job_id))

  # browser()
  # Extract AllocTRES
  alloc_tres <- grep("AllocTRES", job_details, value = TRUE)

  # Initialize variables
  cpus <- NA
  mem <- NA
  nodes <- NA

  # Check if AllocTRES line is found
  if (length(alloc_tres) > 0) {
    tres_parts <- strsplit(alloc_tres, ",")[[1]]
    for (part in tres_parts) {
      if (grepl("cpu=", part)) {
        cpus <- sub(".*cpu=([0-9]+).*", "\\1", part)
      } else if (grepl("mem=", part)) {
        # Extract memory allocation (assuming it's in MB or GB)
        mem_string <- sub(".*mem=([0-9]+[MG]?).*", "\\1", part)

        # Extract the numeric part and the unit (M or G)
        if (grepl("M$", mem_string)) { # Memory in MB
          mem_value <- as.numeric(sub("M$", "", mem_string))
          mem_in_gb <- mem_value / 1024
        } else if (grepl("G$", mem_string)) { # Memory in GB
          mem_in_gb <- as.numeric(sub("G$", "", mem_string))
        } else {
          mem_in_gb <- NA # Default or unknown unit
        }

      } else if (grepl("node=", part)) {
        nodes <- sub(".*node=([0-9]+).*", "\\1", part)
      }
    }
  } else {
    warning("AllocTRES not found")
  }

  # Return the details
  list(hostname = hostname_clean, job_id = job_id, mem_in_gb = round(mem_in_gb,2),
       cpus = as.numeric(cpus), nodes = as.numeric(nodes)
       , runtime = job_runtime)
}



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

oo <- function(path = '.') { # Open current working directory, or any directory.
  system(paste("open", path))
}

# make_bash_compatible <- function(x) {
#   special_chars <- c("\\$", "\\\\", "\\ ", "\\\"", "\\'", "\\|", "\\&", "\\;", "\\<", "\\>"
#                      , "\\(", "\\)", "\\{", "\\}", "\\[", "\\]", "\\*", "\\?", "\\~", "\\#", "\\%", "\\^")
#   for (char in special_chars) {
#     x <- gsub(pattern = char, paste0('\\', substr(char, 3, 3)), x)
#   }
#   return(x)
# }
#


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





# Biology ____________________________________________________________ ----

GC_content <- function(string, len = nchar(string), pattern = c("G","C")) { # GC-content of a string (frequency of G and C letters among all letters).
  char.list <- stringr::str_split_fixed(string, pattern = "", n = nchar(string))
  tbl = table(factor(unlist(char.list), levels = c("A", "T", "G", "C")))
  sum(tbl[  pattern ]) / sum(tbl)
}



getSequences.DNAStringSet <- function(DNAStringSet.obj = dnaSS.HEK.s175239.1e4) { # For DNAStringSet objects
  lx <- length(DNAStringSet.obj)
  Sequences <- 1:lx
  for (i in 1:lx) {
    Sequences[i] <- as.character(DNAStringSet.obj[[i]])
    printEveryN(i = i)
  }
  Sequences
}

# _______________________________________________________________
PCA.percent.var.explained <- function(prcomp.res =  sPCA) { # Determine percent of variation associated with each PC. For Seurat see: seu.PC.var.explained().
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


# Plotting and Graphics ____________________________________________________________ ----

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



## Colors ____________________________________________________________ ----
richColors <- function(n = 3) { gplots::rich.colors(n) } # Alias for rich.colors in gplots



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




## Clustering heatmap tools ____________________________________________________________ ----

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
annot_col.create.pheatmap.vec <- function(data, annot_vec, annot_names = "Annot") { # For VECTORS. Auxiliary function for pheatmap. Prepares the 2 variables needed for "annotation_col" and "annotation_colors" in pheatmap
  stopifnot( length(annot_vec) == dim(data)[2] )
  namez = as.character(if (is.null(annot_names)) substitute(annot_vec) else annot_names)

  df = data.frame(x = annot_vec);
  # df[, 1] = as.character(df[, 1])
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
# _________________________________________________________________________________________________


getVennOverlaps <- function(lsvenn = list(A = sort(sample(LETTERS, 15)),
                                          B = sort(sample(LETTERS, 15)),
                                          C = sort(sample(LETTERS, 15)),
                                          D = sort(sample(LETTERS, 15)))
) {

  ItemsList <- gplots::venn(lsvenn, show.plot = FALSE)
  print(lengths(attributes(ItemsList)$intersections))
  return(attributes(ItemsList)$intersections)
}




# _________________________________________________________________________________________________
ww.randomize <- function(vec = nm.trunk) {
  old <- unique(vec)
  new <- sample(1:length(vec))
  as.numeric(CodeAndRoll2::translate(vec = vec, oldvalues = old, newvalues = new))
}

# _________________________________________________________________________________________________
append_non_na <- function(vec1.core, vec2.suffix) {
  stopifnot(length(vec1.core) == length(vec2.suffix))
  not_na <- !is.na(vec1.core)
  vec1.core[not_na] <- paste(vec1.core[not_na], vec2.suffix[not_na], sep = '.')
  return(vec1.core)
}


# _________________________________________________________________________________________________
date()
dateOK <- function() format(Sys.Date(), "%Y.%m.%d"); dateOK()
dateAndTime <- function() format(Sys.time(), "%Y_%m_%d-%H.%M"); dateAndTime()

backupRprofile <- function(dest_dir = "~/GitHub/pipatorium/R/Rprofile/Local/", backup.dir.create =F) {

  # Define the source file (assuming .Rprofile is in the home directory)
  source_file <- file.path(Sys.getenv("HOME"), ".Rprofile")

  # Check if the source file exists
  if (!file.exists(source_file)) {
    stop("The .Rprofile file does not exist in the home directory.")
    return()
  }

  # Create the destination directory if it does not exist
  if (!dir.exists(dest_dir)) {
    warning(paste(dest_dir, "
    destination directory does not exist!
    You can set backup.dir.create = TRUE"))
    if(backup.dir.create) dir.create(dest_dir, recursive = TRUE)
  }

  # Create the destination file name with date
  dest_file <- file.path(dest_dir, paste0("Rprofile.", dateOK()))

  # Copy the file
  file.copy(source_file, dest_file, overwrite = TRUE)

  # Confirmation message
  cat("Backup of .Rprofile created at:", dest_file, "\n")
}


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
