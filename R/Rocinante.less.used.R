######################################################################
# Rocinante - LESS USED FUNCTIONS
######################################################################
# source('~/GitHub/Packages/Rocinante/R/Rocinante.less.used.R')
# rm(list = ls(all.names = TRUE)); try(dev.off(), silent = T)



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





colSums.barplot <- function(df, col = "seagreen2", na_rm = TRUE, ...) { barplot(colSums(df, na.rm = na_rm), col = col, ...) } # Draw a barplot from ColSums of a matrix.


# _________________________________________________________________________________________________
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
#' @export
#' @examples
#' mem_info <- getMemoryInfo()
#' print(mem_info)


getMemoryInfoSimple <- function() {
  os_type <- Sys.info()["sysname"]
  print(os_type)


  if (os_type == "Windows") { # Implementation for Windows
    message("Windows not tested.")
    mem_used <- memory.size()
    mem_free <- memory.limit() - mem_used

  } else if (os_type == "Linux") { # Implementation for Linux
    mem_info <- system("free -m", intern = TRUE)
    mem_lines <- strsplit(mem_info, " +")[[2]]
    mem_used <- as.numeric(mem_lines[3])
    mem_free <- as.numeric(mem_lines[4])

  } else if (os_type == "Darwin") { # Implementation for macOS
    warning("Maybe does not work correctly on macOS yet.")
    mem_info <- system("vm_stat", intern = TRUE)
    print(mem_info)

    # Extract page size in bytes
    page_size_info <- mem_info[grep("page size of", mem_info)]
    page_size <- as.numeric(gsub(".*page size of ([0-9]+) bytes.*", "\\1", page_size_info))

    # Function to extract the number of pages
    extract_pages <- function(pattern) {
      value <- mem_info[grep(pattern, mem_info)]
      as.numeric(gsub(".*: +([0-9]+).*$", "\\1", value))
    }

    # Calculate free, inactive, and speculative pages
    pages_free <- extract_pages("Pages free")
    pages_inactive <- extract_pages("Pages inactive")
    pages_speculative <- extract_pages("Pages speculative")
    pages_active <- extract_pages("Pages active")
    pages_wired <- extract_pages("Pages wired down")

    # Add up in GBs
    mem_free <- (pages_free + pages_inactive + pages_speculative) * page_size / 1024
    mem_used <- (pages_active + pages_wired) * page_size / 1024

  } else {
    stop("Unsupported OS")
  }

  stopifnot(all(is.numeric(c(mem_used, mem_free))))
  return(ceiling(c(Used = mem_used, Free = mem_free)))

}
# getMemoryInfoSimple()

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
#' @export
#' @examples
#' plotMemoryUsage()


plotMemoryUsageSimple <- function() {
  require(ggplot2)

  mem_info <- getMemoryInfo()
  mem_df <- data.frame(Type = names(mem_info), Memory = mem_info)

  # Calculate total memory and the percentage for each type
  total_memory <- sum(mem_df$Memory)
  mem_df$Percentage <- (mem_df$Memory / total_memory) * 100

  # Operating system and current time/date
  os_info <- Sys.info()["sysname"]
  current_time <- format(Sys.time(), "%Y-%m-%d %H:%M")

  ggplot(mem_df, aes(x = "", y = Memory, fill = Type)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(aes(label = paste0(round(Percentage, 1), "%")),
              position = position_stack(vjust = 0.5), size = 3.5) +
    labs(title = "Memory Usage",
         subtitle = paste("Total Memory:", round(total_memory, 2), "GB"),
         caption = paste(os_info, current_time),
         y = "Memory (GB)", x = "") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set3")
}





# Plotting and Graphics ____________________________________________________________ ----


# qheatmap <- function(df, cluster_rows = F, cluster_cols = F,
#                      main = make.names(Stringendo::FixPlotName(substitute(df))), ...) {
#   pheatmap::pheatmap(mat = df, cluster_rows = cluster_rows, cluster_cols = cluster_cols, main = main, ...)
# }


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
  as.numeric(CodeAndRoll2::translate(vec = vec, old = old, new = new))
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



# make_bash_compatible <- function(x) {
#   special_chars <- c("\\$", "\\\\", "\\ ", "\\\"", "\\'", "\\|", "\\&", "\\;", "\\<", "\\>"
#                      , "\\(", "\\)", "\\{", "\\}", "\\[", "\\]", "\\*", "\\?", "\\~", "\\#", "\\%", "\\^")
#   for (char in special_chars) {
#     x <- gsub(pattern = char, paste0('\\', substr(char, 3, 3)), x)
#   }
#   return(x)
# }
#


# _________________________________________________________________________________________________


# _________________________________________________________________________________________________



# _________________________________________________________________________________________________
