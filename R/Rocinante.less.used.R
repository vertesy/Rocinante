######################################################################
# Rocinante - LESS USED FUNCTIONS
######################################################################
# source('~/GitHub/Packages/Rocinante/R/Rocinante.less.used.R')
# rm(list = ls(all.names = TRUE)); try(dev.off(), silent = T)




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
getMemoryInfoSimple()

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


# _________________________________________________________________________________________________


# _________________________________________________________________________________________________



# _________________________________________________________________________________________________
