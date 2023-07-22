# Define the function
parse_roxygen_simple <- function(file, output_file) {
  # Read the file as a vector of lines
  lines <- readLines(file)

  # Find the lines containing function names
  function_lines <- grep("<- function", lines, value = TRUE)

  # Find the lines containing @title
  title_lines <- grep("@title", lines, value = TRUE)

  # Find the lines containing @description
  description_lines <- grep("@description", lines, value = TRUE)

  # Extract the function names
  function_names <- gsub("\\s*<-.*$", "", function_lines)

  # Extract the titles
  titles <- gsub("^.*@title\\s*", "", title_lines)

  # Extract the descriptions
  descriptions <- gsub("^.*@description\\s*", "", description_lines)

  # Open a connection to the output file
  file_conn <- file(output_file, open = "w")

  cat("## List of Functions\n", file = file_conn)

  # Write each function name, title, and description to the output file
  for (i in seq_along(function_names)) {
    cat(paste0("- ## `", function_names[i], "()`\n"), file = file_conn)
    cat(paste0(titles[i], ". ", descriptions[i], "\n\n"), file = file_conn)
  }

  # Close the connection
  close(file_conn)

  print(paste("Output written to", output_file))
}

# Use the function
# parse_roxygen_simple("~/GitHub/Packages/CodeAndRoll2/R/CodeAndRoll2.R", "output.md")


# __________________________________________________________________________________________
parse_roxygen <- function(file
                          , output_file = "~/Downloads/List.of.Functions.md"
                          , write_title_field = T) {
  # Read the file as a vector of lines
  lines <- readLines(file)

  # Initialize empty vectors to store function names, titles and descriptions
  function_names <- character(0)
  titles <- character(0)
  descriptions <- character(0)

  # Initialize temporary variables
  current_function_name <- ""
  current_title <- ""
  current_description <- ""
  in_description <- FALSE

  for (line in lines) {

    # Detect the start of a function Roxygen skeleton
    if (grepl("^#' @title", line)) {
      # Extract the title
      current_title <- sub("^#' @title\\s*", "", line)
      next
    }

    # Detect the start of a description
    if (grepl("^#' @description", line)) {
      # Extract the start of the description
      current_description <- sub("^#' @description\\s*", "", line)
      in_description <- TRUE
      next
    }

    # Handle lines within the description
    if (in_description && grepl("^#'", line) && !grepl("^#' @param|^#' @export|^#' @returns", line)) {
      # Continue the description
      current_description <- paste0(current_description, " ", sub("^#'", "", line))
      next
    }

    # Detect the end of a description or function Roxygen skeleton
    if (in_description && (grepl("^#' @param|^#' @export|^#' @returns", line) || grepl("<- function", line))) {
      # End the description
      in_description <- FALSE
      function_names <- c(function_names, current_function_name)
      titles <- c(titles, current_title)
      descriptions <- c(descriptions, current_description)
      current_function_name <- ""
      current_title <- ""
      current_description <- ""
      next
    }

    # Detect a function definition
    if (grepl("<- function|<-function", line)) {
      # Extract the function name
      current_function_name <- gsub("\\s*<-.*$", "", line)
    }

  }

  # Open a connection to the output file
  file_conn <- file(output_file, open = "w")

  cat("## List of Functions\n", file = file_conn)
  cat(paste0("Updated: ", format(Sys.time(), "%Y/%m/%d %H:%M"),"\n"), file = file_conn)

  # Write each function name, title, and description to the output file
  for (i in seq_along(function_names)) {
    if (i==1) next
    cat(paste0("- #### ", i-1,' `', function_names[i], "()`\n"), file = file_conn)

    if (write_title_field) {
      cat(paste0(titles[i-1], ". ", descriptions[i-1], "\n\n"), file = file_conn)
    } else {
      cat(paste0(descriptions[i-1], "\n\n"), file = file_conn)
    }

  }

  # Close the connection
  close(file_conn)

  print(paste("Output written to", output_file))
}


# parse_roxygen("~/GitHub/Packages/CodeAndRoll2/R/CodeAndRoll2.R"
              # , write_title_field = F)
