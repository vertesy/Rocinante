# Keep namespace-qualified calls and roxygen imports declared in DESCRIPTION.
r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
stopifnot(length(r_files) > 0, all(file.exists(r_files)))

namespace_packages <- function(path) {
  stopifnot(is.character(path), length(path) == 1)
  packages <- character()

  walk <- function(x) {
    stopifnot(is.language(x) || is.pairlist(x) || is.expression(x) || is.atomic(x))
    if (is.call(x) && identical(x[[1]], as.name("::"))) {
      packages <<- c(packages, as.character(x[[2]]))
    }
    if (is.call(x) && identical(x[[1]], as.name("require")) ||
      (is.call(x) && identical(x[[1]], as.name("library")))) {
      pkg_arg <- if (!is.null(x[["package"]])) x[["package"]] else x[[2]]
      if (is.symbol(pkg_arg) || is.character(pkg_arg)) {
        packages <<- c(packages, as.character(pkg_arg))
      }
    }
    if (is.recursive(x)) {
      lapply(as.list(x), walk)
    }
    invisible(NULL)
  }

  walk(parse(path, keep.source = FALSE))
  unique(packages)
}

roxygen_import_packages <- function(path) {
  stopifnot(is.character(path), length(path) == 1)
  lines <- readLines(path, warn = FALSE)
  imports <- grep("^#'\\s+@import(?:From)?\\s+", lines, value = TRUE)
  unique(sub("^#'\\s+@import(?:From)?\\s+([^ ]+).*$", "\\1", imports))
}

description <- read.dcf("DESCRIPTION")
declared_fields <- c("Depends", "Imports", "Suggests")
declared <- strsplit(description[1, declared_fields], "[,[:space:]]+") |>
  unlist(use.names = FALSE) |>
  sub("\\s*\\(.*$", "", x = _) |>
  setdiff(c("", "R", "base"))

used <- unique(c(
  unlist(lapply(r_files, namespace_packages), use.names = FALSE),
  unlist(lapply(r_files, roxygen_import_packages), use.names = FALSE)
))
standard <- rownames(installed.packages(
  priority = c("base", "recommended"),
  fields = "Priority"
))
missing <- setdiff(used, c(declared, standard))

if (length(missing)) {
  stop("Undeclared package dependencies: ", paste(sort(missing), collapse = ", "))
}
