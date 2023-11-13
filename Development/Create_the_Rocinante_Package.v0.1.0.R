######################################################################################################
# Create_the_Rocinante_Package.R
######################################################################################################
# source("/Users/abel.vertesy/GitHub/Packages/Rocinante/Development/Create_the_Rocinante_Package.R")
rm(list = ls(all.names = TRUE));
try(dev.off(), silent = TRUE)
# install.packages("devtools")
# Functions ------------------------
# try (source('~/GitHub/Packages/CodeAndRoll/CodeAndRoll.R'),silent= FALSE)

# # irequire("devtools")
# # install_version("devtools", version = "2.0.2", repos = "http://cran.at.r-project.org")
# irequire("devtools")
# irequire("roxygen2")
# irequire("stringr")


# Setup ------------------------
package.name <- 	"Rocinante"
package.version <- "0.1.0"
setwd("~/GitHub/Packages/")

RepositoryDir <- paste0("~/GitHub/Packages/", PackageName, "/")
fname <-	paste0(PackageName, ".R")
Package_FnP <-		paste0(RepositoryDir, "R/", fname)

BackupDir <- "~/GitHub/Packages/Rocinante/Development/"
dir.create(BackupDir)

# devtools::use_package("vioplot")
DESCRIPTION <- list("Title" = "Rocinante is a set of helper functions."
    , "Author" = person(given = "Abel", family = "Vertesy", email = "abel.vertesy@imba.oeaw.ac.at", role =  c("aut", "cre") )
    , "Authors@R" = 'person(given = "Abel", family = "Vertesy", email = "a.vertesy@imba.oeaw.ac.at", role =  c("aut", "cre") )'
    , "Description" = "Rocinante is a set of R functions to help coding in R."
    , "License" = "GPL-3 + file LICENSE"
    , "Version" = package.version
    # , "Version" = "4.0.0"
    , "Packaged" =  Sys.time()
    # , "Repository" =  "CRAN"
    , "Imports" = "tidyverse, cowplot, ggpubr, stats, methods, sm, graphics, grDevices, gplots, RColorBrewer, sessioninfo, MarkdownReports"
    # , "Suggests" = ""
    , "BugReports"= "https://github.com/vertesy/Rocinante/issues"
)


setwd(RepositoryDir)
if ( !dir.exists(RepositoryDir) ) { create(path = RepositoryDir, description = DESCRIPTION, rstudio = TRUE)
} else {
    getwd()
    try(file.remove(c("DESCRIPTION","NAMESPACE", "Rocinante.Rproj")))
    create_package(path = RepositoryDir, fields = DESCRIPTION)
}


# go and write fun's ------------------------------------------------------------------------
# file.edit(Package_FnP)

# Create Roxygen Skeletons ------------------------
# RoxygenReady(Package_FnP)

# replace output files ------------------------------------------------
BackupOldFile <-	paste0(BackupDir, "Development", ".bac", print = FALSE)
AnnotatedFile <-	paste0(BackupDir, "Development", ".annot.R", print = FALSE)
file.copy(from = Package_FnP, to = BackupOldFile, overwrite = TRUE)
# file.copy(from = AnnotatedFile, to = Package_FnP, overwrite = TRUE)

# Manual editing of descriptors ------------------------------------------------
# file.edit(Package_FnP)

# Compile a package ------------------------------------------------
setwd(RepositoryDir)
getwd()
devtools::document()


# Install your package ------------------------------------------------
# # setwd(RepositoryDir)
devtools::install(RepositoryDir)
# require("Rocinante")
# # remove.packages("Rocinante")
# # Test your package ------------------------------------------------
# help("wplot")
# cat("\014")
# devtools::run_examples()

{
  "update cff version"
  citpath <- paste0(RepositoryDir, 'CITATION.cff')
  xfun::gsub_file(file = citpath, perl = T
                  , "^version: v.+", paste0("version: v", package.version))
}

# Test if you can install from github ------------------------------------------------
# devtools::install_github(repo = "vertesy/Rocinante")

# require("Rocinante")

# Clean up if not needed anymore ------------------------------------------------
# View(installed.packages())
# remove.packages("Rocinante")

check(RepositoryDir, cran = TRUE)
# as.package(RepositoryDir)
#
#
# # source("https://install-github.me/r-lib/desc")
# # library(desc)
# # desc$set("Rocinante", "foo")
# # desc$get(Rocinante)
#
#
# system("cd ~/GitHub/Rocinante/; ls -a; open .Rbuildignore")
#
