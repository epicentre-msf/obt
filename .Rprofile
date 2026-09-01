# R reads only the first .Renviron / .Rprofile it finds, so a project-level file
# shadows the user-level one. Load the personal files explicitly, then let renv
# activate last so it keeps the final say over .libPaths().
if (file.exists("~/.Renviron")) readRenviron("~/.Renviron")
if (file.exists("~/.Rprofile")) source("~/.Rprofile")

source("renv/activate.R")
