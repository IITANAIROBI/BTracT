suppressPackageStartupMessages(library(shiny))
                                
enableBookmarking(store = "url")

# Tabs
tab_files <- list.files(path = "tabs/ui", full.names = T, recursive = T)
suppressMessages(lapply(tab_files, source))

source("data/update_banana_data.R")

