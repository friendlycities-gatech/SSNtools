#----------------------------------------------#
# -----Process input data to correct format ---#
#----------------------------------------------#

# load required packages ----
if (!require("pacman")) install.packages("pacman") 
pacman::p_load(magrittr, dplyr, usethis, data.table, here)

#------Each node is stored a named list ------#
#------nodes is a list of named list ---------#
#=-----The process returns a list of named lists with the named key as label
example_nodes <- read.csv(here::here("data-raw", "Filtered_MafiaNodes.csv"))
example_edges <- read.csv(here::here("data-raw", "Filtered_MafiaEdges.csv"))

usethis::use_data(example_nodes, overwrite = TRUE)
usethis::use_data(example_edges, overwrite = TRUE)

