#----------------------------------------------#
# -----Process input data to correct format ---#
#----------------------------------------------#

# load required packages ----
if (!require("pacman")) install.packages("pacman") 
pacman::p_load(usethis, here)

#------Each node is stored a named list ------#
#------nodes is a list of named list ---------#
#=-----The process returns a list of named lists with the named key as label
NYCMafiaNodes <- read.csv(here::here("data-raw", "NYCMafiaNodes.csv"))
NYCMafiaEdges <- read.csv(here::here("data-raw", "NYCMafiaEdges.csv"))

usethis::use_data(NYCMafiaNodes, overwrite = TRUE)
usethis::use_data(NYCMafiaEdges, overwrite = TRUE)

DTMafiaNodes <- read.csv(here::here("data-raw", "DTMafiaNodes.csv"))
DTMafiaEdges <- read.csv(here::here("data-raw", "DTMafiaEdges.csv"))

usethis::use_data(DTMafiaNodes, overwrite = TRUE)
usethis::use_data(DTMafiaEdges, overwrite = TRUE)

MafiaNodes <- read.csv(here::here("data-raw", "MafiaNodes.csv"))
MafiaEdges <- read.csv(here::here("data-raw", "MafiaEdges.csv"))

usethis::use_data(MafiaNodes, overwrite = TRUE)
usethis::use_data(MafiaEdges, overwrite = TRUE)

POINodes <- read.csv(here::here("data-raw", "POINodes.csv"))
POIEdges <- read.csv(here::here("data-raw", "POIEdges.csv"))

usethis::use_data(POINodes, overwrite = TRUE)
usethis::use_data(POIEdges, overwrite = TRUE)

EmergencyNodes <- read.csv(here::here("data-raw", "EmergencyNodes.csv"))
EmergencyEdges <- read.csv(here::here("data-raw", "EmergencyEdges.csv"))

usethis::use_data(EmergencyNodes, overwrite = TRUE)
usethis::use_data(EmergencyEdges, overwrite = TRUE)

