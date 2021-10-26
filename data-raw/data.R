#' New York City Mafia Members and Connections in 1960s. 
#'
#' @description The dataset used come from Dr. Clio Andris and Dr. Daniel Della Posta's research of NYC Mafia members' connections. 
#' The dataset is originated from a 1960 investigatory dossier compiled by the U.S. Federal Bureau of Narcotics. 
#' The dataset is filtered down to NYC area 
#' A node in the dataset represents a geolocated Mafia member
#' An edge in the dataset represents connections between members who were "known criminal associates"
#' Read the following paper for more details:
#' Andris C, DellaPosta D, Freelin B N, Zhu X, Hinger B and Chen H (2021) To Racketeer Among Neighbors: Spatial Features of Criminal Collaboration in the American Mafia. International Journal of Geographical Information Science, DOI: 10.1080/13658816.2021.1884869
#'
#' @format The nodes data frame has 298 rows and the following 3 columns:
#' \describe{
#'   \item{label}{the name (string) of the Mafia member}
#'   \item{lon}{the longitude (float) of the Mafia member}
#'   \item{lat}{the latitude (float) of the Mafia member}
#' }
#' #' @format The edges data frame has 946 rows and the following 2 columns. The edges are undirected:
#' \describe{
#'   \item{Source}{the name (string) of one Mafia member}
#'   \item{Target}{the name (string) of the other connected Mafia member}
#' }

"NYCMafiaNodes"
"NYCMafiaEdges"
"MafiaNodes"
"MafiaEdges"