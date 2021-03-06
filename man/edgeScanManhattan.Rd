% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/oxy-SSNtools.R
\name{edgeScanManhattan}
\alias{edgeScanManhattan}
\title{edgeScanManhattan}
\usage{
edgeScanManhattan(
  nodes,
  edges,
  radius,
  min = 3,
  weighted = FALSE,
  bipartite = FALSE
)
}
\arguments{
\item{nodes}{PARAM_DESCRIPTION}

\item{edges}{PARAM_DESCRIPTION}

\item{radius}{PARAM_DESCRIPTION}

\item{min}{PARAM_DESCRIPTION}

\item{weighted}{(optional) boolean value of whether a weighted column has been included.}

\item{bipartite}{(optional) boolean value of whether the data is a bipartite network}
}
\value{
a list of two dataframes. The first R datafrmae contains a column of node label, and a column of heat associated with the node. The second R dataframe contains the edge pairs and a boolean column indicating whether the edge is within the scanning window.
}
\description{
FUNCTION_DESCRIPTION
}
\details{
DETAILS
}
