% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{file2df}
\alias{file2df}
\title{Read a text file made of observations}
\usage{
file2df(file.in, header, skip, cnames)
}
\arguments{
\item{file.in}{A character. Path to a text file with data}

\item{header}{A logical. Does the first column contain column names?}

\item{skip}{A numeric. Number of lines to skip}

\item{cnames}{A vector character. The names of the columns}
}
\value{
A data.frame
}
\description{
Read a text file made of observations
}
