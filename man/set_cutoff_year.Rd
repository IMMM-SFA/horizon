% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_prep.R
\name{set_cutoff_year}
\alias{set_cutoff_year}
\title{set_cutoff_year}
\usage{
set_cutoff_year(x, cutoff_year = NULL)
}
\arguments{
\item{x}{tibble of storage, release, inflow data with water_year specified}

\item{cutoff_year}{a year}
}
\value{
filtered data
}
\description{
set_cutoff_year
}
\details{
filters data post cutoff year only
}
