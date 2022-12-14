% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pipe_functions.R
\name{add_meta_labels}
\alias{add_meta_labels}
\title{Specify labels for meta variables}
\usage{
add_meta_labels(disp, ...)
}
\arguments{
\item{disp}{A trelliscope display object created with \code{\link[=trelliscope]{trelliscope()}}.}

\item{...}{A named set of labels, where each name must correspond to one
of the variables in the dataset}
}
\description{
Specify labels for meta variables
}
\details{
This function can be useful if you don't want to go to the trouble
of explicitly setting meta variable definitions but still want variable
descriptions.
}
