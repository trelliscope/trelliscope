% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pipe_functions.R
\name{add_view}
\alias{add_view}
\title{Add a view specification to a trelliscope display}
\usage{
add_view(trdf, name, ...)
}
\arguments{
\item{trdf}{A trelliscope data frame created with \code{\link[=as_trelliscope_df]{as_trelliscope_df()}}
or a data frame which will be cast as such.}

\item{name}{The name of the view.}

\item{...}{Any number of state specifications that define the view. These
can be specified with any of \code{\link[=state_layout]{state_layout()}}, \code{\link[=state_labels]{state_labels()}},
\code{\link[=state_sort]{state_sort()}}, \code{\link[=filter_string]{filter_string()}}, \code{\link[=filter_range]{filter_range()}}.}
}
\description{
Add a view specification to a trelliscope display
}