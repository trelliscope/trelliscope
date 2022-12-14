% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/display_write.R
\name{write_display}
\alias{write_display}
\title{Write the contents of a display}
\usage{
write_display(disp, force_write = FALSE, jsonp = TRUE)
}
\arguments{
\item{disp}{A trelliscope display object created with \code{\link[=trelliscope]{trelliscope()}}.}

\item{force_write}{Should the panels be forced to be written even if they
have already been written?}

\item{jsonp}{If true, app files are written as "jsonp" format, otherwise
"json" format. The "jsonp" format makes it possible to browse a
trelliscope app without the need for a web server.}
}
\description{
Write the contents of a display
}
