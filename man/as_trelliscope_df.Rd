% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as_trelliscope.R
\name{as_trelliscope_df}
\alias{as_trelliscope_df}
\title{Instantiate a trelliscope data frame}
\usage{
as_trelliscope_df(
  df,
  name = NULL,
  description = name,
  key_cols = NULL,
  tags = NULL,
  path = NULL,
  force_plot = FALSE,
  key_sig = NULL,
  order = 0,
  jsonp = TRUE
)
}
\arguments{
\item{df}{A data frame that contains the metadata of the display as well as
a column that indicate the panels to be displayed.}

\item{name}{Name of the trelliscope display.}

\item{description}{Description of the trelliscope display.}

\item{key_cols}{Variable names in \code{df} that uniquely define a row of the
data. If not supplied, an attempt will be made to infer them.}

\item{tags}{Optional vector of tag names to identify the display in the
case that there are many to search through.}

\item{path}{Directory in which to place the trelliscope display when
it is written using \code{\link[=write_trelliscope]{write_trelliscope()}}.}

\item{force_plot}{Should the panels be forced to be plotted, even if they
have already been plotted and have not changed since the previous plotting?}

\item{key_sig}{A string "signature" that represents the panels for this
display. This should not be specified unless you know what you are doing.}

\item{order}{If there will be multiple displays in the same directory, this
can be used to specify the order in which they should be listed. The
display with the lowest order will be shown on load.}

\item{jsonp}{If true, app data files are written as "jsonp" format, otherwise
"json" format. The "jsonp" format makes it possible to browse a
trelliscope app without the need for a web server and is set to TRUE by
default. Use "json" if you are deploying to a web server that doesn't
work well with "jsonp".}
}
\description{
Instantiate a trelliscope data frame
}
\examples{
# Use `as_trelliscope_df()` to convert panel metadata to a special
# trelliscope data frame
library(ggplot2)
library(dplyr)

panel_dat <- (
  ggplot(gap, aes(year, life_exp)) +
    geom_point() +
    facet_panels(vars(country, continent))
  ) |>
    as_panels_df()

meta_dat <- gap |>
  group_by(country, continent) |>
  summarise(
    mean_life_exp = mean(life_exp),
    min_life_exp = min(life_exp),
    max_life_exp = max(life_exp),
    mean_gdp = mean(gdp_percap),
    .groups = "drop"
  )

joined_dat <- left_join(panel_dat, meta_dat) |>
  as_trelliscope_df(name = "life_expectancy", path = tempfile())

\dontrun{
view_trelliscope(joined_dat)
}

# You can also use `as_trelliscope_df()` on datasets that have links to
# images instead of conventional ggplot objects
\dontrun{
}
}
