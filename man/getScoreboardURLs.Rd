\name{getScoreboardURLs}
\alias{getScoreboardURLs}
\title{Generate miniscoreboard urls for a specified number of dates.}
\usage{
  getScoreboardURLs(start, end)
}
\arguments{
  \item{start}{date (yyyy-mm-dd) to commence scraping of
  pitch F/X data}

  \item{end}{date (yyyy-mm-dd) to terminate scraping pitch
  F/X data}
}
\value{
  Returns a list of "miniscoreboard" URLs.
}
\description{
  This function will create a URL for each day between
  \code{start} and \code{end}. Not every URL will exist.
  For those that do, the file will contain information on
  every game played during that day.
}

