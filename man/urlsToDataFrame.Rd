\name{urlsToDataFrame}
\alias{urlsToDataFrame}
\title{Function for parsing urls and specified nodes into data frames}
\usage{
  urlsToDataFrame(urls,
    tables = list(atbat = NULL, pitch = NULL))
}
\arguments{
  \item{urls}{set of urls for parsing}

  \item{tags}{list of character vectors containing field
  names for each table}

  \item{nodes}{XML node(s) of interest that deem scope of
  table}
}
\value{
  Returns a data frames if the length of tables is one.
  Otherwise, it returns a list of data frames.
}
\description{
  The intended use for this function is to scrape an
  "atbats" table and the corresponding "pitch" (ie, Pitch
  F/X) table for the specified set of URLs. In fact, this
  function is used as the main component of
  \code{scrapePitchFX}. There is added flexibility that
  allows one to specify nodes of interest other than
  "atbat" and "pitch". \bold{Important: You must have
  "atbat" AND "pitch" nodes if you want to identify who
  threw a particular pitch. Also, the order of field names
  should match the order of nodes.}
}
\examples{
#If it isn't currently baseball season, consider changing the dates below:
#Also, this is a small scaled example. Visit my website if you would like to see how to
#build a current and complete database.
#mini.urls <- getScoreboardURLs(first.date = Sys.Date() - 10, last.date = Sys.Date())
#game.urls <- getPitchFxURLs(mini.urls)
#data <- urlsToDataFrame(urls = game.urls)
#atbats <- data$atbat
#pitches <- data$pitch
}

