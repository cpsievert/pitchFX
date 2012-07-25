\name{getPlayerURLs}
\alias{getPlayerURLs}
\title{Generate Player URLs.}
\usage{
  getPlayerURLs(data)
}
\arguments{
  \item{data}{Dataframe with the relevant "url" column.}
}
\value{
  Returns a list of URLs for each player in the data set.
}
\description{
  This function will create a URL for each player in a
  specfic game. It is good practice to enter an 'atbat'
  table, but it is also acceptable to enter a 'game' table.
  In either case, the data frame should have a 'url' column
  with a suffix of "~/inning/inning_all.xml".
}

