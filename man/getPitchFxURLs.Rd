\name{getPitchFxURLs}
\alias{getPitchFxURLs}
\title{Generate Pitch F/X URLs.}
\usage{
  getPitchFxURLs(frame)
}
\arguments{
  \item{frame}{data frame with a column containing the "id"
  attribute from the "game" node.}
}
\value{
  Returns a list of URLs that should contain all available
  pitch F/X info.
}
\description{
  This function will create the relevant URLs for each
  game. It should not be used until one has a table of
  desired games. It takes on a data object containing "id"
  attributes from the "game" node of "~/miniscoreboard.xml"
  files. Entries should have the following format:
  "yyyy/mm/dd/game-id"
}

