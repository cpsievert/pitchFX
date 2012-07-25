\name{scrapePitchFX}
\alias{scrapePitchFX}
\title{Scrape Pitch F/X Data}
\usage{
  scrapePitchFX(start = "2012-01-01", end = Sys.Date(),
    include.games = T,
    tables = list(atbat = fields$atbat, pitch = fields$pitch))
}
\arguments{
  \item{start}{date "yyyy-mm-dd" to commence scraping of
  pitch F/X data}

  \item{end}{date "yyyy-mm-dd" to terminate scraping pitch
  F/X data}

  \item{include.games}{whether or not to include a game
  table in the output}

  \item{tables}{XML nodes to be parsed into a data frame}
}
\value{
  Returns a list containing two different data frames. The
  larger data frame contains data on every pitch thrown
  (pitch F/X). The smaller one contains data on every
  atbat.
}
\description{
  This function scrapes data from the MLB website.
}
\details{
  Details go here.
}
\examples{
#ptm <- proc.time()
#data <- scrapePitchFX(start = "2011-05-01", end = "2011-05-01")
#ptm

#games <- data$game
#atbats <- data$atbat
#pitches <- data$pitch

#pitchFX <- join(pitches, atbats, by = c("num", "url"))

#unique(pitchFX$pitch_type)
#pitchFX <- pitchFX[pitchFX$pitch_type == c("SL","CU"), ]
}

