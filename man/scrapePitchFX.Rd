\name{scrapePitchFX}
\alias{scrapePitchFX}
\title{Scrape Pitch F/X Data}
\usage{
  scrapePitchFX(start = "2012-01-01", end = Sys.Date())
}
\arguments{
  \item{start}{date "yyyy-mm-dd" to commence scraping of
  pitch F/X data}

  \item{end}{date "yyyy-mm-dd" to terminate scraping pitch
  F/X data}
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

#pitches <- data$pitch
#atbats <- data$atbat
#pitchFX <- join(pitches, atbats, by = c("num", "url"))

#unique(pitchFX$pitch_type)
#pitchFX <- pitchFX[pitchFX$pitch_type == c("SL","CU"), ]
}

