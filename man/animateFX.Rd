\name{animateFX}
\alias{animateFX}
\title{Animate Pitch F/X}
\usage{
  animateFX(data, color, facets, time.interval = 0.01)
}
\arguments{
  \item{data}{pitch F/X data to be visualized.}

  \item{color}{variables within the data set that determine
  different colors}

  \item{facets}{variables within the data set that
  determine different facets}

  \item{time.interval}{time interval between flushing of
  plot}
}
\value{
  ggplot2 object
}
\description{
  Pitch trajectories animated on a two-dimensional plot.
}
\details{
  Details to go here.
}
\examples{
animateFX(data = pitchFX, color = "pitch_type", facets = "p_throws",
time.interval = 0.01)
#Possible choices for color and facets. Does it make sense to have one
#variable determine both color and facet?
#c("pitcher", "batter", "p_throws", "stand", "zone", "type", "pitch_type",
#"event")
}

