\name{ggFX}
\alias{ggFX}
\title{Animate Pitch F/X}
\usage{
  ggFX(data, geom = "point", stat = NULL,
    color = "pitch_type", size = 10 * -y, alpha = I(0.5),
    facets = NULL, shape = NULL, layer = NULL,
    time.interval = 0.01)
}
\arguments{
  \item{data}{pitch F/X data to be visualized.}

  \item{layer}{list of ggplot2 modifications to the plot}

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
  More flexible animation.
}
\examples{
#ggFX(data = pitchFX, color = "pitch_type", facets = "p_throws", time.interval = 0.01)
#Possible choices for color and facets. Does it make sense to have one
#variable determine both color and facet?
#c("pitcher", "batter", "p_throws", "stand", "zone", "type", "pitch_type", "event")
}

