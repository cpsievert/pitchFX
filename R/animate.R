#' Animate Pitch F/X
#' 
#' Pitch trajectories animated on a two-dimensional plot.
#' 
#' Details to go here.
#' 
#' @param data pitch F/X data to be visualized.
#' @param color variables within the data set that determine different colors
#' @param facets variables within the data set that determine different facets
#' @param time.interval time interval between flushing of plot
#' @return ggplot2 object
#' @export
#' @examples
#' animateFX(data = pitchFX, color = "pitch_type", facets = "p_throws",
#' time.interval = 0.01)
#' #Possible choices for color and facets. Does it make sense to have one 
#' #variable determine both color and facet?
#' #c("pitcher", "batter", "p_throws", "stand", "zone", "type", "pitch_type",
#' #"event")

animateFX <- function(data, color, facets, time.interval = 0.01){ 
  #Add descriptions to pitch types
  p.types <- cbind(pitch_type=c("SI", "FF", "IN", "SL", "CU", "CH", "FT", "FC", "PO", "KN", "FS", "FA", NA, "FO"),
                   pitch_types=c("Sinker", "Fastball (four-seam)", "Intentional Walk", "Slider", "Curveball", "Changeup", 
                                 "Fastball (two-seam)", "Fastball (cutter)", "Pitchout", "Knuckleball", "Fastball (split-finger)",
                                 "Fastball", "Unknown", "Fastball (FO?)"))
  pitchFX <- merge(data, p.types, by = c("pitch_type"), sort = T)
  #Add names for pitchers and batters!?!
  holder <- !is.na(as.numeric(pitchFX$x0) & as.numeric(pitchFX$y0) & as.numeric(pitchFX$z0)) # Remove NA's, save "nine parameters" as numeric as well as pitch type and pitcher name
  x0 <- as.numeric(pitchFX$x0[holder])
  y0 <- as.numeric(pitchFX$y0[holder])
  z0 <- as.numeric(pitchFX$z0[holder])
  vx0 <- as.numeric(pitchFX$vx0[holder])
  vy0 <- as.numeric(pitchFX$vy0[holder])
  vz0 <- as.numeric(pitchFX$vz0[holder])
  ax <- as.numeric(pitchFX$ax[holder])
  ay <- as.numeric(pitchFX$ay[holder])
  az <- as.numeric(pitchFX$az[holder])
  
  colors <- as.data.frame(pitchFX[holder, color])
  names(colors) <- color
  #return(names(colors))
  
  splits <- as.data.frame(pitchFX[holder, facets])
  names(splits) <- facets
  #return(names(facets))
  
  t <- rep(0, length(type)) #Initial time (at point of release)
  y <- rep(50, length(type)) #Initial distance from home plate
  while (any(as.numeric(y) > 1.417)) { 
    x <- x0 + vx0*t + .5*ax*t^2 #Inside/Outside location (at time t)
    y <- y0 + vy0*t + .5*ay*t^2 #Distance from home plate
    y[y < 1.417] <- 1.417
    z <- z0 + vz0*t + .5*az*t^2 #Height from ground
    snapshot <- data.frame(x, y, z, colors, splits)
    ani.pause(time.interval) #Does this make sense?
    print(qplot(x = x, y = z, data = snapshot, alpha=I(0.5), size=10*-y, 
                colour=pitch_type, facets=p_throws ~ ., #How do I automate these options?
                xlim=c(-3.5, 3.5), xlab="Inside/Outside", ylim=c(0, 10), ylab="Height from Ground") + scale_size(legend = FALSE))
    q <- as.numeric(y > 1.417)
    t <- t + q*time.interval #Only increment time for those that have yet to reach home plate
  }
  return(head(snapshot))
}