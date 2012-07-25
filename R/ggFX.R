#' Animate Pitch F/X
#' 
#' Pitch trajectories animated on a two-dimensional plot.
#' 
#' More flexible animation.
#' 
#' @param data pitch F/X data to be visualized.
#' @param layer list of ggplot2 modifications to the plot
#' @param time.interval time interval between flushing of plot
#' @return ggplot2 object
#' @export

ggFX <- function(data, layer=list(), time.interval = 0.01){ 
  #Add descriptions to pitch_types
  p.types <- cbind(pitch_type=c("SI", "FF", "IN", "SL", "CU", "CH", "FT", "FC", "PO", "KN", "FS", "FA", NA, "FO"),
                   pitch_types=c("Sinker", "Fastball (four-seam)", "Intentional Walk", "Slider", "Curveball", "Changeup", 
                                 "Fastball (two-seam)", "Fastball (cutter)", "Pitchout", "Knuckleball", "Fastball (split-finger)",
                                 "Fastball", "Unknown", "Fastball (FO?)"))
  pitchFX <- merge(data, p.types, by = c("pitch_type"), sort = T)
  idx <- c("x0", "y0", "z0", "vx0", "vy0", "vz0", "ax", "ay", "az")
  snapshot <- pitchFX[complete.cases(pitchFX[,idx]),] #get rid of records with missing parameters
  for (i in idx) 
    snapshot[,i] <- as.numeric(snapshot[,i])
  t <- rep(0, dim(snapshot)[1]) #Initial time (at point of release)
  snapshot$y <- rep(50, dim(snapshot)[1]) #Initial distance from home plate
  while (any(as.numeric(snapshot$y) > 1.417)) { 
    snapshot$x <- with(snapshot, x0 + vx0*t + .5*ax*t^2) #Inside/Outside location (at time t)
    snapshot$y <- with(snapshot, pmax(1.417, y0 + vy0*t + .5*ay*t^2)) #Distance from home plate
    snapshot$z <- with(snapshot,z <- z0 + vz0*t + .5*az*t^2) #Height from ground
    ani.pause(time.interval) #Does this make sense?
    #browser()
    print(ggplot(data = snapshot, mapping = aes(x = x, y = z))
          + xlim(-3.5, 3.5) + xlab("Inside/Outside") + ylim(0, 10) + ylab("Height from Ground")
          + scale_size(guide="none") + scale_alpha(guide="none") #suppress legends for size and alpha
          + layer)
    q <- as.numeric(snapshot$y > 1.417)
    t <- t + q*time.interval #Only increment time for those that have yet to reach home plate
  }
  return(head(snapshot))
}