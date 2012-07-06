#' Pitch F/X package
#' 
#' @docType package
#' @name pitchFX
#' @author Carson Sievert
#' @example 
#'
#' ptm <- proc.time()
#' data <- scrapePitchFX(start = "2011-05-01", end = "2011-05-01")
#' ptm
#'
#' pitches <- data$pitches
#' atbats <- data$atbats
#' pitchFX <- join(pitches, atbats, by = c("num", "url"))
#'
#' unique(pitchFX$pitch_type)
#' pitchFX <- pitchFX[pitchFX$pitch_type == c("SL","CU"), ]
#'
#' animateFX(data = pitchFX, color = "pitch_type", facets = "p_throws",
#' time.interval = 0.01)

#' #Possible choices for color and facets. Does it make sense to have one variable determine both color and facet?
#' #c("pitcher", "batter", "p_throws", "stand", "zone", "type", "pitch_type", "event")
NULL