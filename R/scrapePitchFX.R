#' Scrape Pitch F/X Data
#'
#' This function scrapes data from the MLB website.
#'
#' Details go here.
#' 
#' @param start date "yyyy-mm-dd" to commence scraping of pitch F/X data
#' @param end date "yyyy-mm-dd" to terminate scraping pitch F/X data
#' @param game.data data set with game URLs
#' @return Returns a list containing two different data frames. The larger data frame contains data on every pitch thrown (pitch F/X). The smaller one contains data on every atbat.
#' @export
#' @examples
#' #ptm <- proc.time()
#' #data <- scrapePitchFX(start = "2011-05-01", end = "2011-05-01")
#' #ptm
#'
#' #pitches <- data$pitches
#' #atbats <- data$atbats
#' #pitchFX <- join(pitches, atbats, by = c("num", "url"))
#'
#' #unique(pitchFX$pitch_type)
#' #pitchFX <- pitchFX[pitchFX$pitch_type == c("SL","CU"), ]

#How do I pass a data frame (ie, all_games) through the function?
scrapePitchFX <- function(start = "2012-01-01", end = Sys.Date(), game.data = all_games) { #make fields flexible?
  start <- as.POSIXct(start)
  end <- as.POSIXct(end)
  if (year(start) < 2005) stop("Not only is pitchFX data not avaliable before 2008, data on each game isn't complete until 2005")
  if (year(start) < 2008) stop("Warning: pitchFX data wasn't recorded consistently until 2008. Please consider a later start date.")
  if (end > as.POSIXct(Sys.Date())) stop("Sorry, I can't scrape data on the future!")
  all.games <- data(game.data)
  some.games <- subset(all.games, date >= start & date <= end)
  urls <- some.games[, "url"]
  p <- c("ax", "ay", "az", "break_angle", "break_length", "break_y", "cc", "des", 
                "end_speed", "id", "mt", "nasty", "on_1b", "on_2b", "on_3b", "pfx_x", "pfx_z", "pitch_type", 
                "px", "pz", "spin_dir", "spin_rate", "start_speed", "sv_id", "sz_bot", "sz_top", 
                "tfs", "tfs_zulu", "type", "type_confidence", "vx0", "vy0", "vz0", 
                "x", "x0", "y", "y0", "z0", "zone")
  ab <- c("away_team_runs", "b", "b_height", "batter", "des", "event", "event2", "event3", 
                "event4", "home_team_runs", "num", "o", "p_throws", "pitcher", "s", "score", "stand", "start_tfs", "start_tfs_zulu")
  data <- urlsToDataFrame(urls = urls, tables = list(atbat = ab, pitch = p))
  return(data) #Should I write the table to the wd? Would this help with memory issue? 
}
