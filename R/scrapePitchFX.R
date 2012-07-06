#' Scrape Pitch F/X Data
#'
#' this function goes out to the website and pulls data
#'
#' more elaborate description here
#' @param start date "yyyy-mm-dd" to commence scraping of pitch F/X data
#' @param start date "yyyy-mm-dd" to terminate scraping pitch F/X data
#' export

scrapePitchFX <- function(start = "2012-01-01", end = Sys.Date()) { #make fields flexible?
  first.date <- paste("('", start, "')", sep = "")
  if (year(start) < 2005) stop("Not only is pitchFX data not avaliable before 2008, data on each game isn't complete until 2005")
  if (year(start) < 2008) stop("Warning: pitchFX data wasn't recorded consistently until 2008. Please consider a later start date.")
  last.date <- paste("('", end, "')", sep = "")
  if (end > Sys.Date()) stop("Sorry, I can't scrape data on the future!")
  urls <- dbGetQuery(MLB, paste("SELECT url FROM all_games WHERE all_games.date >= ", first.date, "AND all_games.date <=", last.date, sep = ""))
  u <- urls[,"url"]
  p <- c("ax", "ay", "az", "break_angle", "break_length", "break_y", "cc", "des", 
                "end_speed", "id", "mt", "nasty", "on_1b", "on_2b", "on_3b", "pfx_x", "pfx_z", "pitch_type", 
                "px", "pz", "spin_dir", "spin_rate", "start_speed", "sv_id", "sz_bot", "sz_top", 
                "tfs", "tfs_zulu", "type", "type_confidence", "vx0", "vy0", "vz0", 
                "x", "x0", "y", "y0", "z0", "zone")
  ab <- c("away_team_runs", "b", "b_height", "batter", "des", "event", "event2", "event3", 
                "event4", "home_team_runs", "num", "o", "p_throws", "pitcher", "s", "score", "stand", "start_tfs", "start_tfs_zulu")
  data <- urlsToDataFrame(urls = u, p.tags = p, ab.tags = ab)
  #Should I add names to the atbats table before returning it?
  return(data) #Should I write the table to the wd? Would this help with memory issue? 
}
