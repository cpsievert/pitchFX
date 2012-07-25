#' Scrape Pitch F/X Data
#'
#' This function scrapes data from the MLB website.
#'
#' Details go here.
#' 
#' @param start date "yyyy-mm-dd" to commence scraping of pitch F/X data
#' @param end date "yyyy-mm-dd" to terminate scraping pitch F/X data
#' @param include.games whether or not to include a game table in the output
#' @param tables XML nodes to be parsed into a data frame
#' @return Returns a list containing two different data frames. The larger data frame contains data on every pitch thrown (pitch F/X). The smaller one contains data on every atbat.
#' @export
#' @examples
#' #ptm <- proc.time()
#' #data <- scrapePitchFX(start = "2011-05-01", end = "2011-05-01")
#' #ptm
#'
#' #games <- data$game
#' #atbats <- data$atbat
#' #pitches <- data$pitch
#' 
#' #pitchFX <- join(pitches, atbats, by = c("num", "url"))
#'
#' #unique(pitchFX$pitch_type)
#' #pitchFX <- pitchFX[pitchFX$pitch_type == c("SL","CU"), ]

scrapePitchFX <- function(start = "2012-01-01", end = Sys.Date(), include.games = T, tables = list(atbat = fields$atbat, pitch = fields$pitch)) {
  start <- as.POSIXct(start)
  end <- as.POSIXct(end)
  if (year(start) < 2005) stop("Not only is pitchFX data not avaliable before 2008, data on each game isn't complete until 2005")
  if (year(start) < 2008) warning("pitchFX data wasn't recorded consistently until 2008. Do you want to consider a later start date?")
  if (end > as.POSIXct(Sys.Date())) stop("Sorry, I can't scrape data on the future!")
  #Load data objects
  data(fields)
  data(games)
  #data(players)
  user.url <- getScoreboardURLs(start, end)
  url.diff  <- user.url[!is.element(user.url, games$url_scoreboard)] #Keep all user specified xurls that aren't already available
  some.games <- games[games$date >= start & games$date <= end,] #This is slow!!!
  #rm("games") Why doesn't this work?
  if(length(url.diff) > 0) some.games <- rbind(some.games, urlsToDataFrame(urls = url.diff, tables = list(game = fields$game)))
  pitchFX.urls <- some.games[, "url"]               
  data <- urlsToDataFrame(urls = pitchFX.urls, tables)
  data$game <- some.games
  return(data) #Should I write the table to the wd? Would this help with memory issue? 
}