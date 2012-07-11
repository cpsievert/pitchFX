#'Generate miniscoreboard urls for a specified number of dates.
#'
#'This function will create a URL for each day between \code{start} and 
#'\code{end}. Not every URL will exist. For those that do, the file will contain
#'information on every game played during that day.
#'
#' @param start  date (yyyy-mm-dd) to commence scraping of pitch F/X data
#' @param end date (yyyy-mm-dd) to terminate scraping pitch F/X data
#' @return Returns a list of "miniscoreboard" URLs.

getScoreboardURLs <- function(start, end) {
  first.date <- as.Date(start)
  last.date <- as.Date(end)
  diff <- as.numeric(last.date - first.date)
  dates <- first.date + c(0:diff) * days(1) #Create vector of dates from start date to end date
  years <- year(dates)
  mnths <- formatC(month(dates), width = 2, flag = "0")
  dys <- formatC(day(dates), width = 2, flag = "0")
  scoreboards <- as.list(paste("http://gd2.mlb.com/components/game/mlb/year_", years, 
                               "/month_", mnths, "/day_", dys, "/miniscoreboard.xml", sep = ""))
  return(scoreboards)
}

#'Generate Pitch F/X URLs.
#'
#' This function will create the relevant URLs for each game.
#'
#' @param urls "miniscoreboard" urls used to construct the relevant URLs for each game.
#' @return Returns a list of URLs.

getPitchFxURLs <- function(urls) {
  gids <- NULL
  for (i in urls) {
    if (!is.na(i)) {
      plays <- try_default(xmlParse(i), NULL, quiet = TRUE)
      if(!is.null(plays)){
        games <- getNodeSet(plays, "//game")  
        thisdayurl <- unlist(llply(games, function(x) {
          gid <- xmlGetAttr(x,name="id")
          gsub("miniscoreboard.xml", sprintf("gid_%s", gid), i)
        }))
        gids <- c(gids, thisdayurl)
      }
    }
  }
  return(as.list(paste(gids, "/inning/inning_all.xml", sep = "")))
}


