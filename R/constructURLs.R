#'Generate miniscoreboard urls for a specified number of dates.
#'
#'This function will create a URL for each day between \code{start} and 
#'\code{end}. Not every URL will exist. For those that do, the file will contain
#'information on every game played during that day.
#'
#' @param start  date (yyyy-mm-dd) to commence scraping of pitch F/X data
#' @param end date (yyyy-mm-dd) to terminate scraping pitch F/X data
#' @export
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
#' It should not be used until one has a table of desired games.
#' It takes on a data object containing "id" attributes from the "game" node 
#' of "~/miniscoreboard.xml" files. Entries should have the following format: "yyyy/mm/dd/game-id"
#'
#' @param frame data frame with a column containing the "id" attribute from the "game" node.
#' @export
#' @return Returns a list of URLs that should contain all available pitch F/X info.

getPitchFxURLs <- function(frame) {
  split.id <- try(str_split(frame$id, "/"))
  if (!is.null(attr(split.id, "class"))) {
    warning("Neither 'date' nor '~/inning/inning_all.xml' URL fields were not created.")
  } else{
    names(frame) <- gsub("url", "url_scoreboard", names(frame))
    split.id <- str_split(frame$id, "/")
    frame$date <- unlist(llply(split.id, function(x) { 
      paste(x[1], x[2], x[3], sep = "/") 
    }))
    frame$url <- unlist(llply(split.id, function(x) {
      paste("http://gd2.mlb.com/components/game/mlb/year_", 
            x[1], "/month_", x[2], "/day_", x[3], "/gid_", 
            paste(x[1], x[2], x[3], gsub("-", "_", x[4]), sep = "_"),
            "/inning/inning_all.xml", sep = "")
    }))
  }
  return(frame)
}

# getPitchFxURLs <- function(urls) {
#  gids <- NULL
#  for (i in urls) {
#    if (!is.na(i)) {
#      plays <- try_default(xmlParse(i), NULL, quiet = TRUE)
#      if(!is.null(plays)){
#        games <- getNodeSet(plays, "//game")  
#        thisdayurl <- unlist(llply(games, function(x) {
#          gid <- xmlGetAttr(x,name="id")
#          gsub("miniscoreboard.xml", sprintf("gid_%s", gid), i)
#        }))
#        gids <- c(gids, thisdayurl)
#      }
#    }
#  }
#  return(as.list(paste(gids, "/inning/inning_all.xml", sep = "")))
# }

#' Generate Player URLs.
#'
#' This function will create a URL for each player in a specfic game. 
#' It is good practice to enter an 'atbat' table, but it is also acceptable to enter a 'game' table.
#' In either case, the data frame should have a 'url' column with a suffix of "~/inning/inning_all.xml".
#'
#' @param data Dataframe with the relevant "url" column.
#' @export
#' @return Returns a list of URLs for each player in the data set.

getPlayerURLs <- function(data) { #Scraping page source from every game branch
  if (any(names(data) == "batter") & any(names(data) == "pitcher") & any(names(data) == "url")) {
    unique.pitchers <- unique(data[,c("pitcher", "url")])
    pitchers.xml <- paste("pitchers/", unique.pitchers[, "pitcher"], ".xml", sep = "")
    pitcher.gids <- gsub("inning/inning_all.xml", "", unique.pitchers[,"url"])
    pitcher.urls <- paste(pitcher.gids, pitchers.xml, sep = "")
    unique.batters <- unique(data[,c("batter", "url")])
    batters.xml <- paste("batters/", unique.batters[, "batter"], ".xml", sep = "")
    batter.gids <- gsub("inning/inning_all.xml", "", unique.batters[,"url"])
    batter.urls <- paste(batter.gids, batters.xml, sep = "")
    return(as.list(c(pitcher.urls, batter.urls)))
  }
  if (any(names(data) == "url")) {
    branch <- gsub("inning/inning_all.xml", "", data$url)
    pitchers <- paste(branch, "pitchers", sep = "")
    batters <- paste(branch, "batters/", sep = "")
    everyone <- as.list(c(pitchers, batters))
    docs <- llply(everyone, htmlParse)
    values <- llply(docs, function(x) {
      xpathApply(x, "//a[@href]", xmlGetAttr, "href")
    })
    values <- llply(values, function(x) { x[-1] })
    urls <- llply(everyone, function(x) { 
      llply(values, function(y) {
        paste(x, "/", y, sep="")
      })
    })
    return(unlist(urls))
  } else {
    stop("Please enter a data frame with a 'url' column (with '~/inning/inning_all.xml' suffix). It is good practice to enter an 'atbat' table, but it is also acceptable to enter a 'game' table.")
  }
}
  