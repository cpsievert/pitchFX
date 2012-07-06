########################################################################
#Functions that may not make the package (but useful for own purposes)
########################################################################

#Generate "miniscoreboard" urls for a given time period
getMiniScoreboards <- function(first.date, last.date) {
  start <- as.Date(first.date)
  end <- as.Date(last.date)
  diff <- as.numeric(end - start)
  dates <- start + c(0:diff) * days(1) #Create vector of dates from start date to end date
  years <- year(dates)
  mnths <- formatC(month(dates), width = 2, flag = "0")
  dys <- formatC(day(dates), width = 2, flag = "0")
  scoreboards <- as.list(paste("http://gd2.mlb.com/components/game/mlb/year_", years, 
                               "/month_", mnths, "/day_", dys, "/miniscoreboard.xml", sep = ""))
  return(scoreboards)
}

#This function returns every unique "tag" for a given set of urls and a specific node. 
#This is useful for finding a "masterlist" of field names for a table. 
#The field names should be extracted before the data is scraped

getFieldNames <- function(urls, node = "game") {
  all.tags <- NULL
  for (i in urls) {
    cat(i, "\n")
    doc <- try_default(xmlParse(i), NULL, quiet = TRUE)
    if (!is.null(doc)) {
      nodeset <- getNodeSet(doc, paste("//", node, sep = ""))
      if (length(nodeset) > 0) {
        info <- llply(nodeset, function(x) { xmlAttrs(x) })
        tags <- masterlist(info) #Depends on masterlist!
        all.tags <- c(all.tags, tags)
      }
    }  
  }
  unique(all.tags)
}
